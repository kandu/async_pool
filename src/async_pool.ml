open Core
open Async

module Arena = struct
  type 'a t= {
    elements: 'a Queue.t;
    waiters: 'a Ivar.t Queue.t;
    mutable count: int;

    dispose: 'a -> unit Deferred.t;
    mutable release: 'a t -> 'a -> unit Deferred.t
  }

  let release_normal t e=
    (match Queue.dequeue t.waiters with
    | Some waiter-> Ivar.fill waiter e
    | None-> Queue.enqueue t.elements e);
    Deferred.unit

  let release_clear t e=
    match Queue.dequeue t.waiters with
    | Some waiters-> Ivar.fill waiters e; Deferred.unit
    | None-> t.dispose e

  let create dispose= {
    elements= Queue.create ();
    waiters= Queue.create ();
    count= 0;
    dispose;
    release= release_normal;
  }
end

type 'a t= {
  create: unit -> 'a Deferred.t;
  validate:('a -> bool Deferred.t);
  check: 'a -> (bool -> unit Deferred.t) -> unit Deferred.t;

  capacity: int;

  interval: Core.Time.Span.t;

  mutable arena: 'a Arena.t;
}

let create capacity
  ?(validate= fun _-> return true)
  ?(check= fun _ f-> f true)
  ?(dispose= fun _ -> return ())
  ?(interval= sec 2.)
  create
  =
  {
    create;
    validate;
    check;

    capacity;

    interval;

    arena= Arena.create dispose
  }

let rec create_element t=
  let arena= t.arena in
  match%bind try_with (fun ()->
    (* inc count before any other threads *)
    arena.count <- arena.count + 1;
    t.create ())
  with
  | Ok e->
    if%bind t.validate e then
      return e
    else begin
      arena.dispose e >>= fun ()->
      arena.count <- arena.count - 1;
      after t.interval >>= fun ()->
      create_element t
    end
  | Error _->
    arena.count <- arena.count - 1;
    after t.interval >>= fun ()->
    create_element t

let acquire t=
  let arena= t.arena in
  match Queue.dequeue arena.elements with
  | Some element-> return element
  | None->
    if arena.count < t.capacity then
      create_element t
    else
      let ivar= Ivar.create () in
      Queue.enqueue arena.waiters ivar;
      Ivar.read ivar


let after_check t arena element= function
  | true->
    arena.Arena.release arena element
  | false->
    arena.Arena.dispose element >>= fun ()->
    arena.count <- arena.count - 1;
    let%bind element= create_element t in
    arena.Arena.release arena element

let use t f=
  let arena= t.arena in
  let%bind element= acquire t in
  match%bind try_with (fun ()-> f element) with
  | Ok r->
    arena.release arena element >>| fun ()-> r
  | Error exn->
    t.check element (after_check t arena element) >>= fun ()->
    raise exn


let clear t=
  t.arena.release <- Arena.release_clear;
  Queue.iter t.arena.elements ~f:(Fn.compose don't_wait_for t.arena.dispose);
  t.arena <- Arena.create t.arena.dispose

let wait_queue_length t= Queue.length t.arena.waiters


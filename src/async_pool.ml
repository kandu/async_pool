(*
 * async_pool.mli
 * -----------
 * Copyright : (c) 2022 - 2024, ZAN DoYe <zandoye@gmail.com>
 * Licence   : MIT
 *
 * This file is a part of async_pool.
 *)


open Core
open Async

module Arena = struct
  type action_result=
    | Timeout
    | Cancel

  type cancellable_action= {
    action: unit Deferred.t;
    cancel: unit Ivar.t;
  }

  let action_dummy ()=
    let action= Deferred.unit
    and cancel= Ivar.create_full () in
    {
      action;
      cancel;
    }


  let action_timeout time=
    let action= after time
    and cancel= Ivar.create () in
    {
      action;
      cancel;
    }

  let wait_action cancellable=
    choose [
      choice cancellable.action (fun ()-> Timeout);
      choice (Ivar.read cancellable.cancel) (fun ()-> Cancel);
      ]

  type 'a element= {
    resource: 'a;
    mutable alive: bool;
    mutable cancellable: cancellable_action;
  }

  type 'a t= {
    capacity: int;

    elements: 'a element Queue.t;
    waiters: 'a element Ivar.t Queue.t;

    mutable count: int;

    interval: Core.Time.Span.t;
    idle_limit: Core.Time.Span.t option;

    dispose: 'a -> unit Deferred.t;
    mutable release: 'a t -> 'a element-> unit Deferred.t;
  }

  let reset_element t element=
    Option.iter t.idle_limit ~f:(fun idle_limit->
      element.cancellable<- action_timeout idle_limit;
      don't_wait_for
        (match%bind wait_action element.cancellable with
        | Timeout->
          element.alive <- false;
          t.dispose element.resource
        | Cancel-> Deferred.unit))

  let use_element element=
    Ivar.fill_if_empty element.cancellable.cancel ()

  let new_element resource=
      let alive= true
      and cancellable= action_dummy () in
      let element= {
        resource;
        alive;
        cancellable;
      } in
      element

  let release_normal t e=
    (match Queue.dequeue t.waiters with
    | Some waiter-> Ivar.fill waiter e
    | None->
      reset_element t e;
      Queue.enqueue t.elements e);
    Deferred.unit

  let release_clear t e=
    match Queue.dequeue t.waiters with
    | Some waiters-> Ivar.fill waiters e; Deferred.unit
    | None-> t.dispose e.resource

  let create capacity dispose idle_limit interval= {
    elements= Queue.create ();
    waiters= Queue.create ();
    count= 0;
    capacity;
    dispose;
    release= release_normal;
    idle_limit;
    interval;
  }
end

type 'a t= {
  create: unit -> 'a Deferred.t;
  validate:('a -> bool Deferred.t);
  check: (bool -> unit Deferred.t) option;

  mutable arena: 'a Arena.t;
}

let create
  ?(validate= fun _-> return true)
  ?(dispose= fun _ -> return ())
  ?check
  ?(interval= sec 2.)
  ?idle_limit
  capacity
  create
  =
  {
    create;
    validate;
    check;

    arena= Arena.create capacity dispose idle_limit interval
  }

let rec create_element t=
  let arena= t.arena in
  match%bind try_with (fun ()->
    (* inc count before any other threads *)
    arena.count <- arena.count + 1;
    t.create ())
  with
  | Ok resource->
    if%bind t.validate resource then
      return (Arena.new_element resource)
    else begin
      arena.dispose resource >>= fun ()->
      arena.count <- arena.count - 1;
      after arena.interval >>= fun ()->
      create_element t
    end
  | Error _->
    arena.count <- arena.count - 1;
    after arena.interval >>= fun ()->
    create_element t

let rec acquire t=
  let arena= t.arena in
  match Queue.dequeue arena.elements with
  | Some element->
    if element.alive then
      (Arena.use_element element;
      return element)
    else
      (arena.count <- arena.count - 1;
      acquire t)
  | None->
    if arena.count < arena.capacity then
      create_element t
    else
      let ivar= Ivar.create () in
      Queue.enqueue arena.waiters ivar;
      let%bind element= Ivar.read ivar in
      Arena.use_element element;
      return element


let after_check t element normal=
  let arena= t.arena in
  match normal with
  | true->
    arena.Arena.release arena element
  | false->
    arena.Arena.dispose element.resource >>= fun ()->
    arena.count <- arena.count - 1;
    if arena.count < arena.capacity && not (Queue.is_empty arena.waiters) then
      let%bind element= create_element t in
      Arena.release_normal arena element
    else
      return ()

let use t f=
  let arena= t.arena in
  let%bind element= acquire t in
  match%bind try_with (fun ()-> f element.resource) with
  | Ok r->
    arena.release arena element >>| fun ()-> r
  | Error exn->
    let%bind normal= t.validate element.resource in
    let%bind ()= (match t.check with
      | Some check-> check normal
      | None-> return ())
    in
    after_check t element normal >>= fun ()->
    raise exn


let clear t=
  let arena= t.arena in
  arena.release <- Arena.release_clear;
  Queue.iter arena.elements ~f:(fun element-> don't_wait_for (arena.dispose element.resource));
  t.arena <- Arena.create arena.capacity arena.dispose arena.idle_limit arena.interval

let wait_queue_length t= Queue.length t.arena.waiters


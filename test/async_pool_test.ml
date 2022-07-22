open Core
open Async
open Async_unix

let pool= Async_pool.create 2 (fun ()-> return ())


let stdout= Lazy.force Writer.stdout

let p n=
  let c= ref 0 in
  let rec p n=
    c:= !c + 1;
    if !c > n * 4 then
      return ()
    else
      (Writer.writef stdout "%d\n" n;
      after (sec 1.) >>= fun ()->
      p n)
  in
  p n

let main ()=
  don't_wait_for @@ Async_pool.use pool (fun ()-> p 1);
  don't_wait_for @@ Async_pool.use pool (fun ()-> p 2);
  don't_wait_for @@ Async_pool.use pool (fun ()-> p 3);
  Async_pool.use pool (fun ()-> p 4)

let ()= Thread_safe.block_on_async_exn main


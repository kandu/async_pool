open Core
open Async

let%test_module _= (module struct

let counter= ref 0

let pool= Async_pool.create
  ~idle_limit:(Time.Span.of_sec 1.)
  3
  (fun ()->
    counter:= !counter+1;
    Writer.writef (Lazy.force Writer.stdout) "%d opened\n" !counter;
    return !counter)

let run v=
  let stdout= (Lazy.force Writer.stdout) in
  let%bind ()= Clock.after (sec 2.) in
  Writer.writef stdout "%d\n" v;
  Writer.flushed stdout

let new_thread hi f=
  Async_pool.use hi f

let main ()=
  don't_wait_for @@ new_thread pool run;
  don't_wait_for @@ new_thread pool run;
  Clock.after (sec 1.9) >>= fun ()->
  don't_wait_for @@ new_thread pool run; (* count is 2 and capacity is 3, so resource 3 is created here *)
  don't_wait_for @@ new_thread pool run; (* wait for 0.1 sec, and resource 1 is released, this sentence acquires the resource *)
  Clock.after (sec 2.2) >>= fun ()-> (* resources 1, 2, 3 are idle, but only resource 2 idles for more then 1 second, so is released *)
  don't_wait_for @@ new_thread pool run; (* resource 2 is disposed, so new resource 4 is created *)
  don't_wait_for @@ new_thread pool run;
  don't_wait_for @@ new_thread pool run;
  don't_wait_for @@ new_thread pool run;
  don't_wait_for @@ new_thread pool run;
  Clock.after (sec 6.)

let%expect_test "Async_pool"=
  Thread_safe.block_on_async_exn main;
  [%expect {|
    1 opened
    2 opened
    3 opened
    1
    2
    3
    1
    4 opened
    3
    1
    4
    3
    1 |}]

end)

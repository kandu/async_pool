(*
 * async_pool.mli
 * -----------
 * Copyright : (c) 2022 - 2024, ZAN DoYe <zandoye@gmail.com>
 * Licence   : MIT
 *
 * This file is a part of async_pool.
 *)


open Async

(**
   This module is a Lwt_pool clone built on top of async, provides an abstraction for managing collections of resources.
   *)

(** A pool containing elements of type 'a. *)
type 'a t

(** [create n ?check ?validate ?dispose ?interval f] creates a new pool with at most n elements. f is used to create a new pool element. Elements are created on demand and re-used until disposed of.

[validate] : is called each time a pool element is accessed by Async_pool.use, before the element is provided to Async_pool.use's callback. If validate element resolves to true the element is considered valid and is passed to the callback for use as-is. If validate element resolves to false the tested pool element is passed to dispose then dropped, with a new one is created to take element's place in the pool.

[dispose] : is used as described above and by Async_pool.clear to dispose of all elements in a pool. [dispose] is not guaranteed to be called on the elements in a pool when the pool is garbage collected. Async_pool.clear should be used if the elements of the pool need to be explicitly disposed of.

[check] : is called after the resolution of Async_pool.use's callback when the resolution is a failed promise. [check is_ok] must be called exactly once with true if element is still valid and false otherwise. If check calls is_ok false then dispose will be run on element and the element will not be returned to the pool.

[interval] : if resource creation fails or the new created resource failed in [validate], Async_pool will wait for [interval] time before retry creating the resource. This prevent it from draining too much system resource.

[idle_limit] : if present, all resources idle for [idle_limit] time will be disposed. Otherwise, the resources are permanent until some error occur.
*)
val create :
  ?validate:('a -> bool Deferred.t) ->
  ?dispose:('a -> unit Deferred.t) ->
  ?check: (bool -> unit Deferred.t) ->
  ?interval:Core.Time.Span.t ->
  ?idle_limit:Core.Time.Span.t ->
  int ->
  (unit -> 'a Deferred.t) ->
  'a t

(** [use p] f requests one free element of the pool [p] and gives it to the function [f]. The element is put back into the pool after the promise created by f completes. *)
val use : 'a t -> ('a -> 'b Deferred.t) -> 'b Deferred.t

(** [clear p] will clear all elements in [p], calling the [dispose] function associated with [p] on each of the cleared elements. Any elements from [p] which are currently in use will be disposed of once they are released.

The user functions waiting for resources will be satisfied. Actual disposing will be performed after the waiting user functions are finished.

However, the next call to [use p] after [clear p] guarantees a freshly created pool element, because the internal resource pool is renewed. *)
val clear : 'a t -> unit

(** [wait_queue_length p] returns the number of Async_pool.use requests currently waiting for an element of the pool p to become available. *)
val wait_queue_length : 'a t -> int


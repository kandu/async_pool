# async\_pool

This library mimics the [Lwt\_pool](https://ocsigen.org/lwt/latest/api/Lwt_pool) module of the [Lwt](https://ocsigen.org/lwt/) library. It brings a really good part of Lwt to the async world.

However, the APIs of the two are not identical.

The async version is simplified(the `check` callback of the `create` function, it reuses the `validate` callback, so users don't have to provide validate/check functions twice)

The functionality is also extended(idle resources can be cleared after user-specified time).


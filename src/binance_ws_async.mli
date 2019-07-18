open Core
open Async

open Binance_ws

val connect :
  ?buf:Bi_outbuf.t ->
  ?hb_ns:Time_stamp_counter.Calibrator.t * Int63.t ->
  Stream.t list ->
  (t Pipe.Reader.t * unit Deferred.t,
   [ `Internal of exn
   | `WS of Fastws_async.error ]) result Deferred.t

val connect_exn :
  ?buf:Bi_outbuf.t ->
  ?hb_ns:Time_stamp_counter.Calibrator.t * Int63.t ->
  Stream.t list ->
  (t Pipe.Reader.t * unit Deferred.t) Deferred.t

val with_connection :
  ?buf:Bi_outbuf.t ->
  ?hb_ns:Time_stamp_counter.Calibrator.t * Int63.t ->
  Stream.t list ->
  f:(t Pipe.Reader.t -> 'a Deferred.t) ->
  ('a,  [ `Internal of exn
        | `WS of Fastws_async.error
        | `Cleaned_up
        ]) result Deferred.t

val with_connection_exn :
  ?buf:Bi_outbuf.t ->
  ?hb_ns:Time_stamp_counter.Calibrator.t * Int63.t ->
  Stream.t list ->
  f:(t Pipe.Reader.t -> 'a Deferred.t) -> 'a Deferred.t

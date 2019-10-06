open Core
open Async

open Binance_ws

val connect :
  ?buf:Bi_outbuf.t ->
  ?hb_ns:Time_stamp_counter.Calibrator.t * Int63.t ->
  Stream.t list ->
  (t Pipe.Reader.t * unit Deferred.t) Deferred.Or_error.t

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
  'a Deferred.Or_error.t

val with_connection_exn :
  ?buf:Bi_outbuf.t ->
  ?hb_ns:Time_stamp_counter.Calibrator.t * Int63.t ->
  Stream.t list ->
  f:(t Pipe.Reader.t -> 'a Deferred.t) -> 'a Deferred.t

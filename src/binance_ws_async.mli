open Core
open Async

open Binance_ws

val connect :
  ?buf:Bi_outbuf.t ->
  ?hb_ns:Time_stamp_counter.Calibrator.t * Int63.t ->
  Stream.t list ->
  (event Pipe.Reader.t * unit Deferred.t) Deferred.t

val with_connection :
  ?buf:Bi_outbuf.t ->
  ?hb_ns:Time_stamp_counter.Calibrator.t * Int63.t ->
  Stream.t list ->
  f:(event Pipe.Reader.t -> 'a Deferred.t) -> 'a Deferred.t

open Core
open Async
open Binance

type topic =
  | Trade
  | Depth

type stream = {
  topic : topic ;
  symbol : string ;
}
val stream_of_string : string -> stream

type event =
  | Trade of Trade.t
  | Depth of Depth.t

val open_connection :
  ?buf:Bi_outbuf.t ->
  ?connected:unit Condition.t ->
  ?log:Log.t ->
  stream list ->
  event Pipe.Reader.t

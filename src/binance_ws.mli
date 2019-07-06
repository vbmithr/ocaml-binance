open Binance

type topic =
  | Trade
  | Depth

module Stream : sig
  type t = private {
    topic : topic ;
    symbol : string ;
  }
  val create : topic:topic -> symbol:string -> t
  val of_string : string -> t

  val to_path : t list -> string
end

type event =
  | Trade of Trade.t
  | Depth of Depth.t
[@@deriving sexp]

val pp_print_event : Format.formatter -> event -> unit

val event_encoding : event Json_encoding.encoding

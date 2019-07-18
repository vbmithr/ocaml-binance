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

type t =
  | Trade of Trade.t
  | Depth of Depth.t
[@@deriving sexp]

val pp : Format.formatter -> t -> unit
val encoding : t Json_encoding.encoding

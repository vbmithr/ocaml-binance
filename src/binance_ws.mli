open Binance

module Trade : sig
  type t = {
    event_ts : Ptime.t ;
    trade_ts : Ptime.t ;
    symbol : string ;
    side : [`Buy | `Sell] ;
    id : int64 ;
    buyer_order_id : int64 ;
    seller_order_id : int64 ;
    p : float ;
    q : float ;
  } [@@deriving sexp]

  val encoding : t Json_encoding.encoding
  val pp : t Fmt.t
  val to_string : t -> string
end

module Depth : sig
  type t = {
    event_ts : Ptime.t ;
    symbol : string ;
    first_update_id : int64 ;
    final_update_id : int64 ;
    bids : Level.t list ;
    asks : Level.t list ;
  } [@@deriving sexp]

  module Set : Set.S with type elt = t

  val encoding : t Json_encoding.encoding
  val pp : t Fmt.t
end

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

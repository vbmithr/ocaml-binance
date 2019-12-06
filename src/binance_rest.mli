open Fastrest
open Binance

module ExchangeInfo : sig
  val encoding : Sym.t list Json_encoding.encoding
  val get : (form, Sym.t list) service
end

module Depth : sig
  type t = {
    last_update_id : int64 ;
    bids : Level.t list ;
    asks : Level.t list ;
  } [@@deriving sexp]

  val get : ?limit:int -> string -> (form, t) service
end

module User : sig
  module Balance : sig
    type t = {
      asset : string ;
      free : float ;
      locked : float ;
    }

    val encoding : t Json_encoding.encoding
  end

  module AccountInfo : sig
    type t = {
      makerC : int ;
      takerC : int ;
      buyerC : int ;
      sellerC : int ;
      trade : bool ;
      withdraw : bool ;
      deposit : bool ;
      timestamp : Ptime.t ;
      balances : Balance.t list ;
      accountType : string ;
    }

    val encoding : t Json_encoding.encoding
    val pp : t Fmt.t
    val to_string : t -> string
  end

  module OrderStatus : sig
    type t = {
      symbol : string ;
      orderId : int ;
      clientOrderId : string ;
      price : float ;
      origQty : float ;
      executedQty : float ;
      ordStatus : OrderStatus.t ;
      timeInForce : TimeInForce.t ;
      ordType : OrderType.t ;
      side : Side.t ;
      stopPrice : float ;
      icebergQty : float ;
      time : Ptime.t ;
      isWorking : bool ;
    }

    val order_response_encoding : t Json_encoding.encoding
    val encoding : t Json_encoding.encoding
    val pp : t Fmt.t
    val to_string : t -> string
  end

  val open_orders : string ->
    (form, OrderStatus.t list) service

  val account_info : unit ->
    (form, AccountInfo.t) service

  val myTrades : string -> (form, unit list) service

  val order :
    ?dry_run:bool ->
    symbol:string ->
    side:Side.t ->
    kind:OrderType.t ->
    ?timeInForce:TimeInForce.t ->
    qty:float ->
    ?price:float ->
    ?clientOrdID:string ->
    ?stopPx:float ->
    ?icebergQty:float -> unit ->
    (form, OrderStatus.t option) service

  module Stream : sig
    val start : unit -> (form, string) service
    val renew : listenKey:string -> (form, unit) service
    val close : listenKey:string -> (form, unit) service
  end
end

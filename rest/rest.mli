open Core
open Async
open Binance

module BinanceError : sig
  type t = {
    code : int ;
    msg : string ;
  }

  val encoding : t Json_encoding.encoding
  val pp : t Fmt.t
  val to_string : t -> string
end

module Depth : sig
  type t = {
    last_update_id : int ;
    bids : Level.t list ;
    asks : Level.t list ;
  }

  val get :
    ?buf:Bi_outbuf.t -> ?log:Log.t -> ?limit:int ->
    string -> (Cohttp.Response.t * t) Or_error.t Deferred.t
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

  val open_orders :
    ?buf:Bi_outbuf.t -> ?log:Log.t ->
    key:string -> secret:string -> string ->
    (Cohttp.Response.t * OrderStatus.t list) Or_error.t Deferred.t

  val account_info :
    ?buf:Bi_outbuf.t -> ?log:Log.t ->
    key:string -> secret:string -> unit ->
    (Cohttp.Response.t * AccountInfo.t) Or_error.t Deferred.t

  val order :
    ?buf:Bi_outbuf.t ->
    ?log:Log.t ->
    ?dry_run:bool ->
    key:string ->
    secret:string ->
    symbol:string ->
    side:Side.t ->
    kind:OrderType.t ->
    ?timeInForce:TimeInForce.t ->
    qty:float ->
    ?price:float ->
    ?clientOrdID:string ->
    ?stopPx:float ->
    ?icebergQty:float -> unit ->
    (Cohttp.Response.t * OrderStatus.t option) Or_error.t Deferred.t

  module Stream : sig
    val start :
      ?buf:Bi_outbuf.t -> ?log:Log.t -> key:string -> unit ->
      (Cohttp.Response.t * string) Or_error.t Deferred.t
    val renew :
      ?buf:Bi_outbuf.t -> ?log:Log.t -> key:string -> string ->
      (Cohttp.Response.t * unit) Or_error.t Deferred.t
    val close :
      ?buf:Bi_outbuf.t -> ?log:Log.t -> key:string -> string ->
      (Cohttp.Response.t * unit) Or_error.t Deferred.t
  end
end

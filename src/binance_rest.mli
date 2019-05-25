open Core

open Fastrest
open Binance

module BinanceError : sig
  type t = {
    code : int ;
    msg : string ;
  }

  val encoding : t Json_encoding.encoding
  val or_error :
    'a Json_encoding.encoding -> ('a, t) result Json_encoding.encoding
  val pp : t Fmt.t
  val to_string : t -> string
end

module ExchangeInfo : sig
  module Sym : sig
    type t = {
      symbol: string ;
      base: string ;
      base_decimals: int ;
      quote: string ;
      quote_decimals: int ;
    } [@@deriving sexp]

    val compare : t -> t -> int
  end

  val encoding : Sym.t list Json_encoding.encoding
  val get : (get, Sym.t list, BinanceError.t) service
end

module Depth : sig
  type t = {
    last_update_id : int ;
    bids : Level.t list ;
    asks : Level.t list ;
  } [@@deriving sexp]

  val get : ?limit:int -> string -> (get, t, BinanceError.t) service
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
    (get, OrderStatus.t list, BinanceError.t) service

  val account_info : unit ->
    (get, AccountInfo.t, BinanceError.t) service

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
    (post_form, OrderStatus.t option, BinanceError.t) service

  module Stream : sig
    val start : unit -> (post_form, string, BinanceError.t) service
    val renew : listenKey:string -> (put_form, unit, BinanceError.t) service
    val close : listenKey:string -> (delete, unit, BinanceError.t) service
  end
end

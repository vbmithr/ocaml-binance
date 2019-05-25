module Side : sig
  type t = [`Buy | `Sell]
  val pp : t Fmt.t
  val to_string : t -> string
  val of_string : string -> t option
  val of_string_exn : string -> t
  val encoding : t Json_encoding.encoding
end

module TimeInForce : sig
  type t =
    | GoodTillCanceled
    | ImmediateOrCancel
    | FillOrKill

  val pp : t Fmt.t
  val to_string : t -> string
  val of_string : string -> t option
  val of_string_exn : string -> t
  val encoding : t Json_encoding.encoding
end

module OrderStatus : sig
  type t =
    | New
    | Partially_filled
    | Filled
    | Canceled
    | Pending_cancel
    | Rejected
    | Expired

  val pp : t Fmt.t
  val to_string : t -> string
  val of_string : string -> t option
  val of_string_exn : string -> t
  val encoding : t Json_encoding.encoding
end

module OrderType : sig
  type t =
    | Limit
    | Market
    | Stop_loss
    | Stop_loss_limit
    | Take_profit
    | Take_profit_limit
    | Limit_maker

  val pp : t Fmt.t
  val to_string : t -> string
  val of_string : string -> t option
  val of_string_exn : string -> t
  val encoding : t Json_encoding.encoding
end

open Base

val float_as_string : float Json_encoding.encoding

module Ptime : sig
  include module type of Ptime with type t = Ptime.t
  val float_encoding : t Json_encoding.encoding
end

module Yojson_repr : sig
  open Json_encoding
  val construct : 't encoding -> 't -> Json_repr.Yojson.value
  val destruct : 't encoding -> Json_repr.Yojson.value -> 't
  val destruct_safe : 't encoding -> Json_repr.Yojson.value -> 't
  val custom :
    ('t -> Json_repr.Yojson.value) ->
    (Json_repr.Yojson.value -> 't) -> schema:Json_schema.schema -> 't encoding
end

module Trade : sig
  type t = {
    event_ts : Ptime.t ;
    trade_ts : Ptime.t ;
    symbol : string ;
    tid : int ;
    p : float ;
    q : float ;
    buyer_order_id : int ;
    seller_order_id : int ;
    buyer_is_mm : bool ;
  } [@@deriving sexp]

  include Comparable.S with type t := t

  val encoding : t Json_encoding.encoding
  val pp : t Fmt.t
  val to_string : t -> string
end

module Level : sig
  type t = {
    p : float ;
    q : float ;
  } [@@deriving sexp]

  include Comparable.S with type t := t

  val encoding : t Json_encoding.encoding
end

module Depth : sig
  type t = {
    event_ts : Ptime.t ;
    symbol : string ;
    first_update_id : int ;
    final_update_id : int ;
    bids : Level.t list ;
    asks : Level.t list ;
  } [@@deriving sexp]

  include Comparable.S with type t := t

  val encoding : t Json_encoding.encoding
  val pp : t Fmt.t
  val to_string : t -> string
end

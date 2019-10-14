module Side : sig
  type t = Fixtypes.Side.t = Buy | Sell [@@deriving sexp]
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

val safe_float : float Json_encoding.encoding

module Ptime : sig
  include module type of Ptime with type t = Ptime.t

  val t_of_sexp : Sexplib.Sexp.t -> Ptime.t
  val sexp_of_t : Ptime.t -> Sexplib.Sexp.t
  val encoding : t Json_encoding.encoding
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

module Level : sig
  type t = {
    p : float ;
    q : float ;
  } [@@deriving sexp]

  val encoding : t Json_encoding.encoding
end

module Sym : sig
  type t = {
    symbol: string ;
    base: string ;
    base_decimals: int ;
    quote: string ;
    quote_decimals: int ;
  } [@@deriving sexp]

  val compare : t -> t -> int
  val encoding : t Json_encoding.encoding
end

open Base

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
  }

  include Comparable.S with type t := t

  val encoding : t Json_encoding.encoding
  val pp : t Fmt.t
  val to_string : t -> string
end

module Level : sig
  type t = {
    p : float ;
    q : float ;
  }

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
  }

  include Comparable.S with type t := t

  val encoding : t Json_encoding.encoding
  val pp : t Fmt.t
  val to_string : t -> string
end

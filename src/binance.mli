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

  val encoding : t Json_encoding.encoding
  val pp : t Fmt.t
  val to_string : t -> string
end

module Depth : sig
  type t = {
    event_ts : Ptime.t ;
    symbol : string ;
    first_update_id : int ;
    final_update_id : int ;
    bids : level list ;
    asks : level list ;
  }

  and level = {
    p : float ;
    q : float ;
  }

  val encoding : t Json_encoding.encoding
  val level_encoding : level Json_encoding.encoding
  val pp : t Fmt.t
  val to_string : t -> string
end

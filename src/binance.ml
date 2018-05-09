open Base

let ptime =
  let open Json_encoding in
  conv
    (fun i -> Ptime.to_float_s i *. 1e3)
    (fun i -> Option.value ~default:Ptime.epoch (Ptime.of_float_s (i /. 1e3)))
    float

module Trade = struct
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

  let encoding =
    let open Json_encoding in
    conv
      (fun { event_ts ; trade_ts ; symbol ; tid ; p ; q ;
             buyer_order_id ; seller_order_id ; buyer_is_mm } ->
        (), (event_ts, trade_ts, symbol, tid, p, q,
             buyer_order_id, seller_order_id, buyer_is_mm))
      (fun ((), (event_ts, trade_ts, symbol, tid, p, q,
                 buyer_order_id, seller_order_id, buyer_is_mm)) ->
        { event_ts ; trade_ts ; symbol ; tid ; p ; q ;
          buyer_order_id ; seller_order_id ; buyer_is_mm })
      (merge_objs unit (obj9
                          (req "E" ptime)
                          (req "T" ptime)
                          (req "s" string)
                          (req "t" int)
                          (req "p" float)
                          (req "q" float)
                          (req "b" int)
                          (req "a" int)
                          (req "m" bool)))
end

module Depth = struct
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

  let level =
    let open Json_encoding in
    conv
      (fun { p ; q } -> (Float.to_string p, Float.to_string q, ()))
      (fun (p, q, ()) -> { p = Float.of_string p ; q = Float.of_string q })
      (tup3 string string unit)

  let encoding =
    let open Json_encoding in
    conv
      (fun { event_ts ; symbol ; first_update_id ;
             final_update_id ; bids ; asks } ->
        ((), (event_ts, symbol, first_update_id, final_update_id, bids, asks)))
      (fun ((), (event_ts, symbol, first_update_id, final_update_id, bids, asks)) ->
         { event_ts ; symbol ; first_update_id ;
           final_update_id ; bids ; asks })
      (merge_objs unit (obj6
                          (req "E" ptime)
                          (req "s" string)
                          (req "U" int)
                          (req "u" int)
                          (req "b" (list level))
                          (req "a" (list level))
                       ))
end

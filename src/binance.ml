open Base

module Ptime = struct
  include Ptime

  let t_of_sexp sexp =
    Option.value_exn (Ptime.of_float_s (Sexplib.Std.float_of_sexp sexp))
  let sexp_of_t t =
    Sexplib.Std.sexp_of_float (Ptime.to_float_s t)

  let float_encoding =
    let open Json_encoding in
    conv
      (fun i -> Ptime.to_float_s i *. 1e3)
      (fun i -> Option.value ~default:Ptime.epoch (Ptime.of_float_s (i /. 1e3)))
      float
end

let float_as_string =
  let open Json_encoding in
  conv Float.to_string Float.of_string string

module Trade = struct
  module T = struct
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

    let compare { trade_ts = a } { trade_ts = b } =
      Ptime.compare a b
  end
  include T
  include Comparable.Make(T)

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
                          (req "E" Ptime.float_encoding)
                          (req "T" Ptime.float_encoding)
                          (req "s" string)
                          (req "t" int)
                          (req "p" float_as_string)
                          (req "q" float_as_string)
                          (req "b" int)
                          (req "a" int)
                          (req "m" bool)))

  let pp ppf t =
    Json_repr.(pp (module Ezjsonm) ppf (Json_encoding.construct encoding t))
  let to_string = Fmt.to_to_string pp
end

module Level = struct
  module T = struct
    type t = {
      p : float ;
      q : float ;
    } [@@deriving sexp]

    let compare { p = a } { p = b } =
      Float.compare a b
  end
  include T
  include Comparable.Make(T)

  let encoding =
    let open Json_encoding in
    conv
      (fun { p ; q } -> (Float.to_string p, Float.to_string q, ()))
      (fun (p, q, ()) -> { p = Float.of_string p ; q = Float.of_string q })
      (tup3 string string unit)
end

module Depth = struct
  module T = struct
    type t = {
      event_ts : Ptime.t ;
      symbol : string ;
      first_update_id : int ;
      final_update_id : int ;
      bids : Level.t list ;
      asks : Level.t list ;
    } [@@deriving sexp]

    let compare { final_update_id = a } { final_update_id = b } =
      Int.compare a b
  end
  include T
  include Comparable.Make(T)

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
                          (req "E" Ptime.float_encoding)
                          (req "s" string)
                          (req "U" int)
                          (req "u" int)
                          (req "b" (list Level.encoding))
                          (req "a" (list Level.encoding))))
  let pp ppf t =
    Json_repr.(pp (module Ezjsonm) ppf (Json_encoding.construct encoding t))
  let to_string = Fmt.to_to_string pp
end

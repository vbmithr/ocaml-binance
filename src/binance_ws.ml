open Binance

type topic =
  | Trade
  | Depth

let pp_topic ppf = function
  | Trade -> Format.pp_print_string ppf "trade"
  | Depth -> Format.pp_print_string ppf "depth"

let topic_of_string = function
  | "trade" -> Trade
  | "depth" -> Depth
  | _ -> invalid_arg "topic_of_string"

module Stream = struct
  type t = {
    topic : topic ;
    symbol : string ;
  }

  let create ~topic ~symbol =
    { topic ; symbol = String.lowercase_ascii symbol }

  let pp ppf { topic ; symbol } =
    Format.pp_print_string ppf symbol ;
    Format.pp_print_char ppf '@' ;
    pp_topic ppf topic

  let to_string = Fmt.to_to_string pp
  let of_string s =
    match String.split_on_char '@' s with
    | [symbol; topic] -> { topic = topic_of_string topic ; symbol }
    | _ -> invalid_arg "stream_of_string"

  let to_path streams =
    String.concat "/" (List.map to_string streams)
end

type event =
  | Trade of Trade.t
  | Depth of Depth.t
[@@deriving sexp]

let pp_print_event ppf e =
  Format.fprintf ppf "%a" Sexplib.Sexp.pp (sexp_of_event e)

let trade t = Trade t
let depth d = Depth d

let event_encoding =
  let open Json_encoding in
  union [
    case Trade.encoding (function Trade t -> Some t | _ -> None) trade ;
    case Depth.encoding (function Depth d -> Some d | _ -> None) depth ;
  ]

let event_encoding =
  let open Json_encoding in
  let to_json = function
      | Trade t -> (t.symbol ^ "@trade", Trade t)
      | Depth d -> (d.symbol ^ "@depth", Depth d) in
  let of_json (_, data) = data in
  conv to_json of_json
    (obj2
       (req "stream" string)
       (req "data" event_encoding))


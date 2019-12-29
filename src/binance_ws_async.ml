open Binance
open Binance_ws

let of_string ?buf msg = Yojson_repr.destruct_safe encoding (Yojson.Safe.from_string ?buf msg)

module Persistent = struct
  module Conn = Fastws_async.MakePersistent(struct type r = Binance_ws.t type w = unit end)
  include Persistent_connection_kernel.Make(Conn)

  let create' ~server_name ?on_event ?retry_delay ?buf ?hb =
    create ~server_name ?on_event ?retry_delay ~connect:begin fun url ->
      let rd = of_string ?buf in
      let wr _ = "" in
      Fastws_async.connect ?hb ~rd ~wr url
    end
end

open Binance
open Binance_ws

let of_string ?buf msg = Yojson_repr.destruct_safe encoding (Yojson.Safe.from_string ?buf msg)

module Conn = Fastws_async.MakePersistent(struct type r = Binance_ws.t type w = unit end)
module Persistent = Persistent_connection_kernel.Make(Conn)

open Core
open Async
open Log.Global
open Binance

let command =
  let spec =
    let open Command.Spec in
    empty
    +> flag "-loglevel" (optional int) ~doc:"1-3 loglevel"
    +> anon (sequence ("stream" %: string))
  in
  let set_loglevel = function
    | 2 -> set_level `Info
    | 3 -> set_level `Debug
    | _ -> ()
  in
  let run loglevel streams () =
    Option.iter loglevel ~f:set_loglevel;
    let evts = Ws.open_connection ~log:(Lazy.force log)
        (List.map ~f:Ws.stream_of_string streams) in
    Pipe.iter evts ~f:begin function
      | Trade t ->
        printf "T %s" (Trade.to_string t) ;
        Deferred.unit
      | Depth d ->
        printf "D %s" (Depth.to_string d) ;
        Deferred.unit
    end
  in
  Command.async_spec ~summary:"Binance terminal" spec run

let () = Command.run command

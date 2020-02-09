open Core
open Async
open Binance

open Binance_ws

let src = Logs.Src.create "binance.test.depth"
module Log_async = (val Logs_async.src_log src : Logs_async.LOG)

let drop_events_before depth id =
  let _before, after =
    Depth.Set.partition begin fun { Depth.last_update_id ; _ } ->
      Int64.(last_update_id <= id)
    end depth in
  after

let merge_diffs b a { Depth.bids ; Depth.asks ; _ } =
  let b = List.fold_left bids ~init:b ~f:begin fun acc { p ; q } ->
      Map.(if Float.equal q 0. then remove acc p else set acc ~key:p ~data:q)
    end in
  let a = List.fold_left asks ~init:a ~f:begin fun acc { p ; q } ->
      Map.(if Float.equal q 0. then remove acc p else set acc ~key:p ~data:q)
    end in
  b, a

type acc = {
  prev: Depth.t String.Map.t ;
  unprocessed: Depth.Set.t ;
  bids: float Float.Map.t ;
  asks: float Float.Map.t ;
}

let create_acc ?(prev=String.Map.empty) unprocessed bids asks =
  { prev ; unprocessed ; bids ; asks }

let init_acc = {
  prev = String.Map.empty ;
  unprocessed = Depth.Set.empty ;
  bids = Map.empty (module Float) ;
  asks = Map.empty (module Float) ;
}

let orderbook symbols init c =
  let streams = List.map symbols ~f:begin fun symbol ->
      Stream.create ~topic:Depth ~symbol
    end in
  let module Encoding = Json_encoding.Make(Json_repr.Yojson) in
  let buf = Bi_outbuf.create 4096 in
  let of_string s =
    Encoding.destruct encoding (Yojson.Safe.from_string ~buf s) in
  Fastws_async.connect ~of_string ~to_string:(fun _ -> assert false)
    (Binance_ws.url streams) >>= fun { r; w; _ } ->
  Pipe.close w ;
  Pipe.fold r ~init:init_acc
    ~f:begin fun ({ prev ; unprocessed ; bids ; asks } as acc) -> function
      | Trade _ -> Deferred.return acc
      | Depth ({ symbol; _ } as d) ->
        let symbol = String.lowercase symbol in
        let (_, w) = String.Table.find_exn c symbol in
        match Ivar.peek (String.Table.find_exn init symbol) with
        | None ->
          (* store events *)
          Pipe.write w (Some d, None, None) >>= fun () ->
          return (create_acc
                    ~prev:(String.Map.set prev ~key:symbol ~data:d)
                    (Depth.Set.add d unprocessed) bids asks)
        | Some (last_update_id, (_, _))
          when (not (Map.is_empty bids) || not (Map.is_empty asks)) ->
          (* already inited, add event if compliant *)
          let last_update_id =
            Option.value_map (String.Map.find prev symbol) ~default:last_update_id
              ~f:(fun { last_update_id ; _ } -> last_update_id) in
          if Int64.(d.Depth.first_update_id <> Int64.succ last_update_id) then
            failwith "orderbook: sequence problem, aborting" ;
          let b, a = merge_diffs bids asks d in
          Pipe.write w (Some d, Some b, Some a) >>= fun () ->
          return (create_acc
                    ~prev:(String.Map.set prev ~key:symbol ~data:d)
                    unprocessed b a)
        | Some (id, (bids, asks)) -> begin
            (* initialization phase *)
            let evts = drop_events_before (Depth.Set.add d unprocessed) id in
            match Depth.Set.min_elt_opt evts with
            | None ->
              (* No previous events received *)
              Pipe.write w (None, Some bids, Some asks) >>= fun () ->
              return (create_acc unprocessed bids asks)
            | Some { first_update_id ; last_update_id ; _ } ->
              (* Previous events received *)
              if Int64.(first_update_id > Int64.succ id ||
                        last_update_id < Int64.succ id) then
                failwithf "orderbook: inconsistent data received (%Ld %Ld %Ld)"
                  first_update_id last_update_id last_update_id () ;
              let prev_d, bids, asks =
                Depth.Set.fold
                  begin fun d (_prev_d, bids, asks) ->
                    let bids, asks = merge_diffs bids asks d in
                    Some d, bids, asks
                  end evts (None, bids, asks) in
              Pipe.write w (None, Some bids, Some asks) >>= fun () ->
              let prev =
                Option.map prev_d ~f:(fun data -> (String.Map.set prev ~key:symbol ~data)) in
              Deferred.return (create_acc ?prev unprocessed bids asks)
          end
    end

let load_books b a =
  let b = List.fold_left b
      ~init:(Map.empty (module Float)) ~f:begin fun acc { Level.p ; q } ->
      Map.set acc ~key:p ~data:q
    end in
  let a = List.fold_left a
      ~init:(Map.empty (module Float)) ~f:begin fun acc { Level.p ; q } ->
      Map.set acc ~key:p ~data:q
    end in
  b, a

let wait_n_events c_read n =
  let rec inner n =
    Log_async.app (fun m -> m "wait for %d events" n) >>= fun () ->
    if n > 0 then
      Pipe.read c_read >>= fun _ ->
      inner (pred n)
    else
      Deferred.unit
  in inner n

let init_orderbook limit symbol init c_read =
  let open Binance_rest in
  wait_n_events c_read 10 >>= fun () ->
  Fastrest.request (Depth.get ~limit symbol) >>= fun { Depth.last_update_id ; bids ; asks } ->
  let snapshot = last_update_id, load_books bids asks in
  Logs_async.app (fun m -> m "Got snapshot for %s" symbol) >>= fun () ->
  Ivar.fill init snapshot ;
  Pipe.iter c_read ~f:begin function
    | None, _bids, _asks ->
      Logs_async.app (fun m -> m "Order books initialized %s" symbol)
    | Some _, _, _ ->
      Logs_async.app (fun m -> m "Order books updated")
  end

let main symbols limit =
  let symbols = List.map symbols ~f:String.lowercase in
  let init = String.Table.create () in
  let pipes = String.Table.create () in
  List.iter symbols ~f:begin fun key ->
    String.Table.set init ~key ~data:(Ivar.create ())
  end ;
  List.iter symbols ~f:begin fun key ->
    String.Table.set pipes ~key ~data:(Pipe.create ())
  end ;
  (orderbook symbols init pipes >>> ignore) ;
  List.iter symbols ~f:begin fun s ->
    let i = String.Table.find_exn init s in
    let (r, _) = String.Table.find_exn pipes s in
    don't_wait_for (init_orderbook limit s i r)
  end ;
  Deferred.never ()

let command =
  Command.async ~summary:"Binance depth" begin
    let open Command.Let_syntax in
    [%map_open
      let symbols = anon (sequence ("symbol" %: string))
      and limit = flag_optional_with_default_doc
          "limit" int sexp_of_int ~default:100
          ~doc:"N number of book entries"
      and () = Logs_async_reporter.set_level_via_param [] in
      fun () ->
        Logs.set_reporter (Logs_async_reporter.reporter ()) ;
        main symbols limit
    ] end

let () = Command.run command

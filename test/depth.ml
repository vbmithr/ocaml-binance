open Core
open Async
open Binance

open Binance_ws

let src = Logs.Src.create "binance.test.depth"

let drop_events_before depth last_update_id =
  let _before, after =
    Set.partition_tf depth ~f:(fun { Depth.final_update_id ; _ } ->
        final_update_id <= last_update_id) in
  after

let merge_diffs b a { Depth.bids ; Depth.asks ; _ } =
  let b = List.fold_left bids ~init:b ~f:begin fun acc { p ; q } ->
      Map.(if q = 0. then remove acc p else set acc ~key:p ~data:q)
    end in
  let a = List.fold_left asks ~init:a ~f:begin fun acc { p ; q } ->
      Map.(if q = 0. then remove acc p else set acc ~key:p ~data:q)
    end in
  b, a

type acc = {
  prev: (string, Depth.t, Base.String.comparator_witness) Map.t ;
  unprocessed: (Depth.t, Depth.comparator_witness) Set.t ;
  bids: (float, float, Base.Float.comparator_witness) Map.t ;
  asks: (float, float, Base.Float.comparator_witness) Map.t ;
}

let create_acc ?(prev=String.Map.empty) unprocessed bids asks =
  { prev ; unprocessed ; bids ; asks }

let init_acc = {
  prev = String.Map.empty ;
  unprocessed = Set.empty (module Depth) ;
  bids = Map.empty (module Float) ;
  asks = Map.empty (module Float) ;
}

let orderbook symbols init c =
  let streams = List.map symbols ~f:begin fun symbol ->
      Stream.create ~topic:Depth ~symbol
    end in
  Binance_ws_async.connect streams >>= fun (evts, _cleaned_up) ->
  Pipe.fold evts ~init:init_acc
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
                    (Set.add unprocessed d) bids asks)
        | Some (last_update_id, (_, _))
          when (not (Map.is_empty bids) || not (Map.is_empty asks)) ->
          (* already inited, add event if compliant *)
          let last_update_id =
            Option.value_map (String.Map.find prev symbol) ~default:last_update_id
              ~f:(fun { final_update_id ; _ } -> final_update_id) in
          if d.Depth.first_update_id <> last_update_id + 1 then
            failwith "orderbook: sequence problem, aborting" ;
          let b, a = merge_diffs bids asks d in
          Pipe.write w (Some d, Some b, Some a) >>= fun () ->
          return (create_acc
                    ~prev:(String.Map.set prev ~key:symbol ~data:d)
                    unprocessed b a)
        | Some (last_update_id, (bids, asks)) -> begin
            (* initialization phase *)
            let evts = drop_events_before (Set.add unprocessed d) last_update_id in
            match Set.min_elt evts with
            | None ->
              (* No previous events received *)
              Pipe.write w (None, Some bids, Some asks) >>= fun () ->
              return (create_acc unprocessed bids asks)
            | Some { first_update_id ; final_update_id ; _ } ->
              (* Previous events received *)
              if first_update_id > last_update_id + 1 ||
                 final_update_id < last_update_id + 1 then
                failwithf "orderbook: inconsistent data received (%d %d %d)"
                  first_update_id final_update_id last_update_id () ;
              let prev_d, bids, asks =
                Set.fold evts
                  ~init:(None, bids, asks)
                  ~f:begin fun (_prev_d, bids, asks) d ->
                    let bids, asks = merge_diffs bids asks d in
                    Some d, bids, asks
                  end in
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
    Logs_async.app ~src (fun m -> m "wait for %d events" n) >>= fun () ->
    if n > 0 then
      Pipe.read c_read >>= fun _ ->
      inner (pred n)
    else
      Deferred.unit
  in inner n

let init_orderbook limit symbol init c_read =
  let open Binance_rest in
  wait_n_events c_read 10 >>= fun () ->
  Fastrest.request (Depth.get ~limit symbol) >>|
  Result.map ~f:begin fun { Depth.last_update_id ; bids ; asks } ->
    last_update_id, load_books bids asks
  end >>= function
  | Error err ->
    Logs_async.app ~src begin fun m ->
      m "%a" (Fastrest.pp_print_error Binance_rest.BinanceError.pp) err
    end >>= fun () ->
    failwith "Init orderbook failed"
  | Ok snapshot ->
    Logs_async.app ~src (fun m -> m "Got snapshot for %s" symbol) >>= fun () ->
    Ivar.fill init snapshot ;
    Pipe.iter c_read ~f:begin function
      | (None, _bids, _asks) ->
        Logs_async.app ~src (fun m -> m "Order books initialized %s" symbol)
      | (Some _, _, _) ->
        Logs_async.app ~src (fun m -> m "Order books updated")
    end

let main symbols limit =
  let init = String.Table.create () in
  let pipes = String.Table.create () in
  List.iter symbols ~f:begin fun key ->
    String.Table.set init ~key ~data:(Ivar.create ())
  end ;
  List.iter symbols ~f:begin fun key ->
    String.Table.set pipes ~key ~data:(Pipe.create ())
  end ;
  don't_wait_for (Deferred.ignore (orderbook symbols init pipes)) ;
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
      and () = Logs_async_reporter.set_level_via_param None in
      fun () ->
        Logs.set_reporter (Logs_async_reporter.reporter ()) ;
        main symbols limit
    ] end

let () = Command.run command

open Core
open Async
open Binance
open Log.Global

let drop_events_before depth last_update_id =
  let _before, after =
    Set.partition_tf depth ~f:(fun { Depth.final_update_id } ->
        final_update_id <= last_update_id) in
  after

let merge_diffs b a { Depth.bids ; Depth.asks } =
  let b = List.fold_left bids ~init:b ~f:begin fun acc { p ; q } ->
      Map.(if q = 0. then remove acc p else set acc p q)
    end in
  let a = List.fold_left asks ~init:a ~f:begin fun acc { p ; q } ->
      Map.(if q = 0. then remove acc p else set acc p q)
    end in
  b, a

let orderbook symbol init c =
  let evts = Ws.open_connection ~log:(Lazy.force log)
      Ws.[create_stream ~topic:Depth ~symbol] in
  Pipe.fold evts
    ~init:(
      None, (* previous event *)
      Set.empty (module Depth), (* events unprocessed *)
      Map.empty (module Float), (* bids *)
      Map.empty (module Float)) (* asks *)
    ~f:begin fun ((prev_d, s, b, a) as acc) -> function
    | Trade _ -> Deferred.return acc
    | Depth d ->
      match Ivar.peek init with
      | None ->
        (* store events *)
        Condition.broadcast c (Some d, None, None) ;
        Deferred.return (Some d, (Set.add s d), b, a)
      | Some (last_update_id, _, _) when (not (Map.is_empty b) || not (Map.is_empty b)) ->
        (* already inited, add event if compliant *)
        let last_update_id =
          Option.value_map prev_d ~default:last_update_id
            ~f:(fun { final_update_id } -> final_update_id) in
        if d.Depth.first_update_id <> last_update_id + 1 then
          failwith "orderbook: sequence problem, aborting" ;
        let b, a = merge_diffs b a d in
        Condition.broadcast c (Some d, Some b, Some a) ;
        Deferred.return (Some d, s, b, a)
      | Some (last_update_id, bids, asks) -> begin
          (* initialization phase *)
          let evts = drop_events_before (Set.add s d) last_update_id in
          match Set.min_elt evts with
          | None ->
            (* No previous events received *)
            Condition.broadcast c (None, Some bids, Some asks) ;
            Deferred.return (None, s, bids, asks)
          | Some { first_update_id; final_update_id } ->
            (* Previous events received *)
            if first_update_id > last_update_id + 1 ||
               final_update_id < last_update_id + 1 then
              failwithf "orderbook: inconsistent data received (%d %d %d)"
                first_update_id final_update_id last_update_id () ;
            let prev_d, bids, asks =
              Set.fold evts ~init:(None, bids, asks) ~f:begin fun (prev_d, bids, asks) d ->
                let bids, asks = merge_diffs bids asks d in
                Some d, bids, asks
              end in
            Condition.broadcast c (None, Some bids, Some asks) ;
            Deferred.return (prev_d, s, bids, asks)
        end
    end

let load_books b a =
  let b = List.fold_left b
      ~init:(Map.empty (module Float)) ~f:begin fun acc { Level.p ; q } ->
      Map.set acc p q
    end in
  let a = List.fold_left a
      ~init:(Map.empty (module Float)) ~f:begin fun acc { Level.p ; q } ->
      Map.set acc p q
    end in
  b, a

let init_orderbook symbol =
  Rest.Depth.get ~log:(Lazy.force log) ~limit:100 symbol >>|
  Result.map ~f:begin fun { Rest.Depth.last_update_id ; bids ; asks } ->
    let bids, asks = load_books bids asks in
    last_update_id, bids, asks
  end

let wait_n_events c n =
  let rec inner n =
    printf "%d" n ;
    if n > 0 then
      Condition.wait c >>= fun _ ->
      inner (pred n)
    else
      Deferred.unit
  in inner n

let main symbol =
  stage begin fun `Scheduler_started ->
    let init = Ivar.create () in
    let c = Condition.create () in
    don't_wait_for (Deferred.ignore (orderbook symbol init c)) ;
    wait_n_events c 10 >>= fun () ->
    begin
      init_orderbook symbol >>= function
      | Error err ->
        printf "%s" (Rest.BinanceError.to_string err) ;
        failwith "Init orderbook failed"
      | Ok snapshot ->
        printf "Got snapshot for %s" symbol ;
        Ivar.fill init snapshot ;
        let rec inner () =
          Condition.wait c >>= fun (d, bids, asks) ->
          begin if Option.is_none d then
              printf "Order books initialized %s" symbol
            else
              printf "Order books updated"
          end ;
          inner ()
        in inner ()
    end
  end

let command =
  Command.Staged.async ~summary:"Binance depth" begin
    let open Command.Let_syntax in
    [%map_open
      let symbol = anon ("symbol" %: string)
      and () = set_level_via_param () in
      fun () ->
        main symbol
    ] end

let () = Command.run command

type t = {
  book : Book.t;
  mutable next_id : int;
}

type snap = {
  symbol : string;
  best_bid : (float * float) option;
  best_ask : (float * float) option;
  mid : float option;
  spread : float option;
  micro_px : float option;
  imbalance : float option;
  ofi : float;
  vpin : float option;
  ewma_vol : float option;
  roll_spread : float option;
  active_orders : int;
  trades : int;
  volume : float;
}

let book ?capacity ?stp ?(vpin_bucket_vol = 100.0) ?vpin_buckets symbol =
  let bv = Fixed.of_f_exn vpin_bucket_vol in
  {
    book = Book.create ?capacity ?stp ~vpin_bucket_vol:bv ?vpin_buckets ~symbol ();
    next_id = 1;
  }

let raw e = e.book

let clear e =
  Book.clear e.book;
  e.next_id <- 1

let alloc_id e = function
  | Some id ->
    if id >= e.next_id then e.next_id <- id + 1;
    id
  | None ->
    let id = e.next_id in
    e.next_id <- id + 1;
    id

let submit_side side e ?id ?trader ?tif ?iceberg ~px ~qty () =
  let oid = alloc_id e id in
  let px_fx = Fixed.of_f_exn px in
  let qty_fx = Fixed.of_f_exn qty in
  let iceberg_peak = Option.map Fixed.of_f_exn iceberg in
  let rep =
    Book.add_limit e.book ~id:oid ?trader ~side ~px:px_fx ~qty:qty_fx ?tif ?iceberg_peak ()
  in
  (oid, rep)

let bid = submit_side Book.Bid
let ask = submit_side Book.Ask

let buy_mkt e ?id ?trader ~qty () =
  let oid = alloc_id e id in
  Book.add_market e.book ~id:oid ?trader ~side:Bid ~qty:(Fixed.of_f_exn qty) ()

let sell_mkt e ?id ?trader ~qty () =
  let oid = alloc_id e id in
  Book.add_market e.book ~id:oid ?trader ~side:Ask ~qty:(Fixed.of_f_exn qty) ()

let cancel e id =
  match Book.cancel e.book ~id with Ok _ -> true | Error `Not_found -> false

let amend e ~id ~px ~qty =
  match Book.amend e.book ~id ~new_px:(Fixed.of_f_exn px) ~new_qty:(Fixed.of_f_exn qty) () with
  | Ok _ -> true
  | Error `Not_found -> false

let snap e =
  let pair_f = Option.map (fun (p, q) -> (Fixed.to_f p, Fixed.to_f q)) in
  {
    symbol = Book.symbol e.book;
    best_bid = pair_f (Book.best_bid e.book);
    best_ask = pair_f (Book.best_ask e.book);
    mid = Option.map Fixed.to_f (Book.mid e.book);
    spread = Option.map Fixed.to_f (Book.spread e.book);
    micro_px = Option.map Fixed.to_f (Book.micro_px e.book);
    imbalance = Book.imbalance e.book;
    ofi = Stats.Ofi.decayed (Book.ofi e.book);
    vpin = Stats.Vpin.get (Book.vpin e.book);
    ewma_vol = Stats.Ewma.vol (Book.mid_ewma e.book);
    roll_spread = Stats.Roll.spread (Book.roll e.book);
    active_orders = Book.order_count e.book;
    trades = Book.trade_count e.book;
    volume = Fixed.to_f (Book.volume_traded e.book);
  }

let opt_f_json = function
  | None -> "null"
  | Some v -> Printf.sprintf "%.6f" v

let to_json ?(levels = 5) e =
  let s = snap e in
  let bids, asks = Book.depth e.book ~levels () in
  let fmt_lvl (l : Book.level_snap) =
    Printf.sprintf
      "{\"px\":%.4f,\"qty\":%.4f,\"hidden\":%.4f,\"orders\":%d}"
      (Fixed.to_f l.px)
      (Fixed.to_f l.disp_qty)
      (Fixed.to_f l.hid_qty)
      l.orders
  in
  let bids_s = String.concat "," (List.map fmt_lvl bids) in
  let asks_s = String.concat "," (List.map fmt_lvl asks) in
  Printf.sprintf
    "{\"symbol\":%S,\"mid\":%s,\"spread\":%s,\"micro_px\":%s,\"imbalance\":%s,\"ofi\":%.4f,\"vpin\":%s,\"ewma_vol\":%s,\"roll_spread\":%s,\"active_orders\":%d,\"trades\":%d,\"volume\":%.4f,\"bids\":[%s],\"asks\":[%s]}"
    s.symbol
    (opt_f_json s.mid)
    (opt_f_json s.spread)
    (opt_f_json s.micro_px)
    (opt_f_json s.imbalance)
    s.ofi
    (opt_f_json s.vpin)
    (opt_f_json s.ewma_vol)
    (opt_f_json s.roll_spread)
    s.active_orders
    s.trades
    s.volume
    bids_s
    asks_s

let render_ascii ?(levels = 5) e =
  let s = snap e in
  let bids, asks = Book.depth e.book ~levels () in
  let buf = Buffer.create 512 in
  let bprintf fmt = Printf.bprintf buf fmt in
  bprintf "==================================================================\n";
  bprintf " OBOOK L3 ENGINE  |  %-10s  |  Orders: %-5d  Trades: %-5d\n"
    s.symbol s.active_orders s.trades;
  bprintf "------------------------------------------------------------------\n";
  let fmt_opt = function None -> "   n/a   " | Some v -> Printf.sprintf "%9.4f" v in
  bprintf " Mid: %s  |  MicroPx: %s  |  Spread: %s\n"
    (fmt_opt s.mid) (fmt_opt s.micro_px) (fmt_opt s.spread);
  bprintf " OFI: %9.2f  |  VPIN:    %s  |  Vol(EW): %s\n"
    s.ofi (fmt_opt s.vpin) (fmt_opt s.ewma_vol);
  bprintf "------------------------------------------------------------------\n";
  bprintf "       BIDS (Qty @ Px)         |         ASKS (Px @ Qty)          \n";
  bprintf "-------------------------------+----------------------------------\n";
  let n_rows = Int.max (List.length bids) (List.length asks) in
  let b_arr = Array.of_list bids in
  let a_arr = Array.of_list asks in
  if n_rows = 0 then
    bprintf "          [ empty ]            |            [ empty ]             \n"
  else
    for i = 0 to n_rows - 1 do
      let left =
        if i < Array.length b_arr then
          let l = b_arr.(i) in
          Printf.sprintf "[%2d] %10.4f @ %-11.2f"
            l.orders (Fixed.to_f l.disp_qty) (Fixed.to_f l.px)
        else "                              "
      in
      let right =
        if i < Array.length a_arr then
          let l = a_arr.(i) in
          Printf.sprintf "%-11.2f @ %-10.4f [%2d]"
            (Fixed.to_f l.px) (Fixed.to_f l.disp_qty) l.orders
        else ""
      in
      bprintf " %-29s | %-32s\n" left right
    done;
  bprintf "==================================================================\n";
  Buffer.contents buf

let pp ?levels e = print_string (render_ascii ?levels e)

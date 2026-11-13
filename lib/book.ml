open Fixed.O

type side = Arena.side =
  | Bid
  | Ask

type tif =
  | GTC
  | IOC
  | FOK
  | Post_only

type stp_mode =
  | Stp_none
  | Cancel_newest
  | Cancel_oldest
  | Decrement_and_cancel

type status =
  | Resting
  | Partial_and_resting
  | Filled
  | Cancelled_ioc
  | Cancelled_stp
  | Rejected_fok
  | Rejected_post_only
  | Rejected_duplicate_id
  | Rejected_arena_full
  | Rejected_invalid_qty

type fill = {
  maker_id : int;
  taker_id : int;
  px : Fixed.t;
  qty : Fixed.t;
  maker_side : side;
  ts : int64;
}

type exec_report = {
  order_id : int;
  status : status;
  req_qty : Fixed.t;
  filled_qty : Fixed.t;
  resting_qty : Fixed.t;
  cancelled_qty : Fixed.t;
  fills : fill list;
}

type level_snap = {
  px : Fixed.t;
  disp_qty : Fixed.t;
  hid_qty : Fixed.t;
  orders : int;
}

type order_snap = {
  id : int;
  trader : int;
  side : side;
  px : Fixed.t;
  disp_qty : Fixed.t;
  hid_qty : Fixed.t;
  ts : int64;
}

let null = Arena.null

type t = {
  symbol : string;
  stp : stp_mode;
  arena : Arena.t;
  max_levels : int;
  mutable free_lvl_head : int;
  mutable best_bid_lvl : int;
  mutable best_ask_lvl : int;
  lvl_alive : bool array;
  lvl_side : side array;
  lvl_px : Fixed.t array;
  lvl_head : int array;
  lvl_tail : int array;
  lvl_disp_vol : Fixed.t array;
  lvl_hid_vol : Fixed.t array;
  lvl_count : int array;
  lvl_prev : int array;
  lvl_next : int array;
  mutable clock : int64;
  mutable trade_count : int;
  mutable volume_traded : Fixed.t;
  mid_stats : Stats.Welford.t;
  mid_ewma : Stats.Ewma.t;
  ofi : Stats.Ofi.t;
  vpin : Stats.Vpin.t;
  roll : Stats.Roll.t;
}

let create
    ?(capacity = 65536)
    ?(stp = Stp_none)
    ?(vpin_bucket_vol = Fixed.of_int_exn 100)
    ?(vpin_buckets = 50)
    ~symbol
    () =
  if capacity < 4 then invalid_arg "Book.create: capacity must be >= 4";
  let max_levels = capacity in
  let lvl_next = Array.init max_levels (fun i -> if i + 1 < max_levels then i + 1 else null) in
  {
    symbol;
    stp;
    arena = Arena.create ~capacity;
    max_levels;
    free_lvl_head = 0;
    best_bid_lvl = null;
    best_ask_lvl = null;
    lvl_alive = Array.make max_levels false;
    lvl_side = Array.make max_levels Bid;
    lvl_px = Array.make max_levels Fixed.zero;
    lvl_head = Array.make max_levels null;
    lvl_tail = Array.make max_levels null;
    lvl_disp_vol = Array.make max_levels Fixed.zero;
    lvl_hid_vol = Array.make max_levels Fixed.zero;
    lvl_count = Array.make max_levels 0;
    lvl_prev = Array.make max_levels null;
    lvl_next;
    clock = 0L;
    trade_count = 0;
    volume_traded = Fixed.zero;
    mid_stats = Stats.Welford.create ();
    mid_ewma = Stats.Ewma.of_span 20;
    ofi = Stats.Ofi.create ();
    vpin = Stats.Vpin.create ~bucket_vol:vpin_bucket_vol ~n_buckets:vpin_buckets;
    roll = Stats.Roll.create ();
  }

let clear b =
  Arena.clear b.arena;
  b.free_lvl_head <- 0;
  b.best_bid_lvl <- null;
  b.best_ask_lvl <- null;
  b.clock <- 0L;
  b.trade_count <- 0;
  b.volume_traded <- Fixed.zero;
  for i = 0 to b.max_levels - 1 do
    b.lvl_alive.(i) <- false;
    b.lvl_head.(i) <- null;
    b.lvl_tail.(i) <- null;
    b.lvl_disp_vol.(i) <- Fixed.zero;
    b.lvl_hid_vol.(i) <- Fixed.zero;
    b.lvl_count.(i) <- 0;
    b.lvl_prev.(i) <- null;
    b.lvl_next.(i) <- (if i + 1 < b.max_levels then i + 1 else null)
  done;
  Stats.Welford.reset b.mid_stats;
  Stats.Ewma.reset b.mid_ewma;
  Stats.Ofi.reset b.ofi;
  Stats.Vpin.reset b.vpin;
  Stats.Roll.reset b.roll

let symbol b = b.symbol
let order_count b = Arena.active_count b.arena
let trade_count b = b.trade_count
let volume_traded b = b.volume_traded
let mid_stats b = b.mid_stats
let mid_ewma b = b.mid_ewma
let ofi b = b.ofi
let vpin b = b.vpin
let roll b = b.roll

let next_ts b = function
  | Some t ->
    if t > b.clock then b.clock <- t;
    t
  | None ->
    b.clock <- Int64.succ b.clock;
    b.clock

let best_bid b =
  if b.best_bid_lvl = null then None
  else Some (b.lvl_px.(b.best_bid_lvl), b.lvl_disp_vol.(b.best_bid_lvl))

let best_ask b =
  if b.best_ask_lvl = null then None
  else Some (b.lvl_px.(b.best_ask_lvl), b.lvl_disp_vol.(b.best_ask_lvl))

let bbo b = (best_bid b, best_ask b)

let mid b =
  match best_bid b, best_ask b with
  | Some (bp, _), Some (ap, _) -> Some (Fixed.midpoint bp ap)
  | _ -> None

let spread b =
  match best_bid b, best_ask b with
  | Some (bp, _), Some (ap, _) -> Some (ap -~ bp)
  | _ -> None

let micro_px b =
  match best_bid b, best_ask b with
  | Some (bid_px, bid_sz), Some (ask_px, ask_sz) ->
    (match Fixed.wmid ~bid_px ~bid_sz ~ask_px ~ask_sz with
     | Ok mp -> Some mp
     | Error _ -> None)
  | _ -> None

let imbalance b =
  match best_bid b, best_ask b with
  | Some (_, bid_sz), Some (_, ask_sz) ->
    let qb = Fixed.to_f bid_sz and qa = Fixed.to_f ask_sz in
    let tot = qb +. qa in
    if tot <= 0.0 then None else Some ((qb -. qa) /. tot)
  | _ -> None

let record_bbo_telemetry b =
  let bid_px, bid_sz =
    match best_bid b with Some (p, q) -> (p, q) | None -> (Fixed.zero, Fixed.zero)
  in
  let ask_px, ask_sz =
    match best_ask b with Some (p, q) -> (p, q) | None -> (Fixed.zero, Fixed.zero)
  in
  if bid_px >~ Fixed.zero && ask_px >~ Fixed.zero then (
    let _ = Stats.Ofi.push_bbo b.ofi ~bid_px ~bid_sz ~ask_px ~ask_sz in
    let m = Fixed.to_f (Fixed.midpoint bid_px ask_px) in
    Stats.Welford.push b.mid_stats m;
    Stats.Ewma.push b.mid_ewma m
  )

let alloc_level b ~side ~px =
  let idx = b.free_lvl_head in
  if idx = null then failwith "Book.alloc_level: level pool exhausted";
  b.free_lvl_head <- b.lvl_next.(idx);
  b.lvl_alive.(idx) <- true;
  b.lvl_side.(idx) <- side;
  b.lvl_px.(idx) <- px;
  b.lvl_head.(idx) <- null;
  b.lvl_tail.(idx) <- null;
  b.lvl_disp_vol.(idx) <- Fixed.zero;
  b.lvl_hid_vol.(idx) <- Fixed.zero;
  b.lvl_count.(idx) <- 0;
  b.lvl_prev.(idx) <- null;
  b.lvl_next.(idx) <- null;
  idx

let free_level b lvl =
  let p = b.lvl_prev.(lvl) in
  let n = b.lvl_next.(lvl) in
  if p <> null then b.lvl_next.(p) <- n
  else (
    match b.lvl_side.(lvl) with
    | Bid -> b.best_bid_lvl <- n
    | Ask -> b.best_ask_lvl <- n
  );
  if n <> null then b.lvl_prev.(n) <- p;
  b.lvl_alive.(lvl) <- false;
  b.lvl_head.(lvl) <- null;
  b.lvl_tail.(lvl) <- null;
  b.lvl_disp_vol.(lvl) <- Fixed.zero;
  b.lvl_hid_vol.(lvl) <- Fixed.zero;
  b.lvl_count.(lvl) <- 0;
  b.lvl_prev.(lvl) <- null;
  b.lvl_next.(lvl) <- b.free_lvl_head;
  b.free_lvl_head <- lvl

let find_or_create_level b ~side ~px =
  let better a_px b_px =
    match side with
    | Bid -> a_px >~ b_px
    | Ask -> a_px <~ b_px
  in
  let head_lvl = match side with Bid -> b.best_bid_lvl | Ask -> b.best_ask_lvl in
  if head_lvl = null then (
    let lvl = alloc_level b ~side ~px in
    (match side with Bid -> b.best_bid_lvl <- lvl | Ask -> b.best_ask_lvl <- lvl);
    lvl
  ) else (
    let prev = ref null in
    let curr = ref head_lvl in
    while !curr <> null && better b.lvl_px.(!curr) px do
      prev := !curr;
      curr := b.lvl_next.(!curr)
    done;
    if !curr <> null && b.lvl_px.(!curr) =~ px then !curr
    else (
      let lvl = alloc_level b ~side ~px in
      b.lvl_prev.(lvl) <- !prev;
      b.lvl_next.(lvl) <- !curr;
      if !prev <> null then b.lvl_next.(!prev) <- lvl
      else (
        match side with Bid -> b.best_bid_lvl <- lvl | Ask -> b.best_ask_lvl <- lvl
      );
      if !curr <> null then b.lvl_prev.(!curr) <- lvl;
      lvl
    )
  )

let enqueue_slot b lvl slot =
  Arena.set_level_idx b.arena slot lvl;
  let old_tail = b.lvl_tail.(lvl) in
  Arena.set_prev b.arena slot old_tail;
  Arena.set_next b.arena slot null;
  if old_tail <> null then Arena.set_next b.arena old_tail slot
  else b.lvl_head.(lvl) <- slot;
  b.lvl_tail.(lvl) <- slot;
  b.lvl_disp_vol.(lvl) <- b.lvl_disp_vol.(lvl) +~ Arena.disp_qty b.arena slot;
  b.lvl_hid_vol.(lvl) <- b.lvl_hid_vol.(lvl) +~ Arena.hid_qty b.arena slot;
  b.lvl_count.(lvl) <- b.lvl_count.(lvl) + 1

let unlink_slot_and_free b slot =
  let lvl = Arena.level_idx b.arena slot in
  let dq = Arena.disp_qty b.arena slot in
  let hq = Arena.hid_qty b.arena slot in
  b.lvl_disp_vol.(lvl) <- b.lvl_disp_vol.(lvl) -~ dq;
  b.lvl_hid_vol.(lvl) <- b.lvl_hid_vol.(lvl) -~ hq;
  b.lvl_count.(lvl) <- b.lvl_count.(lvl) - 1;
  let p = Arena.prev b.arena slot in
  let n = Arena.next b.arena slot in
  if p <> null then Arena.set_next b.arena p n else b.lvl_head.(lvl) <- n;
  if n <> null then Arena.set_prev b.arena n p else b.lvl_tail.(lvl) <- p;
  Arena.free b.arena slot;
  if b.lvl_count.(lvl) = 0 then free_level b lvl

let crosses ~taker_side ~limit_px ~maker_px =
  match taker_side with
  | Bid -> limit_px >=~ maker_px
  | Ask -> limit_px <=~ maker_px

(* Look before you leap: mutating the arena on an FOK and trying to roll back on failure
   is how people end up testifying in front of regulators. Pre-scan in read-only mode. *)
let can_fill_fok b ~side ~trader ~limit_px ~req_qty =
  let lvl = ref (match side with Bid -> b.best_ask_lvl | Ask -> b.best_bid_lvl) in
  let avail = ref Fixed.zero in
  let blocked_by_stp = ref false in
  while !lvl <> null && not !blocked_by_stp && !avail <~ req_qty && crosses ~taker_side:side ~limit_px ~maker_px:b.lvl_px.(!lvl) do
    let slot = ref b.lvl_head.(!lvl) in
    while !slot <> null && not !blocked_by_stp && !avail <~ req_qty do
      let same_trader = b.stp <> Stp_none && Arena.trader b.arena !slot = trader in
      if not same_trader then
        avail := !avail +~ Arena.total_qty b.arena !slot
      else (
        match b.stp with
        | Stp_none -> avail := !avail +~ Arena.total_qty b.arena !slot
        | Cancel_newest | Decrement_and_cancel ->
          blocked_by_stp := true
        | Cancel_oldest ->
          () (* Oldest gets nuked during matching, aggressive order keeps cruising *)
      );
      slot := Arena.next b.arena !slot
    done;
    lvl := b.lvl_next.(!lvl)
  done;
  not !blocked_by_stp && !avail >=~ req_qty

let would_cross_immediately b ~side ~limit_px =
  match side with
  | Bid ->
    b.best_ask_lvl <> null && limit_px >=~ b.lvl_px.(b.best_ask_lvl)
  | Ask ->
    b.best_bid_lvl <> null && limit_px <=~ b.lvl_px.(b.best_bid_lvl)

(* Iceberg tip got smoked. Refill display slice from hidden reserve and exile
   the node to the tail of this price level's FIFO queue per CME/NASDAQ rules. *)
let replenish_iceberg_to_tail b lvl slot ts =
  let hid = Arena.hid_qty b.arena slot in
  let peak = Arena.peak_qty b.arena slot in
  let next_disp = Fixed.min peak hid in
  let next_hid = hid -~ next_disp in
  Arena.set_disp_qty b.arena slot next_disp;
  Arena.set_hid_qty b.arena slot next_hid;
  Arena.set_ts b.arena slot ts;
  b.lvl_disp_vol.(lvl) <- b.lvl_disp_vol.(lvl) +~ next_disp;
  b.lvl_hid_vol.(lvl) <- b.lvl_hid_vol.(lvl) -~ next_disp;
  if b.lvl_head.(lvl) <> b.lvl_tail.(lvl) then (
    let n = Arena.next b.arena slot in
    let p = Arena.prev b.arena slot in
    if p <> null then Arena.set_next b.arena p n else b.lvl_head.(lvl) <- n;
    if n <> null then Arena.set_prev b.arena n p else b.lvl_tail.(lvl) <- p;
    let old_tail = b.lvl_tail.(lvl) in
    Arena.set_next b.arena old_tail slot;
    Arena.set_prev b.arena slot old_tail;
    Arena.set_next b.arena slot null;
    b.lvl_tail.(lvl) <- slot
  )

let match_incoming b ~id ~trader ~side ~limit_px ~req_qty ~ts =
  let rem = ref req_qty in
  let filled = ref Fixed.zero in
  let stp_cancelled = ref Fixed.zero in
  let fills_rev = ref [] in
  let stp_halt = ref false in
  let opp_head () = match side with Bid -> b.best_ask_lvl | Ask -> b.best_bid_lvl in
  while !rem >~ Fixed.zero && not !stp_halt && opp_head () <> null && crosses ~taker_side:side ~limit_px ~maker_px:b.lvl_px.(opp_head ()) do
    let lvl = opp_head () in
    let slot = b.lvl_head.(lvl) in
    let same_trader = b.stp <> Stp_none && Arena.trader b.arena slot = trader in
    if same_trader then (
      match b.stp with
      | Stp_none -> assert false
      | Cancel_newest ->
        stp_cancelled := !rem;
        rem := Fixed.zero;
        stp_halt := true
      | Cancel_oldest ->
        unlink_slot_and_free b slot
      | Decrement_and_cancel ->
        let maker_tot = Arena.total_qty b.arena slot in
        if maker_tot <=~ !rem then (
          stp_cancelled := !stp_cancelled +~ maker_tot;
          rem := !rem -~ maker_tot;
          unlink_slot_and_free b slot;
          if !rem =~ Fixed.zero then stp_halt := true
        ) else (
          (* Maker is larger than remaining taker: reduce maker in-place and cancel taker *)
          let cut = !rem in
          let dq = Arena.disp_qty b.arena slot in
          if cut <~ dq then (
            Arena.set_disp_qty b.arena slot (dq -~ cut);
            b.lvl_disp_vol.(lvl) <- b.lvl_disp_vol.(lvl) -~ cut
          ) else (
            let from_hid = cut -~ dq in
            let new_hid = Arena.hid_qty b.arena slot -~ from_hid in
            let peak = Arena.peak_qty b.arena slot in
            let next_disp = Fixed.min peak new_hid in
            let rem_hid = new_hid -~ next_disp in
            b.lvl_disp_vol.(lvl) <- b.lvl_disp_vol.(lvl) -~ dq +~ next_disp;
            b.lvl_hid_vol.(lvl) <- b.lvl_hid_vol.(lvl) -~ from_hid -~ next_disp;
            Arena.set_disp_qty b.arena slot next_disp;
            Arena.set_hid_qty b.arena slot rem_hid
          );
          stp_cancelled := !stp_cancelled +~ cut;
          rem := Fixed.zero;
          stp_halt := true
        )
    ) else (
      let maker_disp = Arena.disp_qty b.arena slot in
      let exec_qty = Fixed.min !rem maker_disp in
      let exec_px = b.lvl_px.(lvl) in
      let maker_id = Arena.id b.arena slot in
      let maker_side = Arena.side b.arena slot in
      rem := !rem -~ exec_qty;
      filled := !filled +~ exec_qty;
      b.trade_count <- b.trade_count + 1;
      b.volume_traded <- b.volume_traded +~ exec_qty;
      Stats.Vpin.push_trade b.vpin ~is_buy:(side = Bid) ~qty:exec_qty;
      Stats.Roll.push_px b.roll (Fixed.to_f exec_px);
      fills_rev :=
        { maker_id; taker_id = id; px = exec_px; qty = exec_qty; maker_side; ts }
        :: !fills_rev;
      let rem_disp = maker_disp -~ exec_qty in
      b.lvl_disp_vol.(lvl) <- b.lvl_disp_vol.(lvl) -~ exec_qty;
      Arena.set_disp_qty b.arena slot rem_disp;
      if rem_disp =~ Fixed.zero then (
        if Arena.hid_qty b.arena slot >~ Fixed.zero then
          replenish_iceberg_to_tail b lvl slot ts
        else
          unlink_slot_and_free b slot
      )
    )
  done;
  (!filled, !rem, !stp_cancelled, List.rev !fills_rev)

let add_limit
    b
    ~id
    ?(trader = 0)
    ~side
    ~px
    ~qty
    ?(tif = GTC)
    ?iceberg_peak
    ?ts
    () =
  let ts = next_ts b ts in
  if qty <=~ Fixed.zero || px <=~ Fixed.zero then
    {
      order_id = id;
      status = Rejected_invalid_qty;
      req_qty = qty;
      filled_qty = Fixed.zero;
      resting_qty = Fixed.zero;
      cancelled_qty = Fixed.max Fixed.zero qty;
      fills = [];
    }
  else if Arena.find_slot b.arena id <> null then
    {
      order_id = id;
      status = Rejected_duplicate_id;
      req_qty = qty;
      filled_qty = Fixed.zero;
      resting_qty = Fixed.zero;
      cancelled_qty = qty;
      fills = [];
    }
  else if tif = Post_only && would_cross_immediately b ~side ~limit_px:px then
    {
      order_id = id;
      status = Rejected_post_only;
      req_qty = qty;
      filled_qty = Fixed.zero;
      resting_qty = Fixed.zero;
      cancelled_qty = qty;
      fills = [];
    }
  else if tif = FOK && not (can_fill_fok b ~side ~trader ~limit_px:px ~req_qty:qty) then
    {
      order_id = id;
      status = Rejected_fok;
      req_qty = qty;
      filled_qty = Fixed.zero;
      resting_qty = Fixed.zero;
      cancelled_qty = qty;
      fills = [];
    }
  else if Arena.active_count b.arena >= Arena.capacity b.arena && tif <> IOC && tif <> FOK then
    {
      order_id = id;
      status = Rejected_arena_full;
      req_qty = qty;
      filled_qty = Fixed.zero;
      resting_qty = Fixed.zero;
      cancelled_qty = qty;
      fills = [];
    }
  else (
    let filled_qty, rem_qty, stp_canc, fills =
      match_incoming b ~id ~trader ~side ~limit_px:px ~req_qty:qty ~ts
    in
    if rem_qty =~ Fixed.zero then (
      record_bbo_telemetry b;
      let status = if stp_canc >~ Fixed.zero then Cancelled_stp else Filled in
      {
        order_id = id;
        status;
        req_qty = qty;
        filled_qty;
        resting_qty = Fixed.zero;
        cancelled_qty = stp_canc;
        fills;
      }
    ) else if tif = IOC then (
      record_bbo_telemetry b;
      {
        order_id = id;
        status = Cancelled_ioc;
        req_qty = qty;
        filled_qty;
        resting_qty = Fixed.zero;
        cancelled_qty = rem_qty +~ stp_canc;
        fills;
      }
    ) else (
      let peak =
        match iceberg_peak with
        | Some p when p >~ Fixed.zero && p <~ rem_qty -> p
        | _ -> rem_qty
      in
      let disp_qty = Fixed.min peak rem_qty in
      let hid_qty = rem_qty -~ disp_qty in
      match Arena.alloc b.arena ~id ~trader ~side ~px ~disp_qty ~hid_qty ~peak_qty:peak ~ts with
      | Error `Arena_full ->
        record_bbo_telemetry b;
        {
          order_id = id;
          status = Rejected_arena_full;
          req_qty = qty;
          filled_qty;
          resting_qty = Fixed.zero;
          cancelled_qty = rem_qty +~ stp_canc;
          fills;
        }
      | Error `Duplicate_id -> assert false
      | Ok slot ->
        let lvl = find_or_create_level b ~side ~px in
        enqueue_slot b lvl slot;
        record_bbo_telemetry b;
        let status = if filled_qty >~ Fixed.zero then Partial_and_resting else Resting in
        {
          order_id = id;
          status;
          req_qty = qty;
          filled_qty;
          resting_qty = rem_qty;
          cancelled_qty = stp_canc;
          fills;
        }
    )
  )

let add_market b ~id ?(trader = 0) ~side ~qty ?ts () =
  let limit_px = match side with Bid -> Fixed.max_val | Ask -> Fixed.of_raw 1L in
  add_limit b ~id ~trader ~side ~px:limit_px ~qty ~tif:IOC ?ts ()

let cancel b ~id =
  let slot = Arena.find_slot b.arena id in
  if slot = null then Error `Not_found
  else (
    let cancelled = Arena.total_qty b.arena slot in
    unlink_slot_and_free b slot;
    record_bbo_telemetry b;
    Ok cancelled
  )

(* Exchange priority law: shrinking size at the same price keeps your spot in line.
   Greedy size increases or price changes send you to the back of the bus. *)
let amend b ~id ~new_px ~new_qty ?ts () =
  let slot = Arena.find_slot b.arena id in
  if slot = null then Error `Not_found
  else (
    let ts = next_ts b ts in
    let old_px = Arena.px b.arena slot in
    let old_tot = Arena.total_qty b.arena slot in
    let trader = Arena.trader b.arena slot in
    let side = Arena.side b.arena slot in
    let peak = Arena.peak_qty b.arena slot in
    if new_qty <=~ Fixed.zero then (
      unlink_slot_and_free b slot;
      record_bbo_telemetry b;
      Ok
        {
          order_id = id;
          status = Cancelled_ioc;
          req_qty = old_tot;
          filled_qty = Fixed.zero;
          resting_qty = Fixed.zero;
          cancelled_qty = old_tot;
          fills = [];
        }
    ) else if new_px =~ old_px && new_qty <=~ old_tot then (
      let lvl = Arena.level_idx b.arena slot in
      let old_disp = Arena.disp_qty b.arena slot in
      let old_hid = Arena.hid_qty b.arena slot in
      let new_disp = Fixed.min old_disp new_qty in
      let new_hid = new_qty -~ new_disp in
      b.lvl_disp_vol.(lvl) <- b.lvl_disp_vol.(lvl) -~ old_disp +~ new_disp;
      b.lvl_hid_vol.(lvl) <- b.lvl_hid_vol.(lvl) -~ old_hid +~ new_hid;
      Arena.set_disp_qty b.arena slot new_disp;
      Arena.set_hid_qty b.arena slot new_hid;
      record_bbo_telemetry b;
      Ok
        {
          order_id = id;
          status = Resting;
          req_qty = new_qty;
          filled_qty = Fixed.zero;
          resting_qty = new_qty;
          cancelled_qty = Fixed.zero;
          fills = [];
        }
    ) else (
      unlink_slot_and_free b slot;
      let iceberg_peak = if peak <~ old_tot then Some peak else None in
      Ok (add_limit b ~id ~trader ~side ~px:new_px ~qty:new_qty ~tif:GTC ?iceberg_peak ~ts ())
    )
  )

let depth b ?(levels = 10) () =
  let collect start_lvl =
    let acc = ref [] in
    let curr = ref start_lvl in
    let n = ref 0 in
    while !curr <> null && !n < levels do
      let l = !curr in
      acc :=
        {
          px = b.lvl_px.(l);
          disp_qty = b.lvl_disp_vol.(l);
          hid_qty = b.lvl_hid_vol.(l);
          orders = b.lvl_count.(l);
        }
        :: !acc;
      curr := b.lvl_next.(l);
      incr n
    done;
    List.rev !acc
  in
  (collect b.best_bid_lvl, collect b.best_ask_lvl)

let l3_orders b ~side ~px =
  let curr = ref (match side with Bid -> b.best_bid_lvl | Ask -> b.best_ask_lvl) in
  let found_lvl = ref null in
  while !curr <> null && !found_lvl = null do
    if b.lvl_px.(!curr) =~ px then found_lvl := !curr;
    curr := b.lvl_next.(!curr)
  done;
  if !found_lvl = null then []
  else (
    let acc = ref [] in
    let s = ref b.lvl_head.(!found_lvl) in
    while !s <> null do
      let idx = !s in
      acc :=
        {
          id = Arena.id b.arena idx;
          trader = Arena.trader b.arena idx;
          side = Arena.side b.arena idx;
          px = Arena.px b.arena idx;
          disp_qty = Arena.disp_qty b.arena idx;
          hid_qty = Arena.hid_qty b.arena idx;
          ts = Arena.ts b.arena idx;
        }
        :: !acc;
      s := Arena.next b.arena idx
    done;
    List.rev !acc
  )

let find_order b ~id =
  let idx = Arena.find_slot b.arena id in
  if idx = null then None
  else
    Some
      {
        id = Arena.id b.arena idx;
        trader = Arena.trader b.arena idx;
        side = Arena.side b.arena idx;
        px = Arena.px b.arena idx;
        disp_qty = Arena.disp_qty b.arena idx;
        hid_qty = Arena.hid_qty b.arena idx;
        ts = Arena.ts b.arena idx;
      }

let verify_report_conservation (r : exec_report) =
  match Fixed.add r.filled_qty r.resting_qty with
  | Error _ -> false
  | Ok s1 ->
    (match Fixed.add s1 r.cancelled_qty with
     | Error _ -> false
     | Ok total ->
       let fills_sum =
         List.fold_left (fun acc (f : fill) -> acc +~ f.qty) Fixed.zero r.fills
       in
       total =~ r.req_qty && fills_sum =~ r.filled_qty)

let audit b =
  let exception Invariant_broken of string in
  let fail msg = raise (Invariant_broken msg) in
  try
    (match best_bid b, best_ask b with
     | Some (bp, _), Some (ap, _) when bp >=~ ap ->
       fail (Printf.sprintf "Crossed book: best_bid %s >= best_ask %s" (Fixed.to_str bp) (Fixed.to_str ap))
     | _ -> ());
    let total_orders_seen = ref 0 in
    let check_side side start_lvl =
      let prev_lvl = ref null in
      let curr = ref start_lvl in
      while !curr <> null do
        let l = !curr in
        if not b.lvl_alive.(l) then fail "Dead level in active chain";
        if b.lvl_side.(l) <> side then fail "Level side mismatch";
        if b.lvl_prev.(l) <> !prev_lvl then fail "Level prev pointer broken";
        if !prev_lvl <> null then (
          let prev_px = b.lvl_px.(!prev_lvl) in
          let cur_px = b.lvl_px.(l) in
          match side with
          | Bid -> if not (prev_px >~ cur_px) then fail "Bid levels not strictly descending"
          | Ask -> if not (prev_px <~ cur_px) then fail "Ask levels not strictly ascending"
        );
        let sum_disp = ref Fixed.zero in
        let sum_hid = ref Fixed.zero in
        let cnt = ref 0 in
        let prev_slot = ref null in
        let slot = ref b.lvl_head.(l) in
        while !slot <> null do
          let s = !slot in
          if not (Arena.is_alive b.arena s) then fail "Dead order slot in level queue";
          if Arena.level_idx b.arena s <> l then fail "Order level_idx mismatch";
          if Arena.side b.arena s <> side then fail "Order side mismatch";
          if not (Arena.px b.arena s =~ b.lvl_px.(l)) then fail "Order px != level px";
          if Arena.prev b.arena s <> !prev_slot then fail "Order prev pointer broken";
          if Arena.disp_qty b.arena s <=~ Fixed.zero then fail "Order disp_qty <= 0";
          if Arena.hid_qty b.arena s <~ Fixed.zero then fail "Order hid_qty < 0";
          if Arena.find_slot b.arena (Arena.id b.arena s) <> s then
            fail "Hash table lookup mismatch for active order";
          sum_disp := !sum_disp +~ Arena.disp_qty b.arena s;
          sum_hid := !sum_hid +~ Arena.hid_qty b.arena s;
          incr cnt;
          incr total_orders_seen;
          prev_slot := s;
          slot := Arena.next b.arena s
        done;
        if !prev_slot <> b.lvl_tail.(l) then fail "Level tail pointer broken";
        if !cnt = 0 || !cnt <> b.lvl_count.(l) then fail "Level order count mismatch";
        if not (!sum_disp =~ b.lvl_disp_vol.(l)) then fail "Level disp_vol sum mismatch";
        if not (!sum_hid =~ b.lvl_hid_vol.(l)) then fail "Level hid_vol sum mismatch";
        prev_lvl := l;
        curr := b.lvl_next.(l)
      done
    in
    check_side Bid b.best_bid_lvl;
    check_side Ask b.best_ask_lvl;
    if !total_orders_seen <> Arena.active_count b.arena then
      fail "Total orders across levels != Arena.active_count";
    Ok ()
  with Invariant_broken msg -> Error msg

(** Zero-allocation L3 Limit Order Book & Matching Engine.
    Supports GTC, IOC, FOK (with read-only pre-scan), Post-Only, Iceberg orders,
    Self-Trade Prevention (STP), O(1) cancel/amend, and integrated streaming quant stats. *)

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

type t

val create :
  ?capacity:int ->
  ?stp:stp_mode ->
  ?vpin_bucket_vol:Fixed.t ->
  ?vpin_buckets:int ->
  symbol:string ->
  unit ->
  t

val clear : t -> unit
val symbol : t -> string
val order_count : t -> int
val trade_count : t -> int
val volume_traded : t -> Fixed.t

val add_limit :
  t ->
  id:int ->
  ?trader:int ->
  side:side ->
  px:Fixed.t ->
  qty:Fixed.t ->
  ?tif:tif ->
  ?iceberg_peak:Fixed.t ->
  ?ts:int64 ->
  unit ->
  exec_report

val add_market :
  t ->
  id:int ->
  ?trader:int ->
  side:side ->
  qty:Fixed.t ->
  ?ts:int64 ->
  unit ->
  exec_report

val cancel : t -> id:int -> (Fixed.t, [> `Not_found ]) result

val amend :
  t ->
  id:int ->
  new_px:Fixed.t ->
  new_qty:Fixed.t ->
  ?ts:int64 ->
  unit ->
  (exec_report, [> `Not_found ]) result

val best_bid : t -> (Fixed.t * Fixed.t) option
val best_ask : t -> (Fixed.t * Fixed.t) option
val bbo : t -> (Fixed.t * Fixed.t) option * (Fixed.t * Fixed.t) option
val mid : t -> Fixed.t option
val spread : t -> Fixed.t option
val micro_px : t -> Fixed.t option
val imbalance : t -> float option

val depth : t -> ?levels:int -> unit -> level_snap list * level_snap list
val l3_orders : t -> side:side -> px:Fixed.t -> order_snap list
val find_order : t -> id:int -> order_snap option

val mid_stats : t -> Stats.Welford.t
val mid_ewma : t -> Stats.Ewma.t
val ofi : t -> Stats.Ofi.t
val vpin : t -> Stats.Vpin.t
val roll : t -> Stats.Roll.t

val verify_report_conservation : exec_report -> bool
val audit : t -> (unit, string) result

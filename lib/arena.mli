(** Zero-allocation flat order arena + open-addressed O(1) ID lookup table.
    Because triggering a GC collection in the matching hot path is a great way
    to donate alpha to faster counterparties. *)

type side =
  | Bid
  | Ask

type t

val null : int

val create : capacity:int -> t
val clear : t -> unit
val capacity : t -> int
val active_count : t -> int

val alloc :
  t ->
  id:int ->
  trader:int ->
  side:side ->
  px:Fixed.t ->
  disp_qty:Fixed.t ->
  hid_qty:Fixed.t ->
  peak_qty:Fixed.t ->
  ts:int64 ->
  (int, [> `Duplicate_id | `Arena_full ]) result

val free : t -> int -> unit
val find_slot : t -> int -> int

val is_alive : t -> int -> bool
val id : t -> int -> int
val trader : t -> int -> int
val side : t -> int -> side
val px : t -> int -> Fixed.t
val disp_qty : t -> int -> Fixed.t
val hid_qty : t -> int -> Fixed.t
val peak_qty : t -> int -> Fixed.t
val total_qty : t -> int -> Fixed.t
val ts : t -> int -> int64
val prev : t -> int -> int
val next : t -> int -> int
val level_idx : t -> int -> int

val set_px : t -> int -> Fixed.t -> unit
val set_disp_qty : t -> int -> Fixed.t -> unit
val set_hid_qty : t -> int -> Fixed.t -> unit
val set_ts : t -> int -> int64 -> unit
val set_prev : t -> int -> int -> unit
val set_next : t -> int -> int -> unit
val set_level_idx : t -> int -> int -> unit

(** Batteries-included ergonomic wrapper for [Obook].
    Zero ceremony: pass floats, get auto-assigned order IDs, grab JSON or
    pretty ASCII depth tables in one line. *)

type t

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

val book :
  ?capacity:int ->
  ?stp:Book.stp_mode ->
  ?vpin_bucket_vol:float ->
  ?vpin_buckets:int ->
  string ->
  t

val raw : t -> Book.t
val clear : t -> unit

val bid :
  t ->
  ?id:int ->
  ?trader:int ->
  ?tif:Book.tif ->
  ?iceberg:float ->
  px:float ->
  qty:float ->
  unit ->
  int * Book.exec_report

val ask :
  t ->
  ?id:int ->
  ?trader:int ->
  ?tif:Book.tif ->
  ?iceberg:float ->
  px:float ->
  qty:float ->
  unit ->
  int * Book.exec_report

val buy_mkt : t -> ?id:int -> ?trader:int -> qty:float -> unit -> Book.exec_report
val sell_mkt : t -> ?id:int -> ?trader:int -> qty:float -> unit -> Book.exec_report

val cancel : t -> int -> bool
val amend : t -> id:int -> px:float -> qty:float -> bool

val snap : t -> snap
val to_json : ?levels:int -> t -> string
val render_ascii : ?levels:int -> t -> string
val pp : ?levels:int -> t -> unit

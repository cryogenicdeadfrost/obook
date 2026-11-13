(** Obook: Zero-Allocation L3 Limit Order Book, 128-Bit Fixed-Point Math,
    ITCH 5.0 Wire Codec, and Streaming Microstructure Analytics. *)

module Fixed = Fixed
module Stats = Stats
module Arena = Arena
module Book = Book
module Itch = Itch
module Ez = Ez

val book :
  ?capacity:int ->
  ?stp:Book.stp_mode ->
  ?vpin_bucket_vol:float ->
  ?vpin_buckets:int ->
  string ->
  Ez.t

val bid :
  Ez.t ->
  ?id:int ->
  ?trader:int ->
  ?tif:Book.tif ->
  ?iceberg:float ->
  px:float ->
  qty:float ->
  unit ->
  int * Book.exec_report

val ask :
  Ez.t ->
  ?id:int ->
  ?trader:int ->
  ?tif:Book.tif ->
  ?iceberg:float ->
  px:float ->
  qty:float ->
  unit ->
  int * Book.exec_report

val buy_mkt : Ez.t -> ?id:int -> ?trader:int -> qty:float -> unit -> Book.exec_report
val sell_mkt : Ez.t -> ?id:int -> ?trader:int -> qty:float -> unit -> Book.exec_report
val cancel : Ez.t -> int -> bool
val amend : Ez.t -> id:int -> px:float -> qty:float -> bool
val snap : Ez.t -> Ez.snap
val pp : ?levels:int -> Ez.t -> unit

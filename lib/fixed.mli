(** Fixed-point 64-bit arithmetic scaled by [10^8] (1 satoshi / 1 pipette).
    Uses 128-bit intermediate math so multiplying $100k by 1,000 BTC doesn't
    vaporize your portfolio in an int64 overflow. *)

type t = private int64

type round =
  | Down
  | Up
  | Zero
  | Half_even

type err =
  [ `Overflow
  | `Div_by_zero
  | `Parse_err of string ]

val scale : int64
val zero : t
val one : t
val neg_one : t
val max_val : t
val min_val : t

val of_raw : int64 -> t
val to_raw : t -> int64
val of_int : int -> (t, [> `Overflow ]) result
val of_int_exn : int -> t
val of_f : ?round:round -> float -> (t, [> `Overflow ]) result
val of_f_exn : ?round:round -> float -> t
val to_f : t -> float

val of_str : string -> (t, [> err ]) result
val of_str_exn : string -> t
val to_str : ?dec:int -> t -> string

val neg : t -> (t, [> `Overflow ]) result
val abs : t -> (t, [> `Overflow ]) result
val add : t -> t -> (t, [> `Overflow ]) result
val sub : t -> t -> (t, [> `Overflow ]) result
val mul : ?round:round -> t -> t -> (t, [> `Overflow ]) result
val div : ?round:round -> t -> t -> (t, [> `Overflow | `Div_by_zero ]) result

val add_exn : t -> t -> t
val sub_exn : t -> t -> t
val mul_exn : ?round:round -> t -> t -> t
val div_exn : ?round:round -> t -> t -> t

val cmp : t -> t -> int
val eq : t -> t -> bool
val min : t -> t -> t
val max : t -> t -> t
val is_zero : t -> bool
val is_pos : t -> bool
val is_neg : t -> bool
val sign : t -> int

val midpoint : t -> t -> t
val wmid : bid_px:t -> bid_sz:t -> ask_px:t -> ask_sz:t -> (t, [> `Overflow | `Div_by_zero ]) result

module O : sig
  val ( +~ ) : t -> t -> t
  val ( -~ ) : t -> t -> t
  val ( *~ ) : t -> t -> t
  val ( /~ ) : t -> t -> t
  val ( =~ ) : t -> t -> bool
  val ( <>~ ) : t -> t -> bool
  val ( <~ ) : t -> t -> bool
  val ( <=~ ) : t -> t -> bool
  val ( >~ ) : t -> t -> bool
  val ( >=~ ) : t -> t -> bool
end

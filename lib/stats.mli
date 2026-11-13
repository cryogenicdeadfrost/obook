(** Streaming O(1) market microstructure & statistical indicators.
    No sliding arrays eating RAM, no catastrophic float cancellation. *)

module Welford : sig
  type t

  val create : unit -> t
  val reset : t -> unit
  val push : t -> float -> unit
  val n : t -> int
  val mean : t -> float option
  val var : t -> float option
  val var_pop : t -> float option
  val stdev : t -> float option
  val zscore : t -> float -> float option
  val merge : t -> t -> t
end

module Ewma : sig
  type t

  val create : alpha:float -> t
  val of_half_life : float -> t
  val of_span : int -> t
  val reset : t -> unit
  val push : t -> float -> unit
  val n : t -> int
  val mean : t -> float option
  val var : t -> float option
  val vol : t -> float option
end

module Ofi : sig
  type t

  val create : ?decay:float -> unit -> t
  val reset : t -> unit
  val push_bbo : t -> bid_px:Fixed.t -> bid_sz:Fixed.t -> ask_px:Fixed.t -> ask_sz:Fixed.t -> float
  val last : t -> float
  val cumulative : t -> float
  val decayed : t -> float
  val ticks : t -> int
end

module Vpin : sig
  type t

  val create : bucket_vol:Fixed.t -> n_buckets:int -> t
  val reset : t -> unit
  val push_trade : t -> is_buy:bool -> qty:Fixed.t -> unit
  val get : t -> float option
  val warmed_up : t -> bool
  val completed_buckets : t -> int
end

module Roll : sig
  type t

  val create : ?alpha:float -> unit -> t
  val reset : t -> unit
  val push_px : t -> float -> unit
  val autocov : t -> float option
  val spread : t -> float option
end

module Welford = struct
  type t = {
    mutable n : int;
    mutable mean : float;
    mutable m2 : float;
  }

  let create () = { n = 0; mean = 0.0; m2 = 0.0 }

  let reset s =
    s.n <- 0;
    s.mean <- 0.0;
    s.m2 <- 0.0

  (* E[X^2] - E[X]^2 turns into IEEE-754 soup when BTC ticks by $0.01 at $95k.
     Welford's recurrence keeps m2 strictly non-negative and stable. *)
  let push s x =
    if not (Float.is_nan x || Float.is_infinite x) then (
      s.n <- s.n + 1;
      let delta = x -. s.mean in
      s.mean <- s.mean +. (delta /. float_of_int s.n);
      let delta2 = x -. s.mean in
      s.m2 <- s.m2 +. (delta *. delta2)
    )

  let n s = s.n
  let mean s = if s.n = 0 then None else Some s.mean
  let var s = if s.n < 2 then None else Some (s.m2 /. float_of_int (s.n - 1))
  let var_pop s = if s.n < 1 then None else Some (s.m2 /. float_of_int s.n)

  let stdev s =
    match var s with
    | None -> None
    | Some v -> Some (Float.sqrt (Float.max 0.0 v))

  let zscore s x =
    match mean s, stdev s with
    | Some m, Some sd when sd > 1e-15 -> Some ((x -. m) /. sd)
    | _ -> None

  (* Chan's parallel variance merge so worker domains can combine stats in O(1). *)
  let merge a b =
    if a.n = 0 then { n = b.n; mean = b.mean; m2 = b.m2 }
    else if b.n = 0 then { n = a.n; mean = a.mean; m2 = a.m2 }
    else
      let na = float_of_int a.n and nb = float_of_int b.n in
      let n_tot = a.n + b.n in
      let nf = float_of_int n_tot in
      let delta = b.mean -. a.mean in
      let mean = a.mean +. (delta *. nb /. nf) in
      let m2 = a.m2 +. b.m2 +. (delta *. delta *. na *. nb /. nf) in
      { n = n_tot; mean; m2 }
end

module Ewma = struct
  type t = {
    alpha : float;
    mutable n : int;
    mutable mean : float;
    mutable var : float;
  }

  let create ~alpha =
    if alpha <= 0.0 || alpha >= 1.0 || Float.is_nan alpha then
      invalid_arg "Ewma.create: alpha must be in (0, 1)";
    { alpha; n = 0; mean = 0.0; var = 0.0 }

  let of_half_life hl =
    if hl <= 0.0 || Float.is_nan hl then
      invalid_arg "Ewma.of_half_life: half-life must be > 0";
    let alpha = 1.0 -. Float.exp (Float.log 0.5 /. hl) in
    create ~alpha

  let of_span span =
    if span < 2 then invalid_arg "Ewma.of_span: span must be >= 2";
    let alpha = 2.0 /. (float_of_int span +. 1.0) in
    create ~alpha

  let reset s =
    s.n <- 0;
    s.mean <- 0.0;
    s.var <- 0.0

  (* Finch (2009) incremental EWMA variance. First sample seeds mean directly
     so we don't spend the first 100 ticks crawling up from zero. *)
  let push s x =
    if not (Float.is_nan x || Float.is_infinite x) then
      if s.n = 0 then (
        s.n <- 1;
        s.mean <- x;
        s.var <- 0.0
      ) else (
        s.n <- s.n + 1;
        let diff = x -. s.mean in
        let incr = s.alpha *. diff in
        s.mean <- s.mean +. incr;
        s.var <- (1.0 -. s.alpha) *. (s.var +. (diff *. incr))
      )

  let n s = s.n
  let mean s = if s.n = 0 then None else Some s.mean
  let var s = if s.n < 2 then None else Some s.var
  let vol s = if s.n < 2 then None else Some (Float.sqrt (Float.max 0.0 s.var))
end

module Ofi = struct
  type t = {
    decay : float;
    mutable ticks : int;
    mutable prev_bid_px : Fixed.t;
    mutable prev_bid_sz : Fixed.t;
    mutable prev_ask_px : Fixed.t;
    mutable prev_ask_sz : Fixed.t;
    mutable last_ofi : float;
    mutable cum_ofi : float;
    mutable decayed_ofi : float;
  }

  let create ?(decay = 0.95) () =
    if decay <= 0.0 || decay > 1.0 || Float.is_nan decay then
      invalid_arg "Ofi.create: decay must be in (0, 1]";
    {
      decay;
      ticks = 0;
      prev_bid_px = Fixed.zero;
      prev_bid_sz = Fixed.zero;
      prev_ask_px = Fixed.zero;
      prev_ask_sz = Fixed.zero;
      last_ofi = 0.0;
      cum_ofi = 0.0;
      decayed_ofi = 0.0;
    }

  let reset s =
    s.ticks <- 0;
    s.prev_bid_px <- Fixed.zero;
    s.prev_bid_sz <- Fixed.zero;
    s.prev_ask_px <- Fixed.zero;
    s.prev_ask_sz <- Fixed.zero;
    s.last_ofi <- 0.0;
    s.cum_ofi <- 0.0;
    s.decayed_ofi <- 0.0

  (* Cont, Kukanov & Stoikov (2014): e_n = I(Pb_n >= Pb_{n-1})*qb_n - I(Pb_n <= Pb_{n-1})*qb_{n-1}
                                         - I(Pa_n <= Pa_{n-1})*qa_n + I(Pa_n >= Pa_{n-1})*qa_{n-1} *)
  let push_bbo s ~bid_px ~bid_sz ~ask_px ~ask_sz =
    if s.ticks = 0 then (
      s.ticks <- 1;
      s.prev_bid_px <- bid_px;
      s.prev_bid_sz <- bid_sz;
      s.prev_ask_px <- ask_px;
      s.prev_ask_sz <- ask_sz;
      s.last_ofi <- 0.0;
      0.0
    ) else (
      s.ticks <- s.ticks + 1;
      let qb = Fixed.to_f bid_sz and qb_prev = Fixed.to_f s.prev_bid_sz in
      let qa = Fixed.to_f ask_sz and qa_prev = Fixed.to_f s.prev_ask_sz in
      let c_bid =
        let c = Fixed.cmp bid_px s.prev_bid_px in
        if c > 0 then qb
        else if c = 0 then qb -. qb_prev
        else -.qb_prev
      in
      let c_ask =
        let c = Fixed.cmp ask_px s.prev_ask_px in
        if c < 0 then qa
        else if c = 0 then qa -. qa_prev
        else -.qa_prev
      in
      let ofi = c_bid -. c_ask in
      s.prev_bid_px <- bid_px;
      s.prev_bid_sz <- bid_sz;
      s.prev_ask_px <- ask_px;
      s.prev_ask_sz <- ask_sz;
      s.last_ofi <- ofi;
      s.cum_ofi <- s.cum_ofi +. ofi;
      s.decayed_ofi <- (s.decayed_ofi *. s.decay) +. ofi;
      ofi
    )

  let last s = s.last_ofi
  let cumulative s = s.cum_ofi
  let decayed s = s.decayed_ofi
  let ticks s = s.ticks
end

module Vpin = struct
  type t = {
    bucket_vol : int64;
    n_buckets : int;
    ring : int64 array;
    mutable head : int;
    mutable completed : int;
    mutable sum_imbalance : int64;
    mutable cur_buy : int64;
    mutable cur_sell : int64;
  }

  let create ~bucket_vol ~n_buckets =
    let bv = Fixed.to_raw bucket_vol in
    if bv <= 0L then invalid_arg "Vpin.create: bucket_vol must be > 0";
    if n_buckets < 1 then invalid_arg "Vpin.create: n_buckets must be >= 1";
    {
      bucket_vol = bv;
      n_buckets;
      ring = Array.make n_buckets 0L;
      head = 0;
      completed = 0;
      sum_imbalance = 0L;
      cur_buy = 0L;
      cur_sell = 0L;
    }

  let reset v =
    Array.fill v.ring 0 v.n_buckets 0L;
    v.head <- 0;
    v.completed <- 0;
    v.sum_imbalance <- 0L;
    v.cur_buy <- 0L;
    v.cur_sell <- 0L

  (* Whale protection: a single aggressive sweep can span 5 volume buckets at once.
     Slice incoming qty chunk-by-chunk so every bucket has exact volume V and VPIN <= 1.0. *)
  let push_trade v ~is_buy ~qty =
    let rem = ref (Fixed.to_raw qty) in
    while !rem > 0L do
      let cur_fill = Int64.add v.cur_buy v.cur_sell in
      let space = Int64.sub v.bucket_vol cur_fill in
      let take = if !rem < space then !rem else space in
      if is_buy then v.cur_buy <- Int64.add v.cur_buy take
      else v.cur_sell <- Int64.add v.cur_sell take;
      rem := Int64.sub !rem take;
      if Int64.add v.cur_buy v.cur_sell = v.bucket_vol then (
        let imb = Int64.abs (Int64.sub v.cur_buy v.cur_sell) in
        if v.completed >= v.n_buckets then
          v.sum_imbalance <- Int64.sub v.sum_imbalance v.ring.(v.head);
        v.ring.(v.head) <- imb;
        v.sum_imbalance <- Int64.add v.sum_imbalance imb;
        v.head <- (v.head + 1) mod v.n_buckets;
        v.completed <- v.completed + 1;
        v.cur_buy <- 0L;
        v.cur_sell <- 0L
      )
    done

  let warmed_up v = v.completed >= v.n_buckets
  let completed_buckets v = v.completed

  let get v =
    let active = Int.min v.completed v.n_buckets in
    if active = 0 then None
    else
      let denom = Int64.to_float v.bucket_vol *. float_of_int active in
      let num = Int64.to_float v.sum_imbalance in
      Some (Float.min 1.0 (Float.max 0.0 (num /. denom)))
end

module Roll = struct
  type t = {
    alpha : float;
    mutable n : int;
    mutable prev_px : float;
    mutable prev_dp : float;
    mutable mean_dp : float;
    mutable cov : float;
  }

  let create ?(alpha = 0.05) () =
    if alpha <= 0.0 || alpha >= 1.0 || Float.is_nan alpha then
      invalid_arg "Roll.create: alpha must be in (0, 1)";
    { alpha; n = 0; prev_px = 0.0; prev_dp = 0.0; mean_dp = 0.0; cov = 0.0 }

  let reset r =
    r.n <- 0;
    r.prev_px <- 0.0;
    r.prev_dp <- 0.0;
    r.mean_dp <- 0.0;
    r.cov <- 0.0

  (* Roll (1984): s = 2 * sqrt(-Cov(dP_t, dP_{t-1})).
     When serial covariance is positive (momentum regime), effective bounce spread is 0. *)
  let push_px r px =
    if not (Float.is_nan px || Float.is_infinite px) then
      if r.n = 0 then (
        r.n <- 1;
        r.prev_px <- px
      ) else if r.n = 1 then (
        r.n <- 2;
        let dp = px -. r.prev_px in
        r.prev_px <- px;
        r.prev_dp <- dp;
        r.mean_dp <- dp
      ) else (
        r.n <- r.n + 1;
        let dp = px -. r.prev_px in
        let dev_cur = dp -. r.mean_dp in
        let dev_prev = r.prev_dp -. r.mean_dp in
        r.mean_dp <- r.mean_dp +. (r.alpha *. dev_cur);
        r.cov <- ((1.0 -. r.alpha) *. r.cov) +. (r.alpha *. dev_cur *. dev_prev);
        r.prev_px <- px;
        r.prev_dp <- dp
      )

  let autocov r = if r.n < 3 then None else Some r.cov

  let spread r =
    if r.n < 3 then None
    else if r.cov >= 0.0 then Some 0.0
    else Some (2.0 *. Float.sqrt (-.r.cov))
end

type t = int64

type round =
  | Down
  | Up
  | Zero
  | Half_even

type err =
  [ `Overflow
  | `Div_by_zero
  | `Parse_err of string ]

let scale = 100_000_000L
let scale_f = 100_000_000.0
let zero = 0L
let one = scale
let neg_one = -100_000_000L
let max_val = Int64.max_int
let min_val = Int64.min_int

let of_raw x = x
let to_raw x = x

let add a b =
  if (b > 0L && a > Int64.sub Int64.max_int b)
     || (b < 0L && a < Int64.sub Int64.min_int b)
  then Error `Overflow
  else Ok (Int64.add a b)

let sub a b =
  if b = Int64.min_int then
    if a >= 0L then Error `Overflow else Ok (Int64.sub a b)
  else add a (Int64.neg b)

let neg a =
  if a = Int64.min_int then Error `Overflow else Ok (Int64.neg a)

let abs a =
  if a = Int64.min_int then Error `Overflow else Ok (Int64.abs a)

let of_int n =
  let x = Int64.of_int n in
  if x > Int64.div Int64.max_int scale || x < Int64.div Int64.min_int scale then
    Error `Overflow
  else Ok (Int64.mul x scale)

let of_int_exn n =
  match of_int n with
  | Ok v -> v
  | Error `Overflow -> invalid_arg "Fixed.of_int_exn: overflow"

(* Rounding helper on unsigned (quotient, remainder, divisor) with final sign.
   Compares rem vs (div - rem) so 2 * rem never overflows int64 when div = max_int. *)
let apply_round ~round ~neg_res ~q ~rem ~div =
  let inc =
    if rem = 0L then false
    else
      match round with
      | Zero -> false
      | Down -> neg_res
      | Up -> not neg_res
      | Half_even ->
        let comp = div |> fun d -> Int64.sub d rem in
        if rem > comp then true
        else if rem < comp then false
        else Int64.logand q 1L = 1L
  in
  let q' =
    if not inc then Ok q
    else if q = Int64.max_int then Error `Overflow
    else Ok (Int64.succ q)
  in
  match q' with
  | Error _ as e -> e
  | Ok mag -> Ok (if neg_res then Int64.neg mag else mag)

let of_f ?(round = Half_even) f =
  if Float.is_nan f || Float.is_infinite f then Error `Overflow
  else
    let scaled = f *. scale_f in
    let max_f = Int64.to_float Int64.max_int in
    let min_f = Int64.to_float Int64.min_int in
    if scaled >= max_f || scaled < min_f then Error `Overflow
    else
      let r =
        match round with
        | Zero -> Float.trunc scaled
        | Down -> Float.floor scaled
        | Up -> Float.ceil scaled
        | Half_even ->
          let fl = Float.floor scaled in
          let diff = scaled -. fl in
          if diff < 0.5 then fl
          else if diff > 0.5 then fl +. 1.0
          else
            let n = Int64.of_float fl in
            if Int64.logand n 1L = 0L then fl else fl +. 1.0
      in
      if r >= max_f || r < min_f then Error `Overflow
      else Ok (Int64.of_float r)

let of_f_exn ?round f =
  match of_f ?round f with
  | Ok v -> v
  | Error `Overflow -> invalid_arg "Fixed.of_f_exn: overflow or NaN"

let to_f x = Int64.to_float x /. scale_f

let add_u63 a b =
  if a > Int64.sub Int64.max_int b then Error `Overflow
  else Ok (Int64.add a b)

let mul_u63 a b =
  if a = 0L || b = 0L then Ok 0L
  else if a > Int64.div Int64.max_int b then Error `Overflow
  else Ok (Int64.mul a b)

(* $100k * 1,000 BTC = 10^24 raw units, which turns standard int64 into confetti.
   Decompose ua = qa*S + ra and ub = qb*S + rb so (ra * rb) < 10^16 << 2^63. *)
let mul ?(round = Half_even) a b =
  if a = 0L || b = 0L then Ok 0L
  else if a = Int64.min_int || b = Int64.min_int then Error `Overflow
  else
    let neg_res = (a < 0L) <> (b < 0L) in
    let ua = Int64.abs a in
    let ub = Int64.abs b in
    let qa = Int64.div ua scale and ra = Int64.rem ua scale in
    let qb = Int64.div ub scale and rb = Int64.rem ub scale in
    let rarb = Int64.mul ra rb in
    let q_low = Int64.div rarb scale in
    let rem = Int64.rem rarb scale in
    match mul_u63 qa qb with
    | Error _ as e -> e
    | Ok qaqb ->
      (match mul_u63 qaqb scale with
       | Error _ as e -> e
       | Ok term1 ->
         (match mul_u63 qa rb with
          | Error _ as e -> e
          | Ok term2 ->
            (match mul_u63 qb ra with
             | Error _ as e -> e
             | Ok term3 ->
               (match add_u63 term1 term2 with
                | Error _ as e -> e
                | Ok s1 ->
                  (match add_u63 s1 term3 with
                   | Error _ as e -> e
                   | Ok s2 ->
                     (match add_u63 s2 q_low with
                      | Error _ as e -> e
                      | Ok q -> apply_round ~round ~neg_res ~q ~rem ~div:scale))))))

(* Binary shift-and-add over the 27 bits of S = 10^8 to compute (r0 * S) / ub
   and (r0 * S) mod ub without ever exceeding ub <= Int64.max_int. *)
let div ?(round = Half_even) a b =
  if b = 0L then Error `Div_by_zero
  else if a = 0L then Ok 0L
  else if a = Int64.min_int || b = Int64.min_int then Error `Overflow
  else
    let neg_res = (a < 0L) <> (b < 0L) in
    let ua = Int64.abs a in
    let ub = Int64.abs b in
    let q0 = Int64.div ua ub in
    let r0 = Int64.rem ua ub in
    match mul_u63 q0 scale with
    | Error _ as e -> e
    | Ok q_high ->
      let q_frac = ref 0L in
      let rem = ref 0L in
      for bit = 26 downto 0 do
        q_frac := Int64.shift_left !q_frac 1;
        let diff_double = Int64.sub ub !rem in
        if !rem >= diff_double then (
          q_frac := Int64.succ !q_frac;
          rem := Int64.sub !rem diff_double
        ) else (
          rem := Int64.shift_left !rem 1
        );
        if Int64.logand (Int64.shift_right scale bit) 1L = 1L then (
          let diff_add = Int64.sub ub r0 in
          if !rem >= diff_add then (
            q_frac := Int64.succ !q_frac;
            rem := Int64.sub !rem diff_add
          ) else (
            rem := Int64.add !rem r0
          )
        )
      done;
      (match add_u63 q_high !q_frac with
       | Error _ as e -> e
       | Ok q -> apply_round ~round ~neg_res ~q ~rem:!rem ~div:ub)

let add_exn a b =
  match add a b with Ok v -> v | Error `Overflow -> invalid_arg "Fixed.add_exn: overflow"

let sub_exn a b =
  match sub a b with Ok v -> v | Error `Overflow -> invalid_arg "Fixed.sub_exn: overflow"

let mul_exn ?round a b =
  match mul ?round a b with Ok v -> v | Error `Overflow -> invalid_arg "Fixed.mul_exn: overflow"

let div_exn ?round a b =
  match div ?round a b with
  | Ok v -> v
  | Error `Div_by_zero -> invalid_arg "Fixed.div_exn: division by zero"
  | Error `Overflow -> invalid_arg "Fixed.div_exn: overflow"

let of_str s =
  let s = String.trim s in
  let len = String.length s in
  if len = 0 then Error (`Parse_err "empty string")
  else
    let neg_sign, idx =
      if s.[0] = '-' then (true, 1)
      else if s.[0] = '+' then (false, 1)
      else (false, 0)
    in
    if idx >= len then Error (`Parse_err s)
    else
      let body = String.sub s idx (len - idx) in
      let parts = String.split_on_char '.' body in
      let parse_digits str =
        let ok = ref true in
        for i = 0 to String.length str - 1 do
          let c = str.[i] in
          if c < '0' || c > '9' then ok := false
        done;
        !ok
      in
      match parts with
      | [ whole ] ->
        if String.length whole = 0 || not (parse_digits whole) then Error (`Parse_err s)
        else (
          match Int64.of_string_opt whole with
          | None -> Error `Overflow
          | Some w ->
            (match mul_u63 w scale with
             | Error _ as e -> e
             | Ok raw -> Ok (if neg_sign then Int64.neg raw else raw)))
      | [ whole; frac ] ->
        if (String.length whole = 0 && String.length frac = 0)
           || not (parse_digits whole)
           || not (parse_digits frac)
        then Error (`Parse_err s)
        else
          let w_res =
            if String.length whole = 0 then Ok 0L
            else
              match Int64.of_string_opt whole with
              | None -> Error `Overflow
              | Some w -> mul_u63 w scale
          in
          let flen = String.length frac in
          let frac_str =
            if flen >= 8 then String.sub frac 0 8
            else frac ^ String.make (8 - flen) '0'
          in
          let f_val = Int64.of_string frac_str in
          (match w_res with
           | Error _ as e -> e
           | Ok w_scaled ->
             (match add_u63 w_scaled f_val with
              | Error _ as e -> e
              | Ok mag -> Ok (if neg_sign then Int64.neg mag else mag)))
      | _ -> Error (`Parse_err s)

let of_str_exn s =
  match of_str s with
  | Ok v -> v
  | Error (`Parse_err msg) -> invalid_arg ("Fixed.of_str_exn: " ^ msg)
  | Error `Overflow -> invalid_arg "Fixed.of_str_exn: overflow"

let to_str ?(dec = 8) x =
  let dec = Int.max 0 (Int.min 8 dec) in
  if x = Int64.min_int then "-92233720368.54775808"
  else
    let sign_str = if x < 0L then "-" else "" in
    let ux = Int64.abs x in
    let whole = Int64.div ux scale in
    let frac = Int64.rem ux scale in
    if dec = 0 then Printf.sprintf "%s%Ld" sign_str whole
    else
      let full_frac = Printf.sprintf "%08Ld" frac in
      let trimmed = String.sub full_frac 0 dec in
      Printf.sprintf "%s%Ld.%s" sign_str whole trimmed

let cmp = Int64.compare
let eq = Int64.equal
let min a b = if a <= b then a else b
let max a b = if a >= b then a else b
let is_zero x = x = 0L
let is_pos x = x > 0L
let is_neg x = x < 0L
let sign x = if x > 0L then 1 else if x < 0L then -1 else 0

(* Bitwise (a & b) + ((a ^ b) >> 1) computes floor((a + b) / 2) with zero overflow. *)
let midpoint a b =
  Int64.add (Int64.logand a b) (Int64.shift_right (Int64.logxor a b) 1)

let wmid ~bid_px ~bid_sz ~ask_px ~ask_sz =
  match add bid_sz ask_sz with
  | Error _ as e -> e
  | Ok tot_sz ->
    if tot_sz <= 0L then Error `Div_by_zero
    else
      match mul bid_px ask_sz, mul ask_px bid_sz with
      | Ok n1, Ok n2 ->
        (match add n1 n2 with
         | Ok num -> div num tot_sz
         | Error _ as e -> e)
      | (Error _ as e), _ | _, (Error _ as e) -> e

module O = struct
  let ( +~ ) = add_exn
  let ( -~ ) = sub_exn
  let ( *~ ) = mul_exn ~round:Half_even
  let ( /~ ) = div_exn ~round:Half_even
  let ( =~ ) = eq
  let ( <>~ ) a b = not (eq a b)
  let ( <~ ) a b = cmp a b < 0
  let ( <=~ ) a b = cmp a b <= 0
  let ( >~ ) a b = cmp a b > 0
  let ( >=~ ) a b = cmp a b >= 0
end

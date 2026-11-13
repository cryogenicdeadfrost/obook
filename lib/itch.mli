(** Zero-copy NASDAQ TotalView-ITCH 5.0 binary wire codec & book driver.
    Reads big-endian network frames straight out of a [bytes] buffer without
    allocating intermediate strings on the hot path. *)

type msg =
  | System_event of {
      ts : int64;
      code : char;
    }
  | Add_order of {
      ts : int64;
      oid : int;
      side : Book.side;
      qty : Fixed.t;
      symbol : string;
      px : Fixed.t;
    }
  | Order_executed of {
      ts : int64;
      oid : int;
      exec_qty : Fixed.t;
    }
  | Order_cancel of {
      ts : int64;
      oid : int;
      canc_qty : Fixed.t;
    }
  | Order_delete of {
      ts : int64;
      oid : int;
    }
  | Order_replace of {
      ts : int64;
      orig_oid : int;
      new_oid : int;
      new_qty : Fixed.t;
      new_px : Fixed.t;
    }

val msg_size : char -> int option
val decode : bytes -> off:int -> (msg * int, string) result
val decode_all : bytes -> (msg list, string) result
val encode : msg -> bytes
val encode_all : msg list -> bytes
val apply : Book.t -> msg -> unit

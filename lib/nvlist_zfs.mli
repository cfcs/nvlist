module Fletcher4 : sig

  type t = {
    a: Int64.t ;
    b: Int64.t ;
    c: Int64.t ;
    d: Int64.t ;
  }

  (* A zero-initialized checksum state *)
  val empty : t

  (* [varsize old_t src off len] is [old_t] updated
     with the bytes in [src] from [off] through [off+len-1].
     [off] and [len] must be multiples of [4] (32-bit alignment), otherwise an assertion is triggered.
  *)
  val varsize : t -> string -> int -> int -> t

  (* [truncated_fletcher4 payload] is the first word
     (the [a] word) of the Fletcher-4 checksum of the 32-bit aligned [payload].
     The last 0-3 bytes (if not word-aligned) are ignored.
  *)
  val truncated_fletcher4 : string -> int64

end

module Resumetoken : sig

  (** [parse resumetok] is the decoded resume token *)
  val parse : string -> (Nvlist.nvl, string) result

  val create : obj:int64 -> offset:int64 ->
    bytes:int64 -> toguid:int64 -> toname:string -> (string, string) result
end

type t =
  | Boolean:          t
  | Byte   : char  -> t
  | Int16  : int   -> t
  | Uint16 : int   -> t
  | Int32  : int32 -> t
  | Uint32 : int32 -> t
  | Int64  : int64 -> t
  | Uint64 : int64 -> t
  | Str : string -> t
  | ByteArray   : string   -> t
  | Int16Array  : int list -> t
  | Uint16Array : int list -> t
  | Int32Array  : int32 list -> t
  | Uint32Array : int32 list -> t
  | Int64Array  : int64 list -> t
  | Uint64Array : int64 list -> t
  | HrTime      : int64       -> t
  | StrArray    : string list -> t
  | Nvlist : nvl -> t
  | NvlistArray : nvl list -> t
  | BooleanValue : bool -> t
  | Int8 : int -> t
  | Uint8 : int -> t
  | BooleanArray : bool list -> t
  | Int8Array  : int list -> t
  | Uint8Array : int list -> t
  | Double : t (** [Double] is not implemented in fnvlist_(un)pack *)
and nvvalue = t
and nvl = (string * t) list

val compare : t -> t -> int

val pp_value : Format.formatter -> t -> unit
val pp_nvl : Format.formatter -> nvl -> unit

val fnvlist_unpack : string -> (nvl, string) result
val fnvlist_pack : nvl -> (string, string) result

(** [lookup key nvl] is the value stored under [key] inside [nvl], or [exception Not_found]*)
val lookup : string -> nvl -> t

val lookup_uint64 : string -> nvl -> int64

val lookup_string : string -> nvl -> string

(**/*)

val pad_str : Buffer.t -> string -> unit

val unhex : string -> string

val hex : string -> string

val to_c : nvl -> string

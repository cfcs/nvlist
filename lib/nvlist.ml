module Logs =
  (val Logs.src_log
      ( Logs.Src.create "nvlist"
          ~doc:"Sun/Solaris nvlist serialization"
      ) : Logs.LOG)

let unhex s =
  String.init (String.length s lsr 1) (fun i ->
      let nib ch =
        let y = (Char.code ch - 0x10) land (lnot 0x20)
        in y - (((y land 0x16) lsr 4) * 71) in
      Char.chr @@ ((nib s.[2*i]) lsl 4) lor (nib s.[2*i+1]) )

let hex s =
  String.init (String.length s * 2) (fun i ->
      let c =
        let a =
          86 + let ch = Char.code s.[i lsr 1] in
          (((ch lsr (4 - 4* (i land 1)))) land 0xf) in
        1 + ((a - 71 * ((a land 0x10) lsr 4)) lor 0x20) in
      Char.unsafe_chr (c land 0xff))

type nvflag = {
  nv_unique_name      : bool ; (* 0b01 *)
  nv_unique_name_type : bool ; (* 0b10*)
}

let nvflag_of_int32 v =
  { nv_unique_name = 1l = Int32.logand v 0b1_l ;
    nv_unique_name_type = 2l = Int32.logand v 0b10_l ;
  }

let int32_of_nvflag { nv_unique_name = nun ; nv_unique_name_type = nunt } =
  Int32.logor
    (if nun  then 1_l else 0_l)
    (if nunt then 2_l else 0_l)
(*
type nvpair = {
  size: int32;
  name_sz: int; (* int16 *)
  reserve: int; (* int16 *)
  value_elem: int32; (* number of elements for array types *)
  nvp_type: int; (* enum data_type_t include/sys/nvpair.h *)
}
type _nvl = { (* include/sys/nvpair.h *)
  version: int32; (* NV_VERSION = 0 *)
  nvflag:  int32; (* NV_UNIQUE_NAME:0x1 | NV_UNIQUE_NAME_TYPE:0x2 *)
  priv:    int64;
  flag:    int32; (* 0 *)
  pad:     int32; (* 0 *)
}

flow
   nvlist_init(): set NV_VERSION etc etc
   nvs_encode_pairs: walks list, calls nvs->nvs_ops->nvs_nvpair()
   nvs_native_ops:.nvs_nvl_fini=
   nvs_native_nvl_fini(): add uint32 (NULL) to end of list
*)

open Rresult

type el_typ =
  | T_Boolean
  | T_Byte
  | T_Int16
  | T_Uint16
  | T_Int32
  | T_Uint32
  | T_Int64
  | T_Uint64
  | T_Str
  | T_ByteArray
  | T_Int16Array
  | T_Uint16Array
  | T_Int32Array
  | T_Uint32Array
  | T_Int64Array
  | T_Uint64Array
  | T_HrTime
  | T_StrArray
  | T_Nvlist
  | T_NvlistArray
  | T_BooleanValue
  | T_Int8
  | T_Uint8
  | T_BooleanArray
  | T_Int8Array
  | T_Uint8Array
  | T_Double

let el_typ_of_int32 = function
  | 0_l -> Error "el_typ is zero"
  | 1_l -> Ok T_Boolean
  | 2_l -> Ok T_Byte
  | 3_l -> Ok T_Int16
  | 4_l -> Ok T_Uint16
  | 5_l -> Ok T_Int32
  | 6_l -> Ok T_Uint32
  | 7_l -> Ok T_Int64
  | 8_l -> Ok T_Uint64
  | 9_l -> Ok T_Str
  | 10_l -> Ok T_ByteArray
  | 11_l -> Ok T_Int16Array
  | 12_l -> Ok T_Uint16Array
  | 13_l -> Ok T_Int32Array
  | 14_l -> Ok T_Uint32Array
  | 15_l -> Ok T_Int64Array
  | 16_l -> Ok T_Uint64Array
  | 17_l -> Ok T_StrArray
  | 18_l -> Ok T_HrTime
  | 19_l -> Ok T_Nvlist
  | 20_l -> Ok T_NvlistArray
  | 21_l -> Ok T_BooleanValue
  | 22_l -> Ok T_Int8
  | 23_l -> Ok T_Uint8
  | 24_l -> Ok T_BooleanArray
  | 25_l -> Ok T_Int8Array
  | 26_l -> Ok T_Uint8Array
  | 27_l -> Ok T_Double
  | n when 0_l <= n && n < 27_l -> failwith "oops el_typ"
  | n -> Error (Fmt.str "not implemented el_typ=%#lx" n)

type t =
  | Boolean:          t
  | Byte   : char  -> t
  | Int16  : int   -> t
  | Uint16 : int   -> t
  | Int32  : int32 -> t
  | Uint32 : int32 -> t
  | Int64  : int64 -> t
  | Uint64 : int64 -> t (* DATA_TYPE_UINT64 *)
  | Str : string -> t
  | ByteArray : string -> t
  | Int16Array : int list -> t
  | Uint16Array : int list -> t
  | Int32Array : int32 list -> t
  | Uint32Array : int32 list -> t
  | Int64Array : int64 list -> t
  | Uint64Array : int64 list -> t
  | HrTime      : int64 -> t
  | StrArray    : string list -> t
  | Nvlist : nvl -> t
  | NvlistArray : nvl list -> t
  | BooleanValue : bool -> t
  | Int8         : int -> t
  | Uint8        : int -> t
  | BooleanArray : bool list -> t
  | Int8Array  : int list -> t
  | Uint8Array : int list -> t
  | Double     :  t
and nvvalue = t
and nvl = (string * t) list

let element_count = function
  | ByteArray vec -> Int32.of_int (String.length vec)
  | StrArray lst -> Int32.of_int (List.length lst)
  | NvlistArray lst -> Int32.of_int (List.length lst)
  | Uint16Array lst
  | Int16Array lst -> Int32.of_int (List.length lst)
  | Uint32Array lst
  | Int32Array lst -> Int32.of_int (List.length lst)
  | Uint64Array lst
  | Int64Array lst -> Int32.of_int (List.length lst)
  | Int8Array lst | Uint8Array lst -> Int32.of_int (List.length lst)
  | BooleanArray lst -> Int32.of_int (List.length lst)
  | Boolean -> 0l
  | Int16 _ | Uint16 _ | Int8 _ | Uint8 _
  | Int32 _ | Uint32 _
  | Int64 _ | Uint64 _ | HrTime _ | Double
  | Str _ | Nvlist _ | Byte _ | BooleanValue _
    -> 1l
  | _ -> .

let lookup key nvl =
  List.assoc key nvl

let lookup_uint64 key nvl =
  match lookup key nvl with
  | Uint64 n -> n
  | _ -> raise Not_found

let lookup_string key nvl =
  match lookup key nvl with
  | Str n -> n
  | _ -> raise Not_found

let typeof = function
  | Boolean  -> 1_l
  | Byte _   -> 2_l
  | Int16 _  -> 3_l
  | Uint16 _ -> 4_l
  | Int32 _  -> 5_l
  | Uint32 _ -> 6_l
  | Int64 _  -> 7_l
  | Uint64 _ -> 8_l
  | Str _    -> 9_l
  | ByteArray _   -> 10_l
  | Int16Array _  -> 11_l
  | Uint16Array _ -> 12_l
  | Int32Array _  -> 13_l
  | Uint32Array _ -> 14_l
  | Int64Array _  -> 15_l
  | Uint64Array _ -> 16_l
  | StrArray _ -> 17_l
  | HrTime _   -> 18_l
  | Nvlist _   -> 19_l (* 0x13 *)
  | NvlistArray _  -> 20_l
  | BooleanValue _ -> 21_l
  | Int8 _         -> 22_l
  | Uint8 _        -> 23_l
  | BooleanArray _ -> 24_l
  | Int8Array _  -> 25_l
  | Uint8Array _ -> 26_l
  | Double -> 27_l

let align8 size =
  (8 - (size land 7)) land 7

let align8_total size =
  size + align8 size

let compare a b =
  if a = b then 0 else
  if a < b then -1 else 1

let rec pp_value (ppf:Format.formatter) v =
  match (v : t) with
  | Boolean -> Format.fprintf ppf "@[Boolean@]"
  | BooleanValue v -> Format.fprintf ppf "@[BooleanValue %b@]" v
  | Byte v -> Format.fprintf ppf "@[Byte %C@]" v
  | Int64 v -> Format.fprintf ppf "@[Int64 @[(@[%#LxL@])@]@]" v
  | Uint64 v -> Format.fprintf ppf "@[Uint64 @[(%#Lx_L)@]@]" v
  | Int16 v -> Format.fprintf ppf "@[Int16 @[(@[%#x@])@]@]" v
  | Uint16 v -> Format.fprintf ppf "@[Uint16 @[(@[%u | %#x@])@]@]" v v
  | Int32 v -> Format.fprintf ppf "@[Int32 @[(@[%#lx_l@])@]@]" v
  | Uint32 v -> Format.fprintf ppf "@[Uint32 @[(%#lx_l)@]@]" v
  | Int8 v -> Format.fprintf ppf "@[Int8 @[(@[%#x@])@]@]" v
  | Uint8 v -> Format.fprintf ppf "@[Int8 @[(@[%#x@])@]@]" v
  | Str s -> Format.fprintf ppf "@[Str %S@]" s
  | ByteArray s -> Format.fprintf ppf "@[ByteArray %S@]" s
  | Int16Array lst ->
    Format.fprintf ppf "@[Int16Array @[[%a@]]@]"
      (Format.pp_print_list
         ~pp_sep:(fun ppf () -> Format.pp_print_char ppf ';')
         Format.pp_print_int) lst
  | Uint16Array lst ->
    Format.fprintf ppf "@[Uint16Array @[[%a@]]@]"
      (Format.pp_print_list
         ~pp_sep:(fun ppf () -> Format.pp_print_char ppf ';')
         Format.pp_print_int) lst
  | Int32Array lst ->
    Format.fprintf ppf "@[Int32Array @[[%a@]]@]"
      (Format.pp_print_list
         ~pp_sep:(fun ppf () -> Format.pp_print_char ppf ';')
         Fmt.int32) lst
  | Uint32Array lst ->
    Format.fprintf ppf "@[Uint32Array @[[%a@]]@]"
      (Format.pp_print_list
         ~pp_sep:(fun ppf () -> Format.pp_print_char ppf ';')
         Fmt.uint32) lst
  | Int64Array lst ->
    Format.fprintf ppf "@[Int64Array @[[%a@]]@]"
      (Format.pp_print_list
         ~pp_sep:(fun ppf () -> Format.pp_print_char ppf ';')
         (fun ppf v ->
            Fmt.pf ppf "%a_L" Fmt.int64 v)) lst
  | Uint64Array lst ->
    Format.fprintf ppf "@[Uint64Array @[[%a@]]@]"
      (Format.pp_print_list
         ~pp_sep:(fun ppf () -> Format.pp_print_char ppf ';')
         (fun ppf v ->
            Fmt.pf ppf "%a_L" Fmt.uint64 v)) lst
  | Int8Array lst ->
    Format.fprintf ppf "@[Int8Array @[[%a@]]@]"
      (Format.pp_print_list
         ~pp_sep:(fun ppf () -> Format.pp_print_char ppf ';')
         Fmt.int) lst
  | Uint8Array lst ->
    Format.fprintf ppf "@[Uint8Array @[[%a@]]@]"
      (Format.pp_print_list
         ~pp_sep:(fun ppf () -> Format.pp_print_char ppf ';')
         Fmt.int) lst
  | NvlistArray lst ->
    Format.fprintf ppf "@[NvlistArray @[[%a]@]@]"
      (Format.pp_print_list ~pp_sep:(fun ppf () -> Format.pp_print_char ppf ';')
         pp) lst
  | Nvlist nvl -> Fmt.pf ppf "Nvlist %a" pp nvl
  | StrArray strs ->
    Fmt.pf ppf "(StrArray [%a])"
      (Fmt.list ~sep:(Fmt.any ";") (fun ppf -> Fmt.pf ppf "%S")) strs
  | BooleanArray lst ->
    Format.fprintf ppf "@[BooleanArray @[[%a@]]@]"
      (Format.pp_print_list
         ~pp_sep:(fun ppf () -> Format.pp_print_char ppf ';')
         Fmt.bool) lst
  | Double ->
    Fmt.pf ppf "@[Double@]"
  | HrTime nanosec ->
    Fmt.pf ppf "@[HrTime %Lu]" nanosec
  | _ -> .
and pp ppf (lst:nvl) =
  Format.fprintf ppf "@[ @[[@[%a@]]@]@]@,"
    (Format.pp_print_list ~pp_sep:(fun ppf () ->
         Format.pp_print_char ppf ';' ;
         Format.pp_print_cut ppf ()
       )
        (fun ppf ((k:string),(v:t)) ->
           Format.fprintf ppf "@[ @[(@[%10S, @[%a@]@])@]@]" k pp_value v)
    ) (lst:nvl)

let pp_nvl ppf (v:nvl) =
  pp ppf v

let add_string key value old =
  match List.assoc_opt key old with
  | None -> (key, value)::old
  | Some _ ->(key,value)::List.remove_assoc key old

let pad_str b s =
  assert (not @@ String.contains s '\000');
  Buffer.add_string b s ;
  Buffer.add_char b '\000' ;
  (* needs to be 32-bit aligned: *)
  if (String.length s + 1) mod 8 <> 0 then
    for i = 0 to 7 - ((
        String.length s+1) mod 8) do
      Buffer.add_char b '\000'
    done

module Nvl_state = struct
  module NameSet = Set.Make(String)
  module NameTypeSet = Set.Make(struct
      type t = el_typ * string
      let compare a b = Stdlib.compare a b
    end)
  type t = {
    nvflag: nvflag ;
    names: NameSet.t;
    nametypes: NameTypeSet.t ;
  }

  let empty_flags = nvflag_of_int32 0l

  let empty = {
    nvflag = {
      nv_unique_name = false;
      nv_unique_name_type = false;} ;
    names = NameSet.empty;
    nametypes = NameTypeSet.empty;
  }

  let add_name t (name:string) (typ:el_typ) =
    (* inline version of openzfs:nvpair.c:nvt_nvpair_match()
       which considers nv_unique_name_type to take precedence.
       are the flags exclusive?
       ie if nv_unique_name_type=true then match=false
       even if nv_unique_name is set and there are duplicate names.

       this is interesting because in fnvlist when you add,
       it updates duplicates by removing ALL dupl names if NV_UNIQUE_NAME is
       set, and otherwise it'll remove of a given type (makes sense).

       but
       nvt_nvpair_match(a,b,nvflag=NV_UNIQUE_NAME | NV_UNIQUE_NAME_TYPE)
       is B_FALSE if a and b have similar names but different types.
       interestingly it will also crash if nvflag &~3

       nvt_nvpair_match is only called from
       nvt_remove_nvpair(nvlist_t *nvl, nvpair_t *nvp)
       so nvt_remove_pair() will NOT honour NV_UNIQUE_NAME
       if NV_UNIQUE_NAME_TYPE is set, unlike the semantics of
       nvlist_add_common

       conceivably this could be a problem if a datastructure is deserialized
       with both flags set, and the program then tries to sanitize it by
       removing an entry with
       nvt_add_nvpair() -> nvlist_remove_nvpair() -> nvt_remove_nvpair()
       nvlist_add_common() -> nvt_add_nvpair()
       nvs_decode_pairs() -> nvt_add_nvpair()

       it seems to me like nvt_nvpair_match() which was introduced in illumos:9580 in dec 2017
       should validate both, if both flags are set.

    *)
    match t.nvflag with
    | { nv_unique_name_type = true ; nv_unique_name = _ } ->
      (* unique_name_type is weaker than unique_name;
         it allows duplicate entries as long as their type differs: *)
      let tuple = (typ,name) in
      if NameTypeSet.mem tuple t.nametypes
      then Error "nametyp conflict"
      else Ok {t with nametypes = NameTypeSet.add tuple t.nametypes}
    | { nv_unique_name = true ; nv_unique_name_type = _ } ->
      if NameSet.mem name t.names
      then begin
        Fmt.pr "conflicting name %S in %a\n%!"
          name Fmt.(seq ~sep:(any";") @@ quote string) (NameSet.to_seq t.names)
        ;
        Error "unique name present twice"
      end else Ok ({t with names = NameSet.add name t.names})
    | { nv_unique_name = false ; nv_unique_name_type = false } ->
      Ok t

  let add_flags flags (t:t) =
    {t with nvflag = {
         nv_unique_name = t.nvflag.nv_unique_name || flags.nv_unique_name ;
         nv_unique_name_type = t.nvflag.nv_unique_name_type ||
                               flags.nv_unique_name_type ;
       }}

  let descend_child (t:t) flags =
    (* child inherits persistent flags, but
       names are not required to be unique in the whole structure: *)
    add_flags t.nvflag empty
    |> add_flags flags
end

let write_nvlist_header b (nvstate:Nvl_state.t) =
  (* adds 128+64 = 192 bits = 24 bytes *)
  let add_int_32, add_int_64 =
    if true (*little-endian*)
    then Buffer.add_int32_le b, Buffer.add_int64_le b
    else Buffer.add_int32_be b, Buffer.add_int64_be b
  in
  add_int_32 0_l ; (* nvl_version*)
  add_int_32 (int32_of_nvflag nvstate.nvflag) ;
  add_int_64 0_L ; (* TODO priv *)
  add_int_32 0_l ; (* TODO flag *)
  add_int_32 0_l ; (* TODO pad *)
  ()

let validate_nv ~nvflag nvl =
  Ok nvl


let read_nvlist_header b off =
  Fmt.pr "read_nvlist_header off:%d b:%s\n%!" off (Bytes.to_string b |> hex) ;
  (if off >= Bytes.length b - 24
   then Error (Fmt.str "%s input too short" __FUNCTION__)
   else Ok ()) >>=fun() ->
  let nvl_version = Bytes.get_int32_le b off in
  let nvflag = Bytes.get_int32_le b (off+4) in
  let priv = Bytes.get_int64_le b   (off+4+4) in
  let flag = Bytes.get_int32_le b   (off+4+4+8) in
  let pad = Bytes.get_int32_le b    (off+4+4+8+4) in
  Fmt.pr "off:%d nvlist_header_t: %ld %ld priv:%#Lx %ld %ld\n%!"
    off nvl_version nvflag priv flag pad ;
  let _assert_ b1 b2 =
    if b1 = b2 then Ok () else Error (Fmt.str "%s violation" __FUNCTION__) in
  begin if nvl_version <> 0l then
     Error "embedded nvlist header is not 0_l"
   else Ok () end  >>= fun () ->
  (* libnvpair:fnvlist_unpack doesn't care:
     assert_ nvflag 1_l >>= fun () -> (* persistent flags *)
     assert_ priv   0_L >>= fun () ->
     assert_ flag   0_l >>= fun () -> (* flag for this? *)
     assert_ pad    0_l >>= fun () ->
  *)
  Ok (off+24, nvflag_of_int32 nvflag)

let rec get_nv_size ~name nvl =
    align8_total (String.length name +1 )
    + 4 (* el_size *)
    + 4 (* el_namesz *)
    + 4 (* el_count *)
    + 4 (* el_typ *)
    +
    match nvl with
    | Nvlist vec -> 24
    | Str vec -> align8_total (1 + String.length vec)
    | Boolean -> 0
    | Int8 _ | Uint8 _ | Int16 _  | Uint16 _ | Int32 _ | Uint32 _
    | Uint64 _ | Int64 _ | BooleanValue _ | Double | HrTime _
    | Byte _ -> 0x8
    | NvlistArray _ as arr ->
      (element_count arr |> Int32.to_int)*0x20
    | StrArray lst ->
      let n = List.fold_left (fun acc str ->
          acc
          + 1 (* asciiz nullbyte *)
          + (String.length str)
          + 8 (* header space for pointer array*)
        ) 0 lst in
      align8_total (n)
    | BooleanArray lst ->align8_total (4 * (List.length lst))
    | Int16Array vec  -> align8_total (2 * List.length vec)
    | Uint16Array vec -> align8_total (2 * List.length vec )
    | Int32Array vec  -> align8_total (4 * List.length vec)
    | Uint32Array vec -> align8_total (4 * List.length vec)
    | Int64Array vec  -> align8_total (8 * List.length vec)
    | Uint64Array vec -> align8_total (8 * List.length vec)
    | Int8Array vec   -> align8_total (1 * List.length vec)
    | Uint8Array vec  -> align8_total (1 * List.length vec)
    | ByteArray vec   -> align8_total (1 * String.length vec)
    | _ -> .

let fnvlist_pack (nv:nvl) =
  (* see
     sys/sys/nv.h
     sys/cddl/contrib/opensolaris/common/nvpair/opensolaris_fnvpair.c
     NV_UNIQUE_NAME
     NV_ENCODE_NATIVE
     fnvlist_pack
     calls:
     sys/contrib/libnv/nvlist.c:nvlist_xpack(NV_ENCODE_NATIVE)
     ^- that versions starts with NVLIST_MAGIC ('l' / 0x6c)
     which calls out to nvlist_common with nvs_op=NVS_OP_ENCODE
  *)
  let b = Buffer.create 48 in

  (* only written at the start of a serialized structure *)
  (* nvs_header_t
     encoding
     endian = host_endian
     reserved1
     reserved2
     nvs_native()
  *)
  (* this is native encoding (not XDR): *)
  Buffer.add_int8 b 0 ; (* nvs_header_t.nvh_encoding: 0 == native *)
  Buffer.add_int8 b 1 ; (* nvs_header_t.nvh_endian: 1 == little-endian *)
  Buffer.add_int8 b 0 ; (* nvs_header_t.nvh_reserved1 *)
  Buffer.add_int8 b 0 ; (* nvs_header_t.nvh_reserved2 *)
  (* nvs_operation() -> nvs_native_nvlist(): *)
  let first_nvstate = Nvl_state.empty |> Nvl_state.add_flags {
      nv_unique_name = true ; (* 1 *)
      nv_unique_name_type = false; (* 2*)
    } in
  Buffer.add_int32_le b 0l ; (* nvl_version *)
  Buffer.add_int32_le b (int32_of_nvflag first_nvstate.nvflag) ;

  let[@inline] align8_pad b size =
    let pad = (8 - (size land 7)) land 7 in
    assert (pad < 8);
    assert (pad >= 0);
    Buffer.add_string b (String.make pad '\x00')
  in

  let rec pack_untagged b nvstate = function
    | Boolean -> Ok ()
    | BooleanValue v ->
      Buffer.add_int8 b (Bool.to_int v) ;
      Ok (align8_pad b 1)
    | Byte v ->
      Buffer.add_char b v ;
      Ok (align8_pad b 1)
    | Int16 n | Uint16 n ->
      Buffer.add_int16_le b n ;
      Ok (align8_pad b 2)
    | Int32 n | Uint32 n ->
      Buffer.add_int32_le b n ;
      Ok (Buffer.add_int32_le b 0x0l) (* padding *)
    | (Int64 n | Uint64 n | HrTime n) ->
      (* hrtime_t is a Solaris time span, it contains 64 bits of nanoseconds *)
      Ok (Buffer.add_int64_le b n)
    | Str s ->
      Ok (pad_str b s)
    | NvlistArray arr ->
      (* NVLIST_ARRAY is prefixed by an array of pointers: *)
      let pad = 8 * List.length arr in (* sizeof(uint64_t) * count *)
      Buffer.add_string b (String.make pad '\x00');

      let nvstate = (Nvl_state.descend_child nvstate Nvl_state.empty_flags) in
      List.iter (fun _ -> write_nvlist_header b nvstate) arr;
      List.fold_left (fun acc child ->
          acc >>= fun () ->
          pack_native b nvstate child) (Ok ()) arr
    | Int16Array lst | Uint16Array lst ->
      List.iter (fun i -> Buffer.add_int16_le b i) lst;
      let elt_size = 2 in
      Ok (align8_pad b (elt_size * List.length lst))
    | Int32Array lst | Uint32Array lst ->
      List.iter (fun i -> Buffer.add_int32_le b i) lst;
      let elt_size = 4 in
      Ok (align8_pad b (elt_size * List.length lst))
    | Int64Array lst | Uint64Array lst ->
      List.iter (fun i -> Buffer.add_int64_le b i) lst;
      let elt_size = 8 in
      Ok (align8_pad b (elt_size * List.length lst))
    | Int8 i | Uint8 i ->
      Buffer.add_int8 b i ;
      Ok (align8_pad b 1)
    | BooleanArray lst ->
      List.iter (fun i -> Buffer.add_int32_le b
                    (Bool.to_int i|>Int32.of_int)) lst ;
      Ok (align8_pad b (4*List.length lst))
    | Int8Array lst | Uint8Array lst ->
      List.iter (fun i -> Buffer.add_int8 b i) lst;
      let elt_size = 1 in
      Ok (align8_pad b (elt_size * List.length lst))
    | ByteArray vec ->
      Buffer.add_string b vec ;
      Ok (align8_pad b (String.length vec))
    | Nvlist nvl ->
      let nvstate = (Nvl_state.descend_child nvstate Nvl_state.empty_flags) in
      write_nvlist_header b nvstate ;
      pack_native b nvstate nvl
    | StrArray strs ->
      (* STRING_ARRAY is prefixed by an array of pointers: *)
      List.iter (fun _ -> Buffer.add_string b (String.make 8 '\x00')) strs ;
      (* Then follows all the strings, separated by nullbytes
         (but not aligned, ie no 32bit padding between entries): *)
      List.fold_left (fun count x ->
          Buffer.add_string b x ;
          Buffer.add_char b '\x00' ;
          count + String.length x + 1) 0 strs
      |> align8_pad b ;
      Ok ()
    | Double ->
      (* Buffer.add_int64_le b (Int64.bits_of_float f) *)
      Ok (Buffer.add_string b (String.make 8 '\x00'))
    | _ -> .
  and pack_native b (o_nvstate:Nvl_state.t) (nvl:nvl) : (unit, 'a) result =
    let rec pairs nvstate = function
      | [] ->
        (* nvs_native_nvl_fini(): *)
        Buffer.add_int32_le b 0x0l;
        Ok ()
      | (key, nvalue)::next ->
        let child_b = Buffer.create 10 in
        (* key length, including Zero byte: *)
        Buffer.add_int32_le child_b (String.length key+1 |> Int32.of_int) ;
        (* element count: *)
        Buffer.add_int32_le child_b (element_count nvalue);
        (* encode type: *)
        Buffer.add_int32_le child_b (typeof nvalue) ;

        (* key: *)
        Nvl_state.add_name nvstate key (
          typeof nvalue |> el_typ_of_int32 |> Rresult.R.get_ok
        ) >>= fun nvstate ->
        pad_str child_b key ;

        pack_untagged child_b nvstate nvalue >>= fun () ->
        let nv_length = get_nv_size ~name:key nvalue in
        Printf.eprintf "too much lenb %#x lenchildb %#x nvlen:%d(%#x) maybefix:%#x\n%!"
          (Buffer.length b) (Buffer.length child_b) nv_length nv_length (nv_length -8 + align8_total (String.length key));
        Buffer.add_int32_le b (nv_length |> Int32.of_int) ;
        Printf.eprintf "%#x: %s\n%!" (Buffer.length child_b)
          (hex @@ Buffer.contents child_b);
        Buffer.add_buffer b child_b ;
        pairs nvstate next
    in pairs o_nvstate nvl
  in
  pack_native b first_nvstate nv >>| fun () ->
  (Buffer.contents b)

let fnvlist_unpack s =
  let open Bytes in
  let b = of_string s
  (* we don't mutate it, but we need the get_x_le functions*) in
  let [@inline] ensure_atleast ~off size =
    if size <= Bytes.length b - off then Ok ()
    else Error "input too short"
  in

  let read_str_unaligned ~len off =
    assert (len >= 0) ;
    ensure_atleast ~off 1 >>= fun () ->
    begin match Bytes.index_from_opt b off '\x00' with
      | Some null_idx when null_idx > off+len ->
        Error "no nullbyte in string"
      | None -> Error "no nullbyte in string"
      | Some null_idx ->
        let s = Bytes.sub_string b off (null_idx-off) in
        Fmt.pr "len:%d off:%d null_idx:%d s:%S\n%!" len off null_idx s;
        Ok (1 + null_idx, s)
    end
  in
  let read_str ~len (off:int) =
    read_str_unaligned ~len off >>| fun (noff, s) ->
    off + align8_total (noff-off), s
  in

  (if Bytes.length b >= 1+1+1+1 (* header *)
                        + 4 (* version *)
                        + 4 (* endian check *)
                        + 4 (* first appendix length *)
   then Ok () else
     Error "minimal size for nvlist is 16") >>= fun () ->
  (* nvs_header.nvh_encoding: first byte determines type *)
  let is_native_encoding = 0 = get_int8 b 0 in
  (if is_native_encoding then Ok () else (* as opposed to e.g. XDR *)
    Error "nvlist is not using the 'native encoding'; we only implement that"
  ) >>= fun () ->

  (* nvs_header.nvh_endian *)
  (if 1 = get_int8 b 1 then Ok () else
     Error "nvlist is not little-endian") >>= fun () ->

  (* nvs_header_t.nvh_reserved1
   * nvs_header_t.nvh_reserved2 *)
  (* libnvpair:fnvlist_unpack doesn't care:
  (if 0 = get_int16_ne b 2 then Ok () else
     Error "nvlist has reserved bytes set") >>= fun () ->
*)

  (* nvs_operation() -> nvs_native_nvlist(): *)

  (* libnvpair:fnvlist_unpack doesn't care:
  (if 0l = get_int32_ne b 4 then Ok () else
     Error "nvl_version <> 0") >>= fun () ->
  *)
  let _nvl_version = get_int32_le b 4 in

  (* nvl_nvflag *)
  let nvstate = Nvl_state.add_flags
      (get_int32_le b 8 |> nvflag_of_int32) Nvl_state.empty in

  let rec loop (nvstate:Nvl_state.t)
      (acc:nvl) (off:int) : (int * nvl, string) result =
    ensure_atleast ~off 4 >>= fun () ->
    let begin_off = off in
    let appendix = Bytes.get_int32_le b off in
    let off = off + 4 in
    if appendix = 0l then begin
      Fmt.pr "off: %d appendix zero\n%!" off ;
      Ok (off, acc)
    end else begin
      (if appendix < 0l
       then Error "negative appendix"
       else Ok()) >>= fun () ->
      ensure_atleast ~off (Int32.to_int appendix) >>= fun () ->
      let old_off = off in
      ensure_atleast ~off (4+4+4) >>= fun () ->
      let el_namesz = Bytes.get_uint16_le b off in
      let off =
        (* the int16_t is follow by two reserved bytes *)
        off + 2 + 2 in
      let el_count = Bytes.get_int32_le b off |> Int32.to_int in
      let off = off + 4 in
      el_typ_of_int32 (Bytes.get_int32_le b off) >>= fun el_typ ->
      let off = off + 4 in
      begin match el_typ, el_count with
        | T_Boolean, 0 -> Ok ()
        | (T_BooleanValue | T_Int8 | T_Uint8 | T_HrTime | T_Double
          |T_Byte|T_Int16|T_Uint16|T_Int32|T_Uint32|T_Int64|
           T_Uint64|T_Str|T_Nvlist), 1 -> Ok ()
        | (T_Boolean|T_BooleanValue | T_Int8 | T_Uint8 | T_HrTime | T_Double|
           T_Byte|T_Int16|T_Uint16|T_Int32|T_Uint32|T_Int64|
           T_Uint64|T_Str|T_Nvlist), _ -> Error "scalar type el_count <> 1l"
        | (T_StrArray | T_BooleanArray |
           T_ByteArray|T_Int16Array|T_Uint16Array|T_Int32Array|
           T_Uint32Array|T_Int64Array|T_Uint64Array|T_NvlistArray|
           T_Int8Array|T_Uint8Array), n ->
          if 0 <= n && n <= 0x10000
          then Ok ()
          else Error "Array type: 0 <= el_count <= 65536 not true"
        | _ -> .
      end >>= fun () ->
      read_str ~len:el_namesz off >>= fun (off, el_name) ->
      (if String.length el_name <> el_namesz -1
       then Error "nvlist namesz incorrect"
       else Ok ()) >>= fun () ->
      Nvl_state.add_name nvstate el_name el_typ >>= fun nvstate ->
      begin match el_typ with
        | T_Boolean ->
          Ok (off, Boolean)
        | T_BooleanValue ->
          ensure_atleast ~off 8 >>= fun () ->
          let b = Bytes.get_int32_le b off in
          begin match b with
            | 0_l -> Ok false
            | 1_l -> Ok true
            | _ -> Error "BooleanValue must be 0 or 1"
          end >>| fun bool ->
          (off+8, BooleanValue bool)
        | T_Byte ->
          ensure_atleast ~off 8 >>| fun () ->
          (off+8, Byte (Bytes.get b off))
        | T_Int8 ->
          ensure_atleast ~off 8 >>| fun () ->
          (off+8, Int8 (Bytes.get_int8 b off))
        | T_Uint8 ->
          ensure_atleast ~off 8 >>| fun () ->
          (off+8, Uint8 (Bytes.get_uint8 b off))
        | T_Int16 ->
          ensure_atleast ~off 8 >>| fun () ->
          (off+8, Int16 (Bytes.get_int16_le b off))
        | T_Uint16 ->
          ensure_atleast ~off 8 >>| fun () ->
          (off+8, Uint16 (Bytes.get_int16_le b off))
        | T_Int32 ->
           ensure_atleast ~off 8 >>| fun () ->
           (off+8, Int32 (Bytes.get_int32_le b off))
        | T_Uint32 ->
          ensure_atleast ~off 8 >>| fun () ->
          (off+8, Uint32 (Bytes.get_int32_le b off))
        | T_Int64 ->
          ensure_atleast ~off 8 >>| fun () ->
          (off+8, Int64 (Bytes.get_int64_le b off))
        | T_Uint64 ->
          ensure_atleast ~off 8 >>| fun () ->
          (off+8, Uint64 (Bytes.get_int64_le b off))
        | T_Str ->
          ensure_atleast ~off 1 >>= fun () ->
          read_str ~len:(Int32.to_int appendix) off >>| fun (off, s) ->
          off, Str s
        | T_ByteArray ->
          ensure_atleast ~off (align8_total el_count) >>= fun () ->
          Ok (off + align8_total el_count,
              ByteArray (Bytes.sub_string b off el_count))
        | T_Int16Array ->
          let consumed = align8_total (el_count * 2) in
          ensure_atleast ~off consumed >>= fun () ->
          let lst = ref [] in
          for idx = el_count - 1 downto 0 do
            lst := Bytes.get_int16_le b (off+idx*2) :: !lst
          done ;
          Ok (off + consumed, Int16Array !lst)
        | T_Uint16Array ->
          let consumed = align8_total (el_count * 2) in
          ensure_atleast ~off consumed >>= fun () ->
          let lst = ref [] in
          for idx = el_count - 1 downto 0 do
            lst := Bytes.get_uint16_le b (off+idx*2) :: !lst
          done ;
          Ok (off + consumed, Uint16Array !lst)
        | T_Int32Array ->
          let consumed = align8_total (el_count * 4) in
          ensure_atleast ~off consumed >>= fun () ->
          let lst = ref [] in
          for idx = el_count - 1 downto 0 do
            lst := Bytes.get_int32_le b (off+idx*4) :: !lst
          done ;
          Ok (off + consumed, Int32Array !lst)
        | T_Uint32Array ->
          let consumed = align8_total (el_count * 4) in
          ensure_atleast ~off consumed >>= fun () ->
          let lst = ref [] in
          for idx = el_count - 1 downto 0 do
            lst := Bytes.get_int32_le b (off+idx*4) :: !lst
          done ;
          Ok (off + consumed, Uint32Array !lst)
        | T_Int64Array
        | T_Uint64Array ->
          let consumed = el_count * 8 in
          ensure_atleast ~off consumed >>= fun () ->
          let lst = ref [] in
          for idx = el_count - 1 downto 0 do
            lst := Bytes.get_int64_le b (off+idx*8) :: !lst
          done ;
          Ok (off + consumed,
          if T_Int64Array = el_typ then Int64Array !lst else Uint64Array !lst)
        | T_Int8Array ->
          let consumed = align8_total (el_count * 1) in
          ensure_atleast ~off consumed >>= fun () ->
          let lst = ref [] in
          for idx = el_count - 1 downto 0 do
            lst := Bytes.get_int8 b (off+idx*1) :: !lst
          done ;
          Ok (off + consumed, Int8Array !lst)
        | T_Uint8Array ->
          let consumed = align8_total ( el_count * 1) in
          ensure_atleast ~off consumed >>= fun () ->
          let lst = ref [] in
          for idx = el_count - 1 downto 0 do
            lst := Bytes.get_uint8 b (off+idx*1) :: !lst
          done ;
          Ok (off + consumed, Uint8Array !lst)
        | T_BooleanArray ->
          let consumed = align8_total ( el_count * 4) in
          ensure_atleast ~off consumed >>= fun () ->
          let rec aloop acc = function
            | -1 -> Ok (BooleanArray acc)
            | idx ->
              begin match Bytes.get_int32_le b (off+idx*4) with
              | 0_l -> Ok false
              | 1_l -> Ok true
              | _ -> Error "BooleanValue must be 0 or 1"
              end >>= fun bool ->
              aloop (bool::acc) (pred idx)
          in aloop [] (el_count-1) >>| fun acc ->
          (off + consumed, acc)
        | T_Nvlist -> (* 19l *)
          read_nvlist_header b off >>= fun (off, nvflag) ->
          let child_state = Nvl_state.descend_child nvstate nvflag in
          loop child_state [] off >>| fun (off, nvl) ->
          off, Nvlist nvl
        | T_NvlistArray -> (* 20_l NvlistArray *)
          let headers_off = off + el_count*8 in
          let elt_off = headers_off + el_count*24 in
          let res = ref (Ok (elt_off, [])) in
          for el_idx = 0 to el_count -1 do
            res := begin
              !res >>= fun (elt_off, acc) ->
              ensure_atleast ~off:(off+el_idx*8) 8 >>= fun () ->
              let zero = Bytes.get_int64_le b (off + el_idx*8) in
              (if zero = 0_L then Ok () (* sizeof(uint64_t) padding *)
               else Error "nvlistarray padding not zero") >>= fun () ->
              read_nvlist_header b (headers_off + el_idx*24)
              >>= fun (_ , nvflag) ->
              let child_state = Nvl_state.descend_child nvstate nvflag in
              loop child_state [] elt_off >>= fun (n_elt_off, nvl) ->
              assert (n_elt_off > elt_off); (* ought to advance *)
              Ok (n_elt_off, nvl::acc)
            end
          done ;
          !res >>| fun (elt_off, acc) ->
          (elt_off, NvlistArray (List.rev acc))
        | T_StrArray ->
            (* TODO not sure if libnvlist uses these 24 for anything: *)
          if el_count = 0 then Ok (off, StrArray [])
          else begin
            ensure_atleast ~off (el_count * 8) >>= fun () ->
            let off = off + (el_count * 8) in
            let rec loop off acc = function
              | 0 -> Ok (off, StrArray (List.rev acc))
              | idx ->
                read_str_unaligned
                  ~len:(Int32.to_int appendix) off >>= fun (off, str) ->
              loop off (str::acc) (pred idx)
            in loop off [] el_count
            >>| fun (noff, acc) -> (off + align8_total (noff-off), acc)
          end
        | T_HrTime ->
          ensure_atleast ~off 8 >>= fun () ->
          Ok (off+8, HrTime (Bytes.get_int64_le b off))
        | T_Double ->
          (* Int64.float_of_bits , Int64.bits_of_float let header =
            let z = Marshal.to_string 0.0 [] in
            String.sub z (String.length z-8) 8 in
          let f : float = Marshal.from_string
              (header ^ Bytes.sub_string b off 8) 0 in*)
          ensure_atleast ~off 8 >>= fun () ->
          Logs.warn (fun m -> m "type Double in input; not supported");
          Ok (off+8, Double)
        | _ -> .
      end >>= fun (off, el_val) ->
      Fmt.pr "begin_off: %d old_off %d off %d appendix %ld %u:%S elcount:%ud %ld => %a\n%!"
        begin_off old_off off appendix el_namesz el_name el_count (typeof el_val)
        pp_value el_val ;
      Fmt.pr "appendix: %ld expected: %ld\n%!" appendix
        (off - begin_off |> Int32.of_int);
      begin match el_val with
        | Nvlist _
        | NvlistArray (_::_) -> Ok ()
        (*assert (Int32.to_int appendix + 4 <= off-begin_off)*)
        | _ ->
          if (Int32.to_int appendix = off-begin_off) then
            Ok () else
            Error (Fmt.str "bad appendix length")
      end >>= fun () ->
      (if old_off <= off
       then Ok () (* assertion to avoid infinite loops *)
       else Error "fnvlist_unpack loop failed to advance") >>= fun () ->
      let sz = get_nv_size el_name el_val  in
      begin
        Fmt.pr "calculated sz: %d appendix: %ld el_namesz: %u\n%!"
          sz appendix el_namesz ;
        if sz <> Int32.to_int appendix then
          Error "decoded length does not match"
        else Ok ()
      end >>= fun () ->
      begin
        (* avoid going backwards: *)
        if Int32.to_int appendix + begin_off > off
        then
          Error (Fmt.str "didnt consume appendix: %d + %d <> %d"
                   (Int32.to_int appendix) old_off off)
        else Ok ()
     end >>= fun () ->

      loop nvstate (acc @ [el_name, el_val]) off
    end
  in
  loop nvstate [] 12 >>= fun (off, ret) ->
  (* Ensure we didn't bail early:
     (fnvlist_unpack) just returns the consumed length, it does not
     throw an error if the input size is longer than needed *)
  (*
  if (off = String.length s) then Ok ret
  else Error (Fmt.str "nvlist parser early exit %d/%d: %a"
                off (String.length s) pp ret)
     *)
  Ok ret


let to_c t =
  let b = Buffer.create 1008 in
  Buffer.add_string b
{|
/*
gcc -std=c18 -Wall -pie  -I/usr/include/libzfs/ yo.c -lnvpair -luutil -o yo && ./yo

*/
#include <stdlib.h>
#include <assert.h>
#include <stdio.h>
#include <stdint.h>
#include <stddef.h>
#include <stdarg.h>
#include <strings.h>
typedef unsigned int uint_t;
typedef int boolean_t;
typedef unsigned char uchar_t;
typedef uint64_t hrtime_t;
typedef char * const* string_t;
#include <libzfs/sys/nvpair.h>
#include <libzfs/libnvpair.h>

int
main(int argc, char **argv)
{
  char *outbuf = NULL;
  nvlist_t *nv = NULL;
  size_t nv_sz = 0;

  nv = fnvlist_alloc();
|} ;
  let gen = ref 0 in
  let rec loop ~parent =
    let cfun fn k f =
      Buffer.add_string b
        {|  fnvlist_add_|};
      Buffer.add_string b fn ;
      Buffer.add_string b "("; Buffer.add_string b parent ;
      Buffer.add_string b {|, "|} ;
      Buffer.add_string b k ;
      Buffer.add_string b {|", |} ;
      f () ;
      Buffer.add_string b ");\n"
    in
    let mkarray (type el) ~k ~c_typ ~c_fun
        (lst:el list) (to_str:el -> string) =
      let this = c_typ ^ "arr_" ^ string_of_int !gen in
      incr gen ;
      let lst_dim = string_of_int (List.length lst) in
      Buffer.add_string b @@ {|
  {
        |} ^ c_typ ^" "^ this
                             ^ "[" ^ lst_dim ^ "] = {";
      List.iter (fun i ->
          Buffer.add_string b (to_str i);
          Buffer.add_char b ','
        ) lst;
      Buffer.add_string b "};\n" ;
      cfun c_fun k (fun () ->
          Buffer.add_string b this ;
          Buffer.add_char b ',';
          Buffer.add_string b lst_dim;
        );
      Buffer.add_string b "\n}\n";
    in
    function
    | [] -> ()
    | (k, Int8Array v) :: next ->
      mkarray ~k ~c_typ:"int8_t" ~c_fun:"int8_array"
        v string_of_int ;
      loop ~parent next
    | (k, Uint8Array v) :: next ->
      mkarray ~k ~c_typ:"uint8_t" ~c_fun:"uint8_array"
        v string_of_int ;
    | (k, BooleanArray v) :: next ->
      mkarray ~k ~c_typ:"boolean_t" ~c_fun:"boolean_array"
        v (fun v -> Bool.to_int v |> string_of_int);
      loop ~parent next
    | (k, Int16Array v) :: next ->
      mkarray ~k ~c_typ:"int16_t" ~c_fun:"int16_array"
        v string_of_int ;
      loop ~parent next
    | (k, Uint16Array v) :: next ->
      mkarray ~k ~c_typ:"uint16_t" ~c_fun:"uint16_array"
        v string_of_int ;
      loop ~parent next
    | (k, Int32Array v) :: next ->
      mkarray ~k ~c_typ:"uint32_t" ~c_fun:"int32_array"
        v (Fmt.str "%ld") ;
      loop ~parent next
    | (k, Uint32Array v) :: next ->
      mkarray ~k ~c_typ:"uint32_t" ~c_fun:"uint32_array"
        v (Fmt.str "%lu") ;
      loop ~parent next
    | (k, Int64Array v) :: next ->
      mkarray ~k ~c_typ:"int64_t" ~c_fun:"int64_array"
        v (Fmt.str "%Ld") ;
      loop ~parent next
    | (k, Uint64Array v) :: next ->
      mkarray ~k ~c_typ:"uint64_t" ~c_fun:"uint64_array"
        v (Fmt.str "%Lu") ;
      loop ~parent next
    | (k,ByteArray v) :: next ->
      let v = (String.to_seq v |> List.of_seq) in
      mkarray ~k ~c_typ:"uchar_t" ~c_fun:"byte_array"
        v (fun ch -> Char.code ch |> string_of_int) ;
      loop ~parent next
    | (k, StrArray vec)::next ->
      mkarray ~k ~c_typ:"string_t" ~c_fun:"string_array"
        vec (fun s ->
            let ib = Buffer.create 10 in
            Buffer.add_char ib '"' ;
            ( String.to_seq s |> Seq.iter (fun ch ->
                  Buffer.add_string ib (Fmt.str "\\x%x" (Char.code ch));
                )
            ) ;
            Buffer.add_char ib '"';
            Buffer.contents ib
          ) ;
      loop ~parent next
    | (k, Str v) :: next ->
      cfun "string" k (fun () ->
          Buffer.add_char b '"';
          Buffer.add_string b v;
           Buffer.add_char b '"';
        ) ;
      loop ~parent next
    | (k, Boolean) :: next ->
      cfun "boolean" k (fun () -> ())
    | (k, BooleanValue v) :: next ->
      cfun "boolean_value" k (fun () ->
          Buffer.add_string b (Bool.to_int v |> string_of_int))
    ; loop ~parent next
    | (k, Int8 v) :: next ->
      cfun "int8" k (fun () -> Buffer.add_string b (Int.to_string v))
    ; loop ~parent next
    | (k, Uint8 v) :: next ->
      cfun "uint8" k (fun () -> Buffer.add_string b (Int.to_string v))
    ; loop ~parent next
    | (k, Int16 v) :: next ->
      cfun "int16" k (fun () -> Buffer.add_string b (Int.to_string v))
    ; loop ~parent next
    | (k, Uint16 v) :: next ->
      cfun "uint16" k (fun () -> Buffer.add_string b (Fmt.str "%u" v))
    ; loop ~parent next
    | (k,Int32 v) :: next ->
      cfun "int32" k (fun () -> Buffer.add_string b (Int32.to_string v))
    ; loop ~parent next
    | (k,HrTime n) :: next ->
      cfun "hrtime" k (fun () -> Buffer.add_string b (Int64.to_string n))
    ; loop ~parent next
    | (k,Uint32 v) :: next ->
      cfun "uint32" k (fun () -> Buffer.add_string b (Fmt.str "%lu" v))
    ; loop ~parent next
    | (k,Int64 v) :: next ->
      cfun "int64" k (fun () -> Buffer.add_string b (Int64.to_string v))
    ; loop ~parent next
    | (k,Uint64 v) :: next ->
      cfun "uint64" k (fun () -> Buffer.add_string b (Int64.to_string v))
    ; loop ~parent next
    | (key, Nvlist nvl)::next ->
      let this = "nv_" ^ (string_of_int !gen) in
      incr gen ;
      Buffer.add_string b "{\n";
      Buffer.add_string b @@ {|
        nvlist_t *|} ^ this ^ {| = fnvlist_alloc();
        // ... build nvl as this
|} ;
      loop ~parent:this nvl ;
      cfun "nvlist" key (fun () -> Buffer.add_string b this);
      Buffer.add_string b @@ "fnvlist_free(" ^ this ^ {|);
}
|} ; loop ~parent next
    | (key, Byte byte)::next ->
      cfun "byte" key (fun() ->
          Buffer.add_string b (Fmt.str "%C" byte);
        );
      loop ~parent next
    | (key, NvlistArray vec)::next ->
      let this = "nv_arr" ^ string_of_int !gen in
      incr gen;
      Buffer.add_string b @@ {|
      nvlist_t *|} ^ this ^ "[" ^ (List.length vec|>string_of_int);
      Buffer.add_string b "] = {0};\n";
      List.iteri (fun idx elt ->
          Buffer.add_string b this ;
          Buffer.add_char b '[';
Buffer.add_string b (string_of_int idx);
          Buffer.add_string b "] = fnvlist_alloc();\n";
          loop ~parent:(this^"["^(string_of_int idx)^"]") elt
        ) vec;
      cfun "nvlist_array" key (fun() ->
          Buffer.add_string b this ;
          Buffer.add_string b ", ";
          Buffer.add_string b (List.length vec|>string_of_int)
        );
      loop ~parent next
    | (_, Double)::_-> failwith "TODO Double not implemented"
    | _ -> .
  in loop ~parent:"nv" t ;
  Buffer.add_string b
{|
  outbuf = fnvlist_pack(nv, &nv_sz);
  fnvlist_free(nv);

  nvlist_t *check = fnvlist_unpack(outbuf, nv_sz);
  nvlist_print_json(stderr, check);
  fnvlist_free(check);

  printf("(* nvlist size: %zd *)\n", nv_sz);
  printf("let v =\n");
	printf("   \"");
	for (size_t i = 0; i < nv_sz; i++) {
		printf("%02hhx", outbuf[i]);
		if (i && (i % 4) == 3) {
			printf("\"");
			if (i % 40 == 3+4*5) printf("\n");
			printf(" ^ \"");
		}
	}
	printf("\" in\n");
  printf("let exp =|} ;
    Stdlib.Format.asprintf "%a" pp t
  |> String.split_on_char '"'
  |> String.concat {|\"|}
  |> String.split_on_char '\n'
  |> String.concat ""
  |> String.trim
  |> Buffer.add_string b  ;
  Buffer.add_string b
  {|");
  printf("in\n");
  printf("(fun() ->\n");
  printf("  Alcotest.(check @@ result hexcheck string)\n");
  printf("    \"c_test\"\n");
  printf("    (Ok (Nvlist.unhex v))\n");
  printf("    (Nvlist.fnvlist_pack exp)\n");
  printf("),\n");
  printf("(fun() ->\n");
  printf("  Alcotest.(check @@ result nvlist string)\n");
  printf("    \"c_test_decode\"");
  printf("    (Ok exp)");
  printf("    (Nvlist.fnvlist_unpack @@ Nvlist.unhex v)");
  printf(")\n");
  free(outbuf);
  fflush(stdout);
  return 0;
}
|} ;
  Buffer.to_bytes b |> Bytes.to_string

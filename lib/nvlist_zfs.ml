open Nvlist
open Rresult.R

module Fletcher4 = struct
  type t = {
    a: Int64.t ;
    b: Int64.t ;
    c: Int64.t ;
    d: Int64.t ;
  }
  let empty = { a = 0L ; b = 0L ; c = 0L ; d = 0L }
  let get_uint32_le b o =
    (* gee thanks for including get_uint32_le ocaml stdlib *)
    match Bytes.get_int32_le b o with
      | n when n >= 0l -> Int64.of_int32 n
      | n -> Int64.logor
               (Int64.shift_left
                  (Int64.of_int32 (Int32.shift_right_logical n 1)) 1)
               (Int64.of_int32 (Int32.logand n 1l))

  let to_string_le {a;b;c;d} =
    let buf = Bytes.create (4*8) in
    Bytes.set_int64_le buf 0  a ;
    Bytes.set_int64_le buf 8  b ;
    Bytes.set_int64_le buf 16 c ;
    Bytes.set_int64_le buf 24 d ;
    Bytes.to_string buf

  let varsize old_t buf off len =
    assert (off mod 4 = 0); (* TODO can't remember how this is implemented in ZFS *)
    assert (len mod 4 = 0); (* since this works on 32bit-aligned packed nvlist*)
    let ipend = off + (len/4)*4 in
    let ip = ref off in
    let t = ref old_t in
    let buf = Bytes.of_string buf in
    while !ip < ipend do begin
      let (+) = Int64.add in
      t := {!t with a = !t.a + get_uint32_le buf !ip} ;
      t := {!t with b = !t.b + !t.a};
      t := {!t with c = !t.c + !t.b};
      t := {!t with d = !t.d + !t.c };
      ip := Int.add !ip 4 ;
    end done ;
    !t

  let truncated_fletcher4 data =
    (* The fletcher variant used for checksumming resumetok:
       fletcher4, except only look at 32-bit words (so the end is not
       checksummed if not padded to word-alignment).
       only the first 64bits of the checksum is used, so that's all we keep.
    *)
    let yo = ref 0L in
    String.iteri (fun i ch ->
        yo := Int64.add !yo @@ Int64.of_int @@
          (Char.code ch lsl (8*(i mod 4))))
      (String.sub data 0 ((String.length data lsr 2) lsl 2)); !yo
end


module Resumetoken = struct

(*
apt install libz-dev
https://github.com/ayang64/nvlist/blob/master/type.go

the code that prepares a resumetok:
https://github.com/openzfs/zfs/blob/5a42ef04fd390dc96fbbf31bc9f3d05695998211/module/zfs/dsl_dataset.c#L2428-L2442

the code that sends stuff given a resumetok:
https://github.com/openzfs/zfs/blob/a1ba12092744ce4da5ffb8061223cc61c9b8b4f8/lib/libzfs/libzfs_sendrecv.c#L1606

code that parses a resumetok: zfs_send_resume_token_to_nvlist(
https://github.com/openzfs/zfs/blob/a1ba12092744ce4da5ffb8061223cc61c9b8b4f8/lib/libzfs/libzfs_sendrecv.c#L1350


TODO check if nvstring can be > 255 bytes; if it can we have a buffer overflow, yay
https://github.com/openzfs/zfs/blob/a1ba12092744ce4da5ffb8061223cc61c9b8b4f8/lib/libzfs/libzfs_sendrecv.c#L1655


 *   <token version>-<checksum of payload>-<uncompressed payload length>
*)

  let unpack s =
    (** unpacks the textual string given by
        zfs get resumetok foo/bar *)
    (* ZFS_SEND_RESUME_TOKEN_VERSION
     * cksum.zc_word[0]
     * packed_size
     * str *)
    try Ok (
        Scanf.sscanf
          s "%u-%Lx-%Lx-%s"
          (fun version zc_word packed_size str ->
             version, zc_word, packed_size, str))
    with (* TODO check which exceptions this is supposed to throw *)
      _ -> Error "unpack: Failed to unpack token"

  let parse resumetok =
    unpack resumetok >>= fun (
      version, checksum,
      payload_len, (* payload len is the length of
                        decompress(unhex(hexdata)) *)
      hexdata) ->
    let data = unhex hexdata in
    let own_cksum = Fletcher4.truncated_fletcher4 data in
    ( if checksum <> own_cksum then
        Error (Fmt.str "checksum mismatch %Lx <> %Lx" checksum own_cksum) else Ok ()
    ) >>= fun () ->
    (Ezgzip.Z.decompress ~header:true data
     |> reword_error (function `Zlib (Ezgzip.Z.Truncated x
                                     | Compression_error x) -> x))
    >>= fun decompressed ->
    let expected_len = Int64.of_int @@ String.length decompressed in
    (if payload_len <> expected_len
     then
       Error (Fmt.str "compressed payload is truncated (%Ld <> %Ld)" payload_len expected_len)
     else Ok ()) >>= fun () ->
    fnvlist_unpack decompressed >>= fun nvval ->
    Format.fprintf Format.std_formatter "-> %a" Nvlist.pp_nvl (nvval) ;
    Fmt.pr "orig:\n%S\n" decompressed ;
    Fmt.pr "orig:\n%S\n" (hex decompressed) ;
    Ok nvval

  let create ~obj ~offset ~bytes ~toguid ~toname =
    let nv = [
        ("object", Uint64 obj) ;
        ("offset", Uint64 offset) ;
        ("bytes",  Uint64 bytes) ;
        ("toguid", Uint64 toguid) ;
        ("toname", Str toname) ;
      ] in
    fnvlist_pack nv >>= fun packed ->
    let compressed = Ezgzip.Z.compress ~level:9 ~header:true packed in
    let cksum = Fletcher4.truncated_fletcher4 compressed in
    let token =
      Printf.sprintf "1-%Lx-%Lx-%s"
        cksum (*(String.sub (Printf.sprintf "%#018Lx" cksum) 2 16)*)
        (String.length packed |> Int64.of_int)
        (hex compressed)
    in
    Ok token

end

let trim_asciiz bytes =
  match Bytes.index_from_opt bytes 0 '\x00' with
  | Some null_idx ->
    Bytes.sub_string bytes 0 null_idx
  | None -> assert false

module DRR = struct
  (*
   * This is still in a mockup state, but deals with reading
   * zfs send-streams
   *)
  type drr_type =
      DRR_BEGIN | DRR_OBJECT | DRR_FREEOBJECTS |
      DRR_WRITE | DRR_FREE | DRR_END | DRR_WRITE_BYREF |
      DRR_SPILL | DRR_WRITE_EMBEDDED | DRR_OBJECT_RANGE | DRR_REDACT |
      DRR_NUMTYPES
  let int_of_drr_type = function
    | DRR_BEGIN -> 0
    | DRR_OBJECT -> 1
    | DRR_FREEOBJECTS -> 2
    | DRR_WRITE -> 3 | DRR_FREE -> 4 | DRR_END -> 5
    | DRR_WRITE_BYREF -> 6
    | DRR_SPILL -> 7 | DRR_WRITE_EMBEDDED -> 8
    | DRR_OBJECT_RANGE -> 9 | DRR_REDACT -> 10
    | DRR_NUMTYPES -> 11
  let drr_type_of_int32 = function
    | 0l -> DRR_BEGIN
    | 1l -> DRR_OBJECT
    | 2l -> DRR_FREEOBJECTS
    | 3l -> DRR_WRITE | 4l -> DRR_FREE | 5l -> DRR_END
    | 6l -> DRR_WRITE_BYREF | 7l -> DRR_SPILL | 8l -> DRR_WRITE_EMBEDDED
    | 9l -> DRR_OBJECT_RANGE | 10l -> DRR_REDACT | 11l -> DRR_NUMTYPES
    | _ -> raise (Invalid_argument "incorrect drr typ > 11")
  let pp_drr_type ppf v =
    Fmt.pf ppf ((match v with
        | DRR_BEGIN -> "BEGIN" | DRR_OBJECT -> "OBJECT"
        | DRR_FREEOBJECTS -> "FREEOBJECTS" | DRR_WRITE -> "WRITE"
        | DRR_FREE -> "FREE" | DRR_END -> "END"
        | DRR_WRITE_BYREF -> "WRITE_BYREF" | DRR_SPILL -> "SPILL"
        | DRR_WRITE_EMBEDDED -> "WRITE_EMBEDDED"
        | DRR_OBJECT_RANGE -> "OBJECT_RANGE" | DRR_REDACT -> "REDACT"
        | DRR_NUMTYPES -> "NUMTYPES") ^^ "[%u]")
      (int_of_drr_type v)

  type dmu_object_type =
      DMU_OT_PLAIN_FILE_CONTENTS | DMU_OT_DIRECTORY_CONTENTS
    | DMU_OT_MASTER_NODE | DMU_OT_UNLINKED_SET
    | DMU_OT_ZVOL | DMU_OT_ZVOL_PROP | DMU_OT_POOL_PROPS | DMU_OT_SA
    | DMU_OT_SA_MASTER_NODE | DMU_OT_SA_ATTR_REGISTRATION
    | DMU_OT_SA_ATTR_LAYOUTS
    | DMU_OT_DEDUP
  let dmu_object_type_of_int32 = function
    | 19_l -> DMU_OT_PLAIN_FILE_CONTENTS
    | 20_l -> DMU_OT_DIRECTORY_CONTENTS
    | 21_l -> DMU_OT_MASTER_NODE
    | 22_l -> DMU_OT_UNLINKED_SET
    | 23_l -> DMU_OT_ZVOL
    | 24_l -> DMU_OT_ZVOL_PROP
    | 31_l -> DMU_OT_POOL_PROPS
    | 44_l -> DMU_OT_SA
    | 45_l -> DMU_OT_SA_MASTER_NODE
    | 46_l -> DMU_OT_SA_ATTR_REGISTRATION
    | 47_l -> DMU_OT_SA_ATTR_LAYOUTS
    | 49_l -> DMU_OT_DEDUP
  let pp_dmu_object_type ppf v =
    Fmt.pf ppf "%s" (match v with
        | DMU_OT_PLAIN_FILE_CONTENTS -> "DMU_OT_PLAIN_FILE_CONTENTS"
        | DMU_OT_DIRECTORY_CONTENTS -> "DMU_OT_DIRECTORY_CONTENTS"
        | DMU_OT_MASTER_NODE -> "DMU_OT_MASTER_NODE"
        | DMU_OT_UNLINKED_SET -> "DMU_OT_UNLINKED_SET"
        | DMU_OT_ZVOL -> "DMU_OT_ZVOL"
        | DMU_OT_ZVOL_PROP -> "DMU_OT_ZVOL_PROP"
        | DMU_OT_POOL_PROPS -> "DMU_OT_POOL_PROPS"
        | DMU_OT_SA -> "DMU_OT_SA"
        | DMU_OT_SA_MASTER_NODE -> "DMU_OT_SA_MASTER_NODE"
        | DMU_OT_SA_ATTR_REGISTRATION -> "DMU_OT_SA_ATTR_REGISTRATION"
        | DMU_OT_SA_ATTR_LAYOUTS -> "DMU_OT_SA_ATTR_LAYOUTS"
        | DMU_OT_DEDUP -> "DMU_OT_DEDUP"
      )
  let decode_drr_begin s =
    let drr_magic = Bytes.get_int64_le s 0 in
    assert (drr_magic = 0x2F5bacbac_L);
    (* should be DMU_BACKUP_MAGIC ?
       can it assume other values?
       https://github.com/openzfs/zfs/blob/master/lib/libzfs/libzfs_sendrecv.c#L2079
       looks like this is used to detect byteswap requirements
    *)
    let drr_versioninfo =
      (* see DMU_SET_FEATUREFLAGS *)
      Bytes.get_int64_le s 8 in
    let drr_creationtime = Bytes.get_int64_le s 16 in
    let drr_type =
      (* ? DMU_COMPOUNDSTREAM ? *)
      Bytes.get_int64_le s 24 in
    let drr_flags =
      (* | DRR_FLAG_CLONE | DRR_FLAG_CI_DATA
         | DRR_FLAG_FREERECORDS | DRR_FLAG_SPILL_BLOCK | ... *)
      Bytes.get_int32_le s 28 in
    let drr_toguid = Bytes.get_int64_le s 32 in
    let drr_fromguid = Bytes.get_int64_le s 40 in
    let drr_name = Bytes.sub s 48 256 |> trim_asciiz in
    (* drr_name will be padded with 0x00 *)
    Logs.app (fun m -> m "drr_magic: %Lu" drr_magic);
    Logs.app (fun m -> m "drr_versioninfo: %Lu" drr_versioninfo);
    Logs.app (fun m -> m "drr_creationtime: %Lu" drr_creationtime);
    Logs.app (fun m -> m "drr_type: %Lu" drr_type);
    Logs.app (fun m -> m "drr_flags: %lu" drr_flags);
    Logs.app (fun m -> m "drr_fromguid: %Lu" drr_fromguid);
    Logs.app (fun m -> m "drr_toguid: %Lu" drr_toguid);
    Logs.app (fun m -> m "drr_name: %S" drr_name);
    ()
  let decode_drr_object s =
    let drr_object = Bytes.get_int64_le s 0 in
    let drr_type =
      (* https://github.com/openzfs/zfs/blob/afbc61792116c9599afcccfd61204f968401d06e/include/sys/dmu.h#L165-L269 *)
      Bytes.get_int32_le s 8 in
    let drr_bonustype = Bytes.get_int32_le s 12 in
    let drr_blksz = Bytes.get_int32_le s 16 in
    let drr_bonuslen = Bytes.get_int32_le s 20 in
    let drr_checksumtype = Bytes.get_int8 s 24 in
    let drr_compress = Bytes.get_int8 s 25 in
    let drr_dn_slots = Bytes.get_int8 s 26 in
    let drr_flags = Bytes.get_int8 s 27 in
    let drr_raw_bonuslen = Bytes.get_int32_le s 28 in
    let drr_toguid = Bytes.get_int64_le s 32 in
    let drr_indblkshift = Bytes.get_int8 s 40 in
    let drr_nlevels = Bytes.get_int8 s 41 in
    let drr_nblkptr = Bytes.get_int8 s 42 in
    let drr_pad = Bytes.sub_string s 43 5 in
    let drr_maxblkid = Bytes.get_int64_le s 48 in
    Logs.app (fun m ->
        m"drr_object: %Lu (drr_type: %s) cksumtyp:%u compress:%u"
          drr_object
          (try Fmt.str "%a" pp_dmu_object_type
                 (dmu_object_type_of_int32 drr_type)
           with _ -> Int32.to_string drr_type)
          drr_checksumtype drr_compress);
    Logs.app (fun m->m"bonuslen %lu rawbonuslen %lu toguid:%Lu" drr_bonuslen drr_raw_bonuslen drr_toguid);
    assert (drr_raw_bonuslen = 0l);
    Int32.to_int drr_bonuslen
  let decode_drr_write s =
    let drr_object = Bytes.get_int64_le s 0 in
    let drr_type = Bytes.get_int32_le s 8 in
    let drr_pad = Bytes.get_int32_le s 12 in
    let drr_offset = Bytes.get_int64_le s 16 in
    let drr_logical_size = Bytes.get_int64_le s 24 in
    let drr_toguid = Bytes.get_int64_le s 32 in
    let drr_checksumtype = Bytes.get_int8 s 40 in
    (* https://github.com/openzfs/zfs/blob/269b5dadcfd1d5732cf763dddcd46009a332eae4/module/zfs/zio_checksum.c#L166
       2 = off 7 = fletcher4 ZCHECKSUM_FLAG_METADATA,
       ie if checksumtype=2 then there should just be zeroes on disk. *)
    let drr_flags = Bytes.get_int8 s 41 in
    let drr_compressiontype = Bytes.get_int8 s 42 in
    (* 5 bytes of padding*)
    (* https://github.com/openzfs/zfs/blob/60ffc1c460e4cdf3c3ca12c8840fd0675a98ed0d/include/sys/ddt.h#L67 *)
    let drr_key_cksum = Bytes.sub s 48 32 in
    let drr_key_prop = Bytes.get_int64_le s 80 in
    let drr_compressed_size = Bytes.get_int64_le s 88 in
    let _drr_salt = Bytes.get_int64_le s 96 in (* 16 bytes *)
    let _drr_iv = Bytes.get_int64_le s 112 in (* 16 bytes *)
    let _drr_mac = Bytes.get_int64_le s 128 in (* 16 bytes *)
    assert (128+16 = 144);
    Logs.app (fun m -> m "WRITE drr_object: %Lu (drr_type: %s) toguid:%Lu logicsize:%Lu compressedsize:%Lu cksumtyp:%l drr_pad %lu"
                 drr_object
                 (try Fmt.str "%a" pp_dmu_object_type
                        (dmu_object_type_of_int32 drr_type)
                  with _ -> Int32.to_string drr_type)
                 drr_toguid drr_logical_size
                 drr_compressed_size drr_checksumtype drr_pad);
    (* content follows *)
    (* 312-144-8 = 160 ; the rest of this record is used to carry the object
       contents, and logical/compressed size expresses how much *beyond* current
       object we need to read. *)
    let physical_size =
      (if drr_compressed_size > 0_L
       then drr_compressed_size else drr_logical_size)
      |> Int64.to_int
    in
    assert (drr_compressed_size = 0_L); (* not sure how to handle TODO *)
    let content = Bytes.sub_string s 304 physical_size
    in
    Logs.app(fun m -> m "content: %s\n(%Lu+312 < %d)"
                (Nvlist.hex content)
                (drr_logical_size) (Bytes.length s)
            );
    Int64.to_int drr_logical_size
  let decode_drr_free s =
    let drr_object = Bytes.get_int64_le s 0 in
    let drr_offset = Bytes.get_int64_le s 8 in
    let drr_length = Bytes.get_int64_le s 16 in
    let drr_toguid = Bytes.get_int64_le s 24 in
    Logs.app (fun m -> m "drr_object: %Lu offset:%Lu length:%Ld toguid:%Lu"
                 drr_object drr_offset drr_length drr_toguid)
  let parse_drr ~fletcher4 s =
    (* useful resources here:
       https://github.com/openzfs/zfs/blob/958826be7a3e17f29e1f5e114c76aa2ec3c8a490/include/sys/zfs_ioctl.h#L242
       streams consist of  dmu_replay_record structs, and optional "payloads" as determined by the drr_payloadlen member.
       see dump_record()
    *)
    Logs.app (fun m -> m "\nParsing drr 0x%s" @@ Nvlist.hex @@ Bytes.sub_string s 0 4);
    let drr_type =
      let i32 = Bytes.get_int32_le s 0 in
      drr_type_of_int32 i32
    in
    begin match drr_type with
    (* all record types except from DRR_BEGIN are checksummed with
       Fletcher 4 -- TODO check the checksum. *)
      | DRR_BEGIN -> 4
      | _ -> 312 end +
    begin match drr_type with
      | DRR_BEGIN (* 0 *) ->
        let sizeof_drr_begin = 8+8+8 +
                               8 (* dmu_objset_type_t *)
                               + 4+8+8
                               + 256 (*char[MAXNAMELEN]*) in
        decode_drr_begin (Bytes.sub s 8 sizeof_drr_begin);
        sizeof_drr_begin
      | DRR_OBJECT (* 1 *) ->
        decode_drr_object (Bytes.sub s 8 56)
      | DRR_FREEOBJECTS (* 2 *) ->
        0 (*8+8+8*)
      | DRR_WRITE (* 3 *) ->
        decode_drr_write (Bytes.sub s 8 (Bytes.length s -8))
      | DRR_FREE (* 4 *)->
        decode_drr_free (Bytes.sub s 8 32) ; 0
      | DRR_END (* 5 *)->
        let drr_cksum = Bytes.sub_string s 8 32 in
        let drr_toguid = Bytes.get_int64_le s 40 in
        Logs.app (fun m -> m"END: drr_cksum:%s toguid:%Lu\n   cksum comp: %s"
                     (Nvlist.hex drr_cksum) drr_toguid
                     (Nvlist.hex @@ Fletcher4.to_string_le fletcher4)
                 );
        0
      | _ -> failwith (Fmt.str "not implemented: drr type %u" @@ int_of_drr_type drr_type)
    end
    |> fun drr_size ->
    let drr_payloadlen = Bytes.get_int32_le s 4 in
    assert (drr_payloadlen >= 0l);
    assert (Int32.to_int drr_payloadlen >= 0);
    assert (Int32.of_int (Int32.to_int drr_payloadlen) = drr_payloadlen);
    assert (drr_payloadlen = 0l);
    (* TODO not sure what to do with this.
       looks like it's meant to hold XDR-encoded values if "gatherprops" is true.
       needs more investigation.
       https://github.com/openzfs/zfs/blob/master/lib/libzfs/libzfs_sendrecv.c#L2029-L2065
*)
    let drr_payloadlen = Int32.to_int drr_payloadlen in
    let drr_payload = Bytes.sub s 8 drr_payloadlen in
    Logs.app (fun m -> m "%a payload: (%u) %S"
                 pp_drr_type drr_type
                 drr_payloadlen @@ Bytes.to_string drr_payload);
    begin if drr_type = DRR_OBJECT && drr_size <> 312 then
        Logs.app (fun m ->m"BONUS: %s" @@ Nvlist.hex @@ Bytes.sub_string s 56 (drr_size));
    end ;
    if drr_type <> DRR_BEGIN then begin
      (* first we compute the checksum of the fixed-length dmu record,
         then we read the on-disk checksum and compare,
         then we add in the checksum itself and any extra contents to the
         running, incremental checksum state. ie if we have a
         WRITE followed by a FREE, the WRITE contents will be checked as part of
         the on-disk FREE checksum.
      *)
      let cksum_state =
        (* https://github.com/openzfs/zfs/blob/master/lib/libzfs/libzfs_sendrecv.c#L94-L95 *)
        Fletcher4.varsize fletcher4
        (Bytes.to_string s) 0 (312-32) in
      let cksum = Fletcher4.to_string_le cksum_state in
      let cksum_ondisk = Bytes.sub_string s (312-32) (32) in
      let cksum_ok = String.equal cksum cksum_ondisk in
      if cksum_ok then begin
        Logs.app (fun m -> m"cksum %a drr_size %d (/%d)"
                     Fmt.(styled `Green string) (Nvlist.hex cksum)
                     drr_size @@ Bytes.length s)
        end else begin
        Logs.app (fun m -> m"cksum comp: %a drr_size %d (/%d)"
                     Fmt.(styled `Red string)
                     (Nvlist.hex cksum) drr_size (Bytes.length s));
        Logs.app (fun m -> m"cksum disk: %s" @@ Nvlist.hex cksum_ondisk)
      end ;
      ( assert cksum_ok );
      let cksum_state =
        (* add in the checksum so far as well (could just use cksum I guess) *)
        Fletcher4.varsize cksum_state (Bytes.to_string s) (312-32) (drr_size-312+32) in
      drr_size + drr_payloadlen , cksum_state
    end else begin
      let cksum_state = Fletcher4.varsize fletcher4
        (Bytes.to_string s) 0 (drr_size) in
      drr_size + drr_payloadlen , cksum_state
    end
end

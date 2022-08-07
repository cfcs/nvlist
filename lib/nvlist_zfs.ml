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

module ZAP = struct
  (* ZAP: ZFS Attribute Processor
     There are a number of relevant files in OpenZFS:
     - include/sys/zap.h
     - include/sys/zap_impl.h: defines structs zap_phys_t, mzap_phys_t
     - module/zfs/zap_micro.c: example usage of mzap_phys_t
     A very useful resource is Mark Johnston's (markj@) code in FreeBSD:
     - usr.sbin/makefs/zfs/zap.c
  *)
  (*
#define ZBT_LEAF                ((1ULL << 63) + 0) "0000000000000080"
#define ZBT_HEADER              ((1ULL << 63) + 1) "0100000000000080"
#define ZBT_MICRO               ((1ULL << 63) + 3) "0300000000000080"
#define ZAP_MAGIC 0x2F52AB2ABULL
#define MZAP_ENT_LEN            64
#define MZAP_NAME_LEN           (MZAP_ENT_LEN - 8 - 4 - 2) == 50

typedef struct mzap_phys {
        uint64_t mz_block_type; /* ZBT_MICRO */
        uint64_t mz_salt;
        uint64_t mz_normflags;
        uint64_t mz_pad[5];
        mzap_ent_phys_t mz_chunk[1];
        /* actually variable size depending on block size */
} mzap_phys_t;
typedef struct mzap_ent_phys {
        uint64_t mze_value;
        uint32_t mze_cd;
        uint16_t mze_pad;       /* in case we want to chain them someday */
        char mze_name[MZAP_NAME_LEN];
} mzap_ent_phys_t;

typedef struct zap_phys {
        uint64_t zap_block_type;        /* ZBT_HEADER */
        uint64_t zap_magic;             /* ZAP_MAGIC */
*)
  type microzap_entry = {
    value: int64;
    cd: int32; (** Note: uint32 *)
    name: string;
  }
  type microzap = {
    salt: int64;
    normflags: int64;
    (* TODO not sure if this is VLA members: *)
    entries: microzap_entry list;
  }
  type zap = {
    blk: int64;
    numblks: int64;
    shift: int64;
    nextblk: int64;
    blks_copied: int64;
    freeblk: int64;
    num_leafs: int64;
    num_entries: int64;
    salt: int64;
    normflags: int64;
    flags: int64;
  }
  type t = Microzap of microzap
         | Zap of zap
         | Leaf of unit
  let pp_microzap_entry ppf v =
    Fmt.pf ppf "@[{ @[name=%S@ cd=%lu@ value=%Lx@]}@]" v.name v.cd v.value
  let pp ppf = function
    | Microzap mz ->  Fmt.pf ppf "@[Microzap @[{ @[salt=%Lx@ entries=%a@ @]}@]@]"
                        mz.salt (Fmt.brackets @@
                                 Fmt.list pp_microzap_entry) mz.entries
    | Zap z ->
      Fmt.pf ppf ("@[Zap { @[blk=%Lu@ numblks=%Lu@ shift=%Lu@ nextblk=%Lu@ "
                  ^^"blks_copied=%Lu@ freeblk=%Lu@ num_leafs=%Lu@ "
                  ^^"num_entries=%Lu@ salt=%Lx@ normflags=%Lx@ flags=%Lx@ "
                  ^^"@]}@]")
        z.blk z.numblks z.shift z.nextblk z.blks_copied z.freeblk z.num_leafs
        z.num_entries z.salt z.normflags z.flags
    | Leaf _ ->  Fmt.pf ppf "@[Leaf {@[@]}@]"

  let parse s =
    let block_type = Bytes.get_int64_le s 0 in
    match block_type with
    | 0x8000000000000000_L -> (* ZBT_LEAF *)
      (* TODO it seems like ZBT_LEAF are continuations of a previous entry
       * (in the case where content is split across multiple WRITEs) *)
      Ok (Leaf ())
    | 0x8000000000000001_L -> (* ZBT_HEADER *)
      let magic = Bytes.get_int64_le s 8 in
      assert (magic = 0x2F52AB2AB_L);
      (* struct zap_table_phys zap_ptrtbl: *)
      let blk = Bytes.get_int64_le s 16 in
      let numblks = Bytes.get_int64_le s 24 in
      let shift = Bytes.get_int64_le s 32 in
      let nextblk = Bytes.get_int64_le s 40 in
      let blks_copied = Bytes.get_int64_le s 48 in
      (**)
      let freeblk = Bytes.get_int64_le s 56 in
      let num_leafs = Bytes.get_int64_le s 64 in
      let num_entries = Bytes.get_int64_le s 72 in
      let salt = Bytes.get_int64_le s 80 in
      let normflags = Bytes.get_int64_le s 88 in
      let flags = Bytes.get_int64_le s 96 in
      let rest = Bytes.sub_string s 104 (Bytes.length s - 104) in
      Logs.app (fun m -> m"zaprest: %s" (Nvlist.hex rest));
      Ok (Zap {
          blk ; numblks; shift; nextblk; blks_copied;
          freeblk; num_leafs; num_entries; salt; normflags;
          flags;
        })
    | 0x8000000000000003_L -> (* ZBT_MICRO *)
      let salt = Bytes.get_int64_le s 8 in
      let normflags = Bytes.get_int64_le s 16 in
      (* then uint64_t mz_pad[5], resuming at offset 64: 16+8+5*8 *)
      let rec loop acc off =
        if off+64 > Bytes.length s
        then List.rev acc (* TODO this is an error in the on-disk format *)
        else
          let value = Bytes.get_int64_le s off in
          let cd = Bytes.get_int32_le s (off+8) in (* note: unsigned *)
          (*let mze_pad = Bytes.get_uint16_le s (off+12) in*)
          let name = Bytes.sub s (off+14) 50 (* 50: MZAP_NAME_LEN *)
                     |> trim_asciiz in
          if name = ""
          then List.rev acc
          else
            loop ({value;cd;name}::acc) (off+64)
      in
      let entries = loop [] 64 in
      Ok (Microzap { salt ; normflags; entries})

end

module DMU_object = struct
  (* https://cgit.freebsd.org/src/commit/?id=240afd8c1fcc8c5f29dbd4ff0c915795d414405d
     has a clean example of basic DMU_OT structure *)
  type dmutyps = [`ACL
           | `FUID
           | `FUID_SIZE
           | `SYSACL
           | `TODO
           | `dedup
           | `dnode
           | `objset
           | `oldacl
           | `systemattr
           | `uint64
           | `uint8
           | `zap
           | `znode ]
  type _ t = (* include/sys/dmu.h:dmu_object_type *)
    | DMU_OT_OBJECT_DIRECTORY : [>`zap] t
    | DMU_OT_OBJECT_ARRAY : [>`uint64] t
    | DMU_OT_PACKED_NVLIST : [>`uint8] t (* XDR encoded nvlist_pack *)
    | DMU_OT_PACKED_NVLIST_SIZE : [>`uint64] t
    | DMU_OT_BPOBJ : [>`uint64] t
    | DMU_OT_BPOBJ_HDR : [>`uint64] t
    | DMU_OT_SPACE_MAP_HEADER : [>`uint64] t
    | DMU_OT_SPACE_MAP : [>`uint64] t
    | DMU_OT_INTENT_LOG : [>`uint64] t
    | DMU_OT_DNODE : [>`dnode] t
    | DMU_OT_OBJSET : [>`objset] t
    | DMU_OT_DSL_DIR : [>`uint64] t
    | DMU_OT_DSL_DIR_CHILD_MAP : [>`zap] t
    | DMU_OT_DSL_DS_SNAP_MAP : [>`zap] t
    | DMU_OT_DSL_PROPS : [>`zap] t
    | DMU_OT_DSL_DATASET : [>`uint8] t
    | DMU_OT_ZNODE : [>`znode] t
    | DMU_OT_OLDACL : [>`oldacl] t
    | DMU_OT_PLAIN_FILE_CONTENTS : [>`uint8] t
    | DMU_OT_DIRECTORY_CONTENTS : [>`zap] t
    | DMU_OT_MASTER_NODE : [>`zap] t (* zfsprops *)
    | DMU_OT_UNLINKED_SET : [>`zap] t
    | DMU_OT_ZVOL : [>`uint8] t
    | DMU_OT_ZVOL_PROP : [>`zap] t
    | DMU_OT_PLAIN_OTHER : [>`uint8] t
    | DMU_OT_UINT64_OTHER : [>`uint64] t
    | DMU_OT_ZAP_OTHER : [>`zap] t
    | DMU_OT_ERROR_LOG : [>`zap] t
    | DMU_OT_SPA_HISTORY : [>`uint8] t
    | DMU_OT_SPA_HISTORY_OFFSETS : [>`TODO] t
    | DMU_OT_POOL_PROPS : [>`zap] t
    | DMU_OT_DSL_PERMS : [>`zap] t
    | DMU_OT_ACL : [>`ACL] t
    | DMU_OT_SYSACL : [>`SYSACL] t
    | DMU_OT_FUID : [>`FUID] t
    | DMU_OT_FUID_SIZE : [>`FUID_SIZE] t (* TODO uint64? *)
    | DMU_OT_NEXT_CLONES : [>`zap] t
    | DMU_OT_SCAN_QUEUE : [>`zap] t
    | DMU_OT_USERGROUP_USED : [>`zap] t
    | DMU_OT_USERGROUP_QUOTA : [>`zap] t
    | DMU_OT_USERREFS : [>`zap] t
    | DMU_OT_DDT_ZAP : [>`zap] t
    | DMU_OT_DDT_STATS : [>`zap] t
    | DMU_OT_SA : [>`systemattr] t
    | DMU_OT_SA_MASTER_NODE : [>`zap] t (* LAYOUTS, REGISTRY, ... *)
    | DMU_OT_SA_ATTR_REGISTRATION : [>`zap] t (* ZPL_ATIME, ZPL_SIZE, etc *)
    | DMU_OT_SA_ATTR_LAYOUTS : [>`zap] t
    | DMU_OT_SCAN_XLATE : [>`zap] t
    | DMU_OT_DEDUP : [>`dedup] t
    | DMU_OT_DEADLIST : [>`zap] t
    | DMU_OT_DEADLIST_HDR : [>`uint64] t
    | DMU_OT_DSL_CLONES : [>`zap] t
    | DMU_OT_BPOBJ_SUBOBJ : [>`uint64] t
    (* | DMU_OT_NUMTYPES *)
  let of_int32 : int32 -> dmutyps t = function
    (* | 0_l -> DMU_OT_NONE *)
    | 1_l -> DMU_OT_OBJECT_DIRECTORY
    | 2_l -> DMU_OT_OBJECT_ARRAY
    | 3_l -> DMU_OT_PACKED_NVLIST
    | 4_l -> DMU_OT_PACKED_NVLIST_SIZE
    | 5_l -> DMU_OT_BPOBJ
    | 6_l -> DMU_OT_BPOBJ_HDR
    | 7_l -> DMU_OT_SPACE_MAP_HEADER
    | 8_l -> DMU_OT_SPACE_MAP
    | 9_l -> DMU_OT_INTENT_LOG
    | 10_l -> DMU_OT_DNODE
    | 11_l -> DMU_OT_OBJSET
    | 12_l -> DMU_OT_DSL_DIR
    | 13_l -> DMU_OT_DSL_DIR_CHILD_MAP
    | 14_l -> DMU_OT_DSL_DS_SNAP_MAP
    | 15_l -> DMU_OT_DSL_PROPS
    | 16_l -> DMU_OT_DSL_DATASET
    | 17_l -> DMU_OT_ZNODE
    | 18_l -> DMU_OT_OLDACL
    | 19_l -> DMU_OT_PLAIN_FILE_CONTENTS
    | 20_l -> DMU_OT_DIRECTORY_CONTENTS
    | 21_l -> DMU_OT_MASTER_NODE
    | 22_l -> DMU_OT_UNLINKED_SET
    | 23_l -> DMU_OT_ZVOL
    | 24_l -> DMU_OT_ZVOL_PROP
    | 25_l -> DMU_OT_PLAIN_OTHER
    | 26_l -> DMU_OT_UINT64_OTHER
    | 27_l -> DMU_OT_ZAP_OTHER
    | 28_l -> DMU_OT_ERROR_LOG
    | 29_l -> DMU_OT_SPA_HISTORY
    | 30_l -> DMU_OT_SPA_HISTORY_OFFSETS
    | 31_l -> DMU_OT_POOL_PROPS
    | 32_l -> DMU_OT_DSL_PERMS
    | 33_l -> DMU_OT_ACL
    | 34_l -> DMU_OT_SYSACL
    | 35_l -> DMU_OT_FUID
    | 36_l -> DMU_OT_FUID_SIZE
    | 37_l -> DMU_OT_NEXT_CLONES
    | 38_l -> DMU_OT_SCAN_QUEUE
    | 39_l -> DMU_OT_USERGROUP_USED
    | 40_l -> DMU_OT_USERGROUP_QUOTA
    | 41_l -> DMU_OT_USERREFS
    | 42_l -> DMU_OT_DDT_ZAP
    | 43_l -> DMU_OT_DDT_STATS
    | 44_l -> DMU_OT_SA
    | 45_l -> DMU_OT_SA_MASTER_NODE
    | 46_l -> DMU_OT_SA_ATTR_REGISTRATION
    | 47_l -> DMU_OT_SA_ATTR_LAYOUTS
    | 48_l -> DMU_OT_SCAN_XLATE
    | 49_l -> DMU_OT_DEDUP (* "fake dedup" *)
    | 50_l -> DMU_OT_DEADLIST
    | 51_l -> DMU_OT_DEADLIST_HDR
    | 52_l -> DMU_OT_DSL_CLONES
    | 53_l -> DMU_OT_BPOBJ_SUBOBJ
  let pp : Format.formatter -> dmutyps t -> unit = fun ppf v ->
    Fmt.pf ppf "%s" (match v with
        | DMU_OT_OBJECT_DIRECTORY -> "DMU_OT_OBJECT_DIRECTORY"
        | DMU_OT_OBJECT_ARRAY -> "DMU_OT_OBJECT_ARRAY"
        | DMU_OT_PACKED_NVLIST -> "DMU_OT_PACKED_NVLIST"
        | DMU_OT_PACKED_NVLIST_SIZE -> "DMU_OT_PACKED_NVLIST_SIZE"
        | DMU_OT_BPOBJ -> "DMU_OT_BPOBJ"
        | DMU_OT_BPOBJ_HDR -> "DMU_OT_BPOBJ_HDR"
        | DMU_OT_SPACE_MAP_HEADER -> "DMU_OT_SPACE_MAP_HEADER"
        | DMU_OT_SPACE_MAP -> "DMU_OT_SPACE_MAP"
        | DMU_OT_INTENT_LOG -> "DMU_OT_INTENT_LOG"
        | DMU_OT_DNODE -> "DMU_OT_DNODE"
        | DMU_OT_OBJSET -> "DMU_OT_OBJSET"
        | DMU_OT_DSL_DIR -> "DMU_OT_DSL_DIR"
        | DMU_OT_DSL_DIR_CHILD_MAP -> "DMU_OT_DSL_DIR_CHILD_MAP"
        | DMU_OT_DSL_DS_SNAP_MAP -> "DMU_OT_DSL_DS_SNAP_MAP"
        | DMU_OT_DSL_PROPS -> "DMU_OT_DSL_PROPS"
        | DMU_OT_DSL_DATASET -> "DMU_OT_DSL_DATASET"
        | DMU_OT_ZNODE -> "DMU_OT_ZNODE"
        | DMU_OT_OLDACL -> "DMU_OT_OLDACL"
        | DMU_OT_PLAIN_FILE_CONTENTS -> "DMU_OT_PLAIN_FILE_CONTENTS"
        | DMU_OT_DIRECTORY_CONTENTS -> "DMU_OT_DIRECTORY_CONTENTS"
        | DMU_OT_MASTER_NODE -> "DMU_OT_MASTER_NODE"
        | DMU_OT_UNLINKED_SET -> "DMU_OT_UNLINKED_SET"
        | DMU_OT_ZVOL -> "DMU_OT_ZVOL"
        | DMU_OT_ZVOL_PROP -> "DMU_OT_ZVOL_PROP"
        | DMU_OT_PLAIN_OTHER -> "DMU_OT_PLAIN_OTHER"
        | DMU_OT_UINT64_OTHER -> "DMU_OT_UINT64_OTHER"
        | DMU_OT_ZAP_OTHER -> "DMU_OT_ZAP_OTHER"
        | DMU_OT_ERROR_LOG -> "DMU_OT_ERROR_LOG"
        | DMU_OT_SPA_HISTORY -> "DMU_OT_SPA_HISTORY"
        | DMU_OT_SPA_HISTORY_OFFSETS -> "DMU_OT_SPA_HISTORY_OFFSETS"
        | DMU_OT_POOL_PROPS -> "DMU_OT_POOL_PROPS"
        | DMU_OT_DSL_PERMS -> "DMU_OT_DSL_PERMS"
        | DMU_OT_ACL -> "DMU_OT_ACL"
        | DMU_OT_SYSACL -> "DMU_OT_SYSACL"
        | DMU_OT_FUID -> "DMU_OT_FUID"
        | DMU_OT_FUID_SIZE -> "DMU_OT_FUID_SIZE"
        | DMU_OT_NEXT_CLONES -> "DMU_OT_NEXT_CLONES"
        | DMU_OT_SCAN_QUEUE -> "DMU_OT_SCAN_QUEUE"
        | DMU_OT_USERGROUP_USED -> "DMU_OT_USERGROUP_USED"
        | DMU_OT_USERGROUP_QUOTA -> "DMU_OT_USERGROUP_QUOTA"
        | DMU_OT_USERREFS -> "DMU_OT_USERREFS"
        | DMU_OT_DDT_ZAP -> "DMU_OT_DDT_ZAP"
        | DMU_OT_DDT_STATS -> "DMU_OT_DDT_STATS"
        | DMU_OT_SA -> "DMU_OT_SA"
        | DMU_OT_SA_MASTER_NODE -> "DMU_OT_SA_MASTER_NODE"
        | DMU_OT_SA_ATTR_REGISTRATION -> "DMU_OT_SA_ATTR_REGISTRATION"
        | DMU_OT_SA_ATTR_LAYOUTS -> "DMU_OT_SA_ATTR_LAYOUTS"
        | DMU_OT_SCAN_XLATE -> "DMU_OT_SCAN_XLATE"
        | DMU_OT_DEDUP -> "DMU_OT_DEDUP"
        | DMU_OT_DEADLIST -> "DMU_OT_DEADLIST"
        | DMU_OT_DEADLIST_HDR -> "DMU_OT_DEADLIST_HDR"
        | DMU_OT_DSL_CLONES -> "DMU_OT_DSL_CLONES"
        | DMU_OT_BPOBJ_SUBOBJ -> "DMU_OT_BPOBJ_SUBOBJ"
      )
  type payload =
    | Zapt of [`zap] t * ZAP.t
    | Uint64 of [`uint64] t * int64
  let pp_payload ppf = function
    | Zapt (ot, payload) -> Fmt.pf ppf "ZAP %a" (*pp ot*) ZAP.pp payload
    | Uint64 (ot, payload) -> Fmt.pf ppf "Uint64"
  let decode : bytes -> [< dmutyps] t -> (payload, string) result = fun s t ->
    match t with
    | ( DMU_OT_OBJECT_DIRECTORY
      | DMU_OT_DSL_DIR_CHILD_MAP
      | DMU_OT_DSL_DS_SNAP_MAP
      | DMU_OT_DSL_PROPS
      | DMU_OT_DIRECTORY_CONTENTS
      | DMU_OT_MASTER_NODE
      | DMU_OT_UNLINKED_SET
      | DMU_OT_ZVOL_PROP
      | DMU_OT_POOL_PROPS
      | DMU_OT_DSL_PERMS
      | DMU_OT_NEXT_CLONES
      | DMU_OT_SCAN_QUEUE
      | DMU_OT_USERGROUP_USED
      | DMU_OT_USERGROUP_QUOTA
      | DMU_OT_USERREFS
      | DMU_OT_DDT_ZAP
      | DMU_OT_DDT_STATS
      | DMU_OT_SA_MASTER_NODE
      | DMU_OT_SA_ATTR_REGISTRATION
      | DMU_OT_SA_ATTR_LAYOUTS
      | DMU_OT_SCAN_XLATE
      | DMU_OT_DEADLIST
      | DMU_OT_DSL_CLONES
      ) as x ->
      let _ : [`zap] t = x in
      begin match ZAP.parse s with
        | Ok v ->
          Ok (Zapt (x, v))
        | Error s -> Error s
      end
    | ( DMU_OT_OBJECT_ARRAY
      | DMU_OT_PACKED_NVLIST_SIZE
      | DMU_OT_BPOBJ
      | DMU_OT_BPOBJ_HDR
      | DMU_OT_SPACE_MAP_HEADER
      | DMU_OT_SPACE_MAP
      | DMU_OT_INTENT_LOG
      | DMU_OT_DSL_DIR
      | DMU_OT_DEADLIST_HDR
      | DMU_OT_BPOBJ_SUBOBJ
      | DMU_OT_UINT64_OTHER) as x ->
      Ok (Uint64 (x, 1L))
    | _ ->
      Error "Unimplemented DMU_OT"
end

module DRR = struct
  (*
   * This is still in a mockup state, but deals with reading
   * zfs send-streams
   *)
  type ('full, 'status) alloc =
  | Empty : ('full, [`empty]) alloc
  | Full : 'full -> ('full, [`full]) alloc
  constraint 'status = [<`empty | `full]

  type drr_begin =
    { magic: int64;
      versioninfo: int64;
      creationtime: int64;
      typ: int64;
      flags: int32;
      toguid: int64;
      fromguid: int64;
      name: string; (*asciiz*)
    }

  type drr_free =
    { obj: int64;
      offset: int64;
      length: int64;
      toguid: int64; }

  type drr_end =
    { (* on-disk: *)
      cksum: string;
      toguid: int64;
      (* computed by this implementation: *)
      computed_cksum: string;
    }

  type 'status t = (* drr_type *)
    | DRR_BEGIN : (drr_begin, 'status) alloc -> 'status t
    | DRR_OBJECT : (_ DMU_object.t, 'status) alloc -> 'status t
    | DRR_FREEOBJECTS : 'status t
    | DRR_WRITE : 'status t
    | DRR_FREE : (drr_free, 'status) alloc -> 'status t
    | DRR_END : (drr_end, 'status) alloc -> 'status t
    | DRR_WRITE_BYREF : 'status t
    | DRR_SPILL : 'status t
    | DRR_WRITE_EMBEDDED : 'status t
    | DRR_OBJECT_RANGE : 'status t
    | DRR_REDACT : 'status t
    | DRR_NUMTYPES : 'status t
  let int_of_drr_type = function
    | DRR_BEGIN _ -> 0
    | DRR_OBJECT _ -> 1
    | DRR_FREEOBJECTS -> 2
    | DRR_WRITE -> 3 | DRR_FREE _ -> 4 | DRR_END _ -> 5
    | DRR_WRITE_BYREF -> 6
    | DRR_SPILL -> 7 | DRR_WRITE_EMBEDDED -> 8
    | DRR_OBJECT_RANGE -> 9 | DRR_REDACT -> 10
    | DRR_NUMTYPES -> 11
  let drr_type_of_int32 : int32 -> [`empty] t = function
    | 0l -> DRR_BEGIN Empty
    | 1l -> DRR_OBJECT Empty
    | 2l -> DRR_FREEOBJECTS
    | 3l -> DRR_WRITE | 4l -> DRR_FREE Empty
    | 5l -> DRR_END Empty
    | 6l -> DRR_WRITE_BYREF | 7l -> DRR_SPILL | 8l -> DRR_WRITE_EMBEDDED
    | 9l -> DRR_OBJECT_RANGE | 10l -> DRR_REDACT | 11l -> DRR_NUMTYPES
    | _ -> raise (Invalid_argument "incorrect drr typ > 11")
  let pp_drr_type ppf (v :  _ t) =
    Fmt.pf ppf ((match v with
        | DRR_BEGIN _ -> "BEGIN" | DRR_OBJECT _ -> "OBJECT"
        | DRR_FREEOBJECTS -> "FREEOBJECTS" | DRR_WRITE -> "WRITE"
        | DRR_FREE _ -> "FREE" | DRR_END _ -> "END"
        | DRR_WRITE_BYREF -> "WRITE_BYREF" | DRR_SPILL -> "SPILL"
        | DRR_WRITE_EMBEDDED -> "WRITE_EMBEDDED"
        | DRR_OBJECT_RANGE -> "OBJECT_RANGE" | DRR_REDACT -> "REDACT"
        | DRR_NUMTYPES -> "NUMTYPES") ^^ "[%u]")
      (int_of_drr_type v)

  let decode_drr_begin s =
    let magic = Bytes.get_int64_le s 0 in
    assert (magic = 0x2F5bacbac_L);
    (* should be DMU_BACKUP_MAGIC ?
       can it assume other values?
       https://github.com/openzfs/zfs/blob/master/lib/libzfs/libzfs_sendrecv.c#L2079
       looks like this is used to detect byteswap requirements
    *)
    let versioninfo =
      (* see DMU_SET_FEATUREFLAGS *)
      Bytes.get_int64_le s 8 in
    let creationtime = Bytes.get_int64_le s 16 in
    let drr_type =
      (* ? DMU_COMPOUNDSTREAM ? *)
      Bytes.get_int64_le s 24 in
    let flags =
      (* | DRR_FLAG_CLONE | DRR_FLAG_CI_DATA
         | DRR_FLAG_FREERECORDS | DRR_FLAG_SPILL_BLOCK | ... *)
      Bytes.get_int32_le s 28 in
    let toguid = Bytes.get_int64_le s 32 in
    let fromguid = Bytes.get_int64_le s 40 in
    let name = Bytes.sub s 48 256 |> trim_asciiz in
    (* drr_name will be padded with 0x00 *)
    Logs.app (fun m -> m "drr_magic: %Lu" magic);
    Logs.app (fun m -> m "drr_versioninfo: %Lu" versioninfo);
    Logs.app (fun m -> m "drr_creationtime: %Lu" creationtime);
    Logs.app (fun m -> m "drr_type: %Lu" drr_type);
    Logs.app (fun m -> m "drr_flags: %lu" flags);
    Logs.app (fun m -> m "drr_fromguid: %Lu" fromguid);
    Logs.app (fun m -> m "drr_toguid: %Lu" toguid);
    Logs.app (fun m -> m "drr_name: %S" name);
    DRR_BEGIN (Full {
        magic; versioninfo; creationtime;
        typ = drr_type;
        flags; toguid; fromguid; name
      })
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
          (try Fmt.str "%a" DMU_object.pp
                 (DMU_object.of_int32 drr_type)
           with _ -> Int32.to_string drr_type)
          drr_checksumtype drr_compress);
    Logs.app (fun m->m"bonuslen %lu rawbonuslen %lu toguid:%Lu" drr_bonuslen drr_raw_bonuslen drr_toguid);
    assert (drr_raw_bonuslen = 0l);
    DRR_OBJECT (Full (DMU_object.of_int32 drr_type)), Int32.to_int drr_bonuslen
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
    let dmu_ot = DMU_object.of_int32 drr_type in
    Logs.app (fun m -> m "WRITE drr_object: %Lu (drr_type: %s) toguid:%Lu logicsize:%Lu compressedsize:%Lu cksumtyp:%l drr_pad %lu"
                 drr_object
                 (try Fmt.str "%a" DMU_object.pp (dmu_ot)
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
    begin match DMU_object.decode (Bytes.of_string content) dmu_ot with
      | Ok dmu_payload ->
        Logs.app (fun m -> m "payload: %a" DMU_object.pp_payload dmu_payload)
      | _ ->
        Logs.app (fun m -> m "content: %s\n(%Lu+312 < %d)"
                     (Nvlist.hex content)
                     (drr_logical_size) (Bytes.length s)
                 )
    end ;
    DRR_WRITE, Int64.to_int drr_logical_size
  let decode_drr_free s =
    let obj = Bytes.get_int64_le s 0 in
    let offset = Bytes.get_int64_le s 8 in
    let length = Bytes.get_int64_le s 16 in
    let toguid = Bytes.get_int64_le s 24 in
    Logs.app (fun m -> m "drr_object: %Lu offset:%Lu length:%Ld toguid:%Lu"
                 obj offset length toguid) ;
    DRR_FREE (Full { obj; offset; length; toguid }), 0

  let parse_drr (type drr_typ) ~fletcher4 s  : [`full] t * int * Fletcher4.t =
    (* useful resources here:
       https://github.com/openzfs/zfs/blob/958826be7a3e17f29e1f5e114c76aa2ec3c8a490/include/sys/zfs_ioctl.h#L242
       streams consist of  dmu_replay_record structs, and optional "payloads" as determined by the drr_payloadlen member.
       see dump_record()
    *)
    Logs.app (fun m -> m "\nParsing drr 0x%s" @@ Nvlist.hex @@ Bytes.sub_string s 0 4);
    let drr_type : _ t =
      let i32 = Bytes.get_int32_le s 0 in
      drr_type_of_int32 i32
    in
    (*begin match drr_type with
      (* all record types except from DRR_BEGIN are checksummed with
         Fletcher 4 -- TODO check the checksum. *)
      | DRR_BEGIN _ -> DRR_BEGIN Empty, 4
      | _ -> 312 end + *)
    let with_checksum (type a) ((drr: ([`full]) t), bonus_size) =
      let checksum_size = 312 in
      drr, bonus_size + checksum_size
    in
    begin
      match drr_type with
      | DRR_BEGIN Empty (* 0 *) ->
        let sizeof_drr_begin = 8+8+8 +
                               8 (* dmu_objset_type_t *)
                               + 4+8+8
                               + 256 (*char[MAXNAMELEN]*) in
        decode_drr_begin (Bytes.sub s 8 sizeof_drr_begin),
        sizeof_drr_begin + 4
      | DRR_OBJECT _ (* 1 *) ->
        with_checksum @@ decode_drr_object (Bytes.sub s 8 56)
      | DRR_FREEOBJECTS (* 2 *) ->
        with_checksum (DRR_FREEOBJECTS, 0) (*8+8+8*)
      | DRR_WRITE (* 3 *) ->
        with_checksum @@ decode_drr_write (Bytes.sub s 8 (Bytes.length s -8))
      | DRR_FREE _ (* 4 *)->
        with_checksum @@ decode_drr_free (Bytes.sub s 8 32)
      | DRR_END _ (* 5 *)->
        let cksum = Bytes.sub_string s 8 32 in
        let toguid = Bytes.get_int64_le s 40 in
        let computed_cksum = Fletcher4.to_string_le fletcher4 in
        Logs.app (fun m -> m"END: drr_cksum:%s toguid:%Lu\n   cksum comp: %s"
                     (Nvlist.hex cksum) toguid
                     (Nvlist.hex @@ computed_cksum)
                 );
        with_checksum @@ (DRR_END (Full {
            cksum; toguid;
            computed_cksum;
          }), 0)
      | _ -> failwith (Fmt.str "not implemented: drr type %u" @@ int_of_drr_type drr_type)
    end
    |> fun (drr_type, drr_size) ->
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
    begin match  drr_type with DRR_OBJECT _ when drr_size <> 312 ->
      Logs.app (fun m ->m"BONUS: %s" @@ Nvlist.hex @@ Bytes.sub_string s 56 (drr_size));
                             | _ -> ()
    end ;
    begin match drr_type with
      | DRR_BEGIN _ ->
        let cksum_state = Fletcher4.varsize fletcher4
            (Bytes.to_string s) 0 (drr_size) in
        drr_type, drr_size + drr_payloadlen , cksum_state
      | _ ->
        begin
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
          (* older versions of ZFS emitted only checksums for DRR_END records *)
          (* ( assert cksum_ok ); *)
          let cksum_state =
            (* add in the checksum so far as well (could just use cksum I guess) *)
            Fletcher4.varsize cksum_state (Bytes.to_string s) (312-32) (drr_size-312+32) in
          drr_type, drr_size + drr_payloadlen , cksum_state
        end
    end
end

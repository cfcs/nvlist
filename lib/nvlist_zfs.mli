module Fletcher4 : sig

  type t = {
    a: Int64.t ;
    b: Int64.t ;
    c: Int64.t ;
    d: Int64.t ;
  }

  (* A zero-initialized checksum state *)
  val empty : t

  val to_string_le : t -> string
  (* little-endian canonical byte representation as used in ZFS *)

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

(** struct dmu_object_type from include/sys/dmu.h *)
module DMU_object : sig
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
  val of_int32 : int32 -> dmutyps t
  val pp : Format.formatter -> dmutyps t -> unit
end

module DRR : sig
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
    | DRR_OBJECT : ('payload DMU_object.t, 'status) alloc -> 'status t
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

  val drr_type_of_int32 : int32 -> [`empty] t
  val parse_drr : fletcher4:Fletcher4.t -> bytes
    -> [`full] t * int * Fletcher4.t
end

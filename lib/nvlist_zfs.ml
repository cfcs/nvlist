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

  let varsize old_t buf off len =
    assert (off mod 4 = 0); (* TODO can't remember how this is implemented in ZFS *)
    assert (len mod 4 = 0); (* since this works on 32bit-aligned packed nvlist*)
    let ipend = off + len/4 in
    let ip = ref off in
    let t = ref old_t in
    let buf = Bytes.of_string buf in
    while !ip < ipend do begin
      let (+) = Int64.add in
      t:= { a = !t.a + get_uint32_le buf !ip ;
            b = !t.b + !t.a ;
            c = !t.c + !t.b ;
            d = !t.d + !t.c ;
          } ;
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
        Error (Fmt.strf "checksum mismatch %Lx <> %Lx" checksum own_cksum) else Ok ()
    ) >>= fun () ->
    (Ezgzip.Z.decompress ~header:true data
     |> reword_error (function `Zlib (Ezgzip.Z.Truncated x
                                     | Compression_error x) -> x))
    >>= fun decompressed ->
    let expected_len = Int64.of_int @@ String.length decompressed in
    (if payload_len <> expected_len
     then
       Error (Fmt.strf "compressed payload is truncated (%Ld <> %Ld)" payload_len expected_len)
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

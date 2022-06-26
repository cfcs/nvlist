(*
To fuzz with afl:
afl-fuzz -i fuzz_in -o fuzz_out -t 10000 -- ./_build/default/app/nvlist_gen.exe @@
*)
open Rresult

let nvl_gen : Nvlist.nvl Crowbar.gen =
  let open Nvlist in
  let open Crowbar in
  let (>>) a f = map [a] f in
  fix (fun nvl_gen ->
      let nv_gen =
        choose [
          const Nvlist.Boolean ;
          char >> (fun x -> Byte x) ;
          int16 >> (fun x -> Int16 x) ;
          uint16 >> (fun x -> Uint16 x) ;
          int32 >> (fun x -> Int32 x) ;
          int32 >> (fun x -> Uint32 x) ;
          int64 >> (fun x -> Int64 x) ;
          int64 >> (fun x -> Uint64 x) ;
          bytes >> (fun x -> Str x) ;
          bytes >> (fun x -> ByteArray x) ;
          (list int16) >> (fun x -> Int16Array x) ;
          (list uint16) >> (fun x -> Uint16Array x) ;
          (list int32) >> (fun x -> Int32Array x) ;
          (list int32) >> (fun x -> Uint32Array x) ;
          (list int64) >> (fun x -> Int64Array x) ;
          (list int64) >> (fun x -> Uint64Array x) ;
          int64 >> (fun x -> HrTime x) ;
          (list bytes) >> (fun x -> StrArray x) ;
          nvl_gen >> (fun x -> Nvlist x) ;
          list nvl_gen >> (fun x -> NvlistArray x) ;
          int8 >> (fun x -> Int8 x) ;
          uint8 >> (fun x -> Uint8 x) ;
          list bool >> (fun x -> BooleanArray x);
          list int8 >> (fun x -> Int8Array x) ;
          list uint8 >> (fun x -> Uint8Array x) ;
          const Double ;
        ]
      in
      let kvpair =
        Crowbar.map [Crowbar.bytes ; nv_gen]
          (fun key value -> key,value) in
      Crowbar.with_printer (Nvlist.pp_nvl) (list kvpair)
    )

let () =
  Crowbar.add_test ~name:"nvgen" [nvl_gen]
    (fun nvl ->
       match Nvlist.fnvlist_pack nvl with
       | Ok packed ->
       begin match Nvlist.fnvlist_unpack packed with
         | Ok unpacked -> Crowbar.check_eq ~pp:Nvlist.pp_nvl unpacked nvl
         | Error reunpack_failure -> Crowbar.fail reunpack_failure
       end
       | Error _ -> () (* don't care for invalid inputs in this test *)
    )

let c_nvlist =
  Alcotest.testable Nvlist.pp_nvl (=)

let test_case1 () =
  Alcotest.(check @@ result c_nvlist string)
    "badbdec4c"
    (Ok ([
         (  "object", Uint64 (0x23_L));
         (  "offset", Uint64 (0_L));
         (   "bytes", Uint64 (0x2ab0_L));
         (  "toguid", Uint64 (0xa60221498db5b662_L));
         (  "toname", Str "tmpzfs@repli-2020-05-06")]))
    (Nvlist_zfs.Resumetoken.parse
       "1-badbdec4c-c0-789c636064000310a500c4ec50360710e72765a52697303028439460caa7a515a79630c001489e0d493ea9b224b518486fd0c2aebf243fbd343305a86edbd65e4f45a6650648f29c60f9bcc4dc54209d5b509556ec50945a9093a96b646064a06b60aa6b6006361300c1bf18b7")

(*
TODO: this seems to throw camlzip into an infinite loop in camlzip_inflate -- maybe `decompress` can handle it?:
Nvlist_zfs.Resumetoken.parse
       "1-badbdec4c-c0-789c636064000310a500c4ec50360710e72765a52697303028439460caa7a515a79630c001489e0d493ea9b224b518486fd0c2aebf243fbd343305a86edbd65e4f45a6650648f29c60f9bcc4dc54209d5b509556ec50945a9093a96b646064a06b60aa6b6006361300c1bf18b"
*)


let tests = [
  "unpack", [
    "case1", `Quick, test_case1 ;
  ]
]

let () =
  Alcotest.run "zfs tests depending on ezgzip" tests

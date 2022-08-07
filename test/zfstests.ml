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
let hexcheck =
  let pp ppf v =
    Fmt.pf ppf "|%s|" (Nvlist.hex v) in
  Alcotest.testable pp String.equal

let test_f4_basic () =
  let open Nvlist_zfs in
  Alcotest.(check hexcheck) "empty"
  Fletcher4.(to_string_le empty)
  (Fletcher4.(varsize empty) "abcd" 0 0 |> Fletcher4.to_string_le) ;
  Alcotest.(check hexcheck) "1"
  "\x01\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00"
  (Fletcher4.(varsize empty) "\x01\x00\x00\x00" 0 4 |> Fletcher4.to_string_le) ;
  Alcotest.(check hexcheck) "abcd"
  "abcd\x00\x00\x00\x00abcd\x00\x00\x00\x00abcd\x00\x00\x00\x00abcd\x00\x00\x00\x00"
  (Fletcher4.(varsize empty) "abcdefgh" 0 4 |> Fletcher4.to_string_le) ;
  Alcotest.(check hexcheck) "1x2"
    "\x01\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x03\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00"
    (Fletcher4.(varsize empty) "\x01\x00\x00\x00\x00\x00\x00\x00" 0 8
     |> Fletcher4.to_string_le) ;
  Alcotest.(check hexcheck) "1x3"
    "\x01\x00\x00\x00\x00\x00\x00\x00\x03\x00\x00\x00\x00\x00\x00\x00\x06\x00\x00\x00\x00\x00\x00\x00\x0a\x00\x00\x00\x00\x00\x00\x00"
    (Fletcher4.(varsize empty) "\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00" 0 12
     |> Fletcher4.to_string_le)

let test_recv_basic () =
  let basic = Nvlist.unhex "0000000000000000accbbaf50200000011000000000000004384a96100000000020000000c000000e37bed1f7cde6843d9390392d96dcd137465737474616e6b4063726f6e2d323032312d31322d30330000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000005000000000000001b086f44040000007bcb04901e010000d1b620766a2600006b1e9aad00820300e37bed1f7cde684300000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000ddb4f79f06000000cfba502edf020000fd7e8ab423b1000032af16d387621e00" in
  let open Nvlist_zfs in
  let left = ref (String.length basic) in
    let fletcher4 = ref Fletcher4.empty in
  while !left > 0 do
    let _drr, consumed, cksum_new = DRR.parse_drr ~fletcher4:!fletcher4 (
        String.sub basic (String.length basic - !left) !left
        |> Bytes.of_string) in
    left := !left - consumed ;
    fletcher4 := cksum_new
  done ;
  assert (!left = 0);
  ()

let test_dmu_OT_of_int32 () =
  let open Nvlist_zfs in
  for idx = 1 to 53 do
    let ot = DMU_object.of_int32 (Int32.of_int idx) in
    Alcotest.(check unit) (Printf.sprintf "we have a pp for OT %d" idx)
      ( () )
      (DMU_object.pp Format.str_formatter ot) ;
    assert (Obj.tag (Obj.magic ot) == Obj.int_tag) ;
    Alcotest.(check int) "check variant tag matches of_int32"
      (idx)
      (succ (Obj.magic ot : int)) (* succ because we skip DMU_OT_NONE *)
  done

let tests = [
  "fletcher4", [
    "basic", `Quick, test_f4_basic ;
    (* TODO crowbar test that truncated_fletcher4 matches varsize implementation *)
  ];
  "dmu_object_type", [
    "of_int32", `Quick, test_dmu_OT_of_int32 ;
  ];
  "unpack resumetok", [
    "case1", `Quick, test_case1 ;
  ];
  "zfs-send stream", [
    "basic", `Quick, test_recv_basic;
  ];
]

let () =
  Alcotest.run "zfs tests depending on ezgzip" tests

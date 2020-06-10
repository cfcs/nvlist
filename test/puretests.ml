let hexcheck =
  let pp ppf v =
    Fmt.pf ppf "|%s|" (Nvlist.hex v) in
  Alcotest.testable pp String.equal

let nvvalue =
  let pp ppf v = Fmt.pf ppf "%a" Nvlist.pp_value v in
  Alcotest.testable pp (=)

let pp_nvlist ppf v =
  Fmt.pf ppf "%a" Fmt.(
      list ~sep:(unit ";")
      @@
      pair ~sep:(unit ":") string Nvlist.pp_value) v

let nvlist =
  Alcotest.testable pp_nvlist (=)

let test_hex () =
  let estring = Alcotest.testable (fun ppf -> Fmt.pf ppf "%S") String.equal in
  Alcotest.(check estring)
    "encode d1d2"
    "d1d2"
    (Nvlist.hex "\xd1\xd2") ;
  Alcotest.(check estring)
    "decode d1d2"
    "\xd1\xd2"
    (Nvlist.unhex "d1d2")

let test_pad_str () =
  let test src expect =
    Alcotest.(check hexcheck)
      (Nvlist. hex src ^ " -> " ^ Nvlist.hex expect)
      expect
      (let b = Buffer.create 1 in
       Nvlist.pad_str b src ;
       Buffer.contents b)
  in
  test "" ("" ^ String.make 8 '\000') ;
  test "a" ("a" ^ String.make 7 '\000') ;
  test "ab" ("ab" ^ String.make 6 '\000') ;
  test "abc" ("abc" ^ String.make 5 '\000') ;
  test "abcd" ("abcd" ^ String.make 4 '\000') ;
  test "abcde" ("abcde" ^ String.make 3 '\000') ;
  test "abcdef" ("abcdef" ^ String.make 2 '\000') ;
  test "abcdefg" ("abcdefg" ^ String.make 1 '\000') ;
  test "abcdefgh" ("abcdefgh" ^ String.make 8 '\000') ;
  test "abcdefgh3" ("abcdefgh3" ^ String.make 7 '\000')

let test_encode_empty () =
  let empty_nv =
  "00010000" ^ "00000000" ^ "01000000" ^ "00000000" in
  Alcotest.(check @@ result hexcheck string)
    "empty"
    (Ok (Nvlist.unhex empty_nv))
    (Nvlist.fnvlist_pack @@ [])

let test_encode_simple () =
  let v =
   "00010000" ^ "00000000" ^ "01000000" ^ "20000000" ^ "03000000" ^ "01000000"
 ^ "05000000" ^ "796f0000" ^ "00000000" ^ "09000000" ^ "00000000" ^ "20000000" ^ "03000000" ^ "01000000" ^ "05000000" ^ "6c6f0000"
 ^ "00000000" ^ "0a000000" ^ "00000000" ^ "00000000" ^ "" in
  Alcotest.(check @@ result hexcheck string)
    "yo:i32=9,lo:i32=10"
    (Ok (Nvlist.unhex v))
    (Nvlist.fnvlist_pack @@
     [ (      "yo", Int32 (0x9l)); (      "lo", Int32 (0xal))]    ) ;
  let yo_int32_123 =
  "00010000" ^ "00000000" ^ "01000000" ^ "20000000" ^ "03000000" ^ "01000000"
 ^ "05000000" ^ "796f0000" ^ "00000000" ^ "7b000000" ^ "00000000" ^ "00000000" in
  Alcotest.(check @@ result hexcheck string)
    "yo_int32_1234"
    (Ok (Nvlist.unhex yo_int32_123))
    (Nvlist.fnvlist_pack @@ ["yo", Int32 123l]) ;
  (* nvlist size: 120 *)
  let v =
    "00010000" ^ "00000000" ^ "01000000" ^ "20000000" ^ "02000000" ^ "01000000"
    ^ "05000000" ^ "61000000" ^ "00000000" ^ "09000000" ^ "00000000" ^ "28000000" ^ "09000000" ^ "01000000" ^ "05000000" ^ "61626364"
    ^ "494d4150" ^ "00000000" ^ "00000000" ^ "0a000000" ^ "00000000" ^ "20000000" ^ "07000000" ^ "01000000" ^ "07000000" ^ "776f6f68"
    ^ "6f6f0000" ^ "33000000" ^ "00000000" ^ "00000000" in
  Alcotest.(check @@ result hexcheck string)
    "a:i32=9 abcdIMAP:i32=0xa woohoo:i64=0x33"
    (Ok (Nvlist.unhex v))
    (Nvlist.fnvlist_pack @@
     [ (       "a", Int32 (0x9l)); ("abcdIMAP", Int32 (0xal));   (  "woohoo", Int64 (0x33L))]    ) ;

  (* nvlist size: 68 *)
  let v =
    "00010000" ^ "00000000" ^ "01000000" ^ "30000000" ^ "08000000" ^ "01000000"
    ^ "13000000" ^ "31323334" ^ "35363700" ^ "00000000" ^ "01000000" ^ "00000000" ^ "00000000" ^ "00000000" ^ "00000000" ^ "00000000"
    ^ "00000000" ^ "" in
  let v2 = Nvlist.fnvlist_pack @@
    [ (       "1234567",  Nvlist [] )] in
  (* TODO need to check that we can unpack everything we generate
     begin let open Rresult in match v2 >>= Nvlist.fnvlist_unpack with
     | Ok _ -> ()
     | Error _ -> assert false
     end ; *)
  Alcotest.(check @@ result hexcheck string)
    "nested empty nvlist"
    (Ok (Nvlist.unhex v))
    (v2)

let test_encode_simple_small () =
  (* nvlist size: 80 *)
  let v =
    "00010000" ^ "00000000" ^ "01000000" ^ "20000000" ^ "02000000" ^ "01000000"
    ^ "03000000" ^ "61000000" ^ "00000000" ^ "39300000" ^ "00000000" ^ "20000000" ^ "02000000" ^ "01000000" ^ "04000000" ^ "62000000"
    ^ "00000000" ^ "409c0000" ^ "00000000" ^ "00000000" ^ "" in
  Alcotest.(check @@ result hexcheck string)
    "a:int16,b:uint16"
    (Ok (Nvlist.unhex v))
    (Nvlist.fnvlist_pack @@
     [ (       "a", Int16 (0x3039));
       (       "b", Uint16 (40000))]    )

let test_encode_int16array () =
  (* nvlist size: 48 *)
  let v =
    "00010000" ^ "00000000" ^ "01000000" ^ "20000000" ^ "04000000" ^ "03000000"
    ^ "0b000000" ^ "61626300" ^ "00000000" ^ "01000200" ^ "03000000" ^ "00000000" ^ "" in
  Alcotest.(check @@ result hexcheck string)
    "c_test"
    (Ok (Nvlist.unhex v))
    (Nvlist.fnvlist_pack @@
     [ (     "abc", Int16Array [1;2;3])]    )

let test_encode_nested () =
  (* nvlist size: 100 *)
  let v =
    "00010000" ^ "00000000" ^ "01000000" ^ "30000000" ^ "02000000" ^ "01000000"
    ^ "13000000" ^ "61000000" ^ "00000000" ^ "00000000" ^ "01000000" ^ "00000000" ^ "00000000" ^ "00000000" ^ "00000000" ^ "20000000"
    ^ "02000000" ^ "01000000" ^ "05000000" ^ "62000000" ^ "00000000" ^ "33000000" ^ "00000000" ^ "00000000" ^ "00000000" ^ "" in
  let v2 = Nvlist.fnvlist_pack @@
     [ (       "a",  Nvlist [ (       "b", Int32 (0x33l))])] in
  (* TODO need to check that we can unpack everything we generate
begin let open Rresult in match v2 >>= Nvlist.fnvlist_unpack with
    | Ok _ -> ()
    | Error _ -> assert false
  end ; *)
  Alcotest.(check @@ result hexcheck string)
    "nested nvlist"
    (Ok (Nvlist.unhex v))
    (v2) ;
  ()(*(* nvlist size: 132 *)
  let v =
    "00010000" ^ "00000000" ^ "01000000" ^ "30000000" ^ "02000000" ^ "01000000"
    ^ "13000000" ^ "61000000" ^ "00000000" ^ "00000000" ^ "01000000" ^ "00000000" ^ "00000000" ^ "00000000" ^ "00000000" ^ "20000000"
    ^ "02000000" ^ "01000000" ^ "05000000" ^ "62000000" ^ "00000000" ^ "09000000" ^ "00000000" ^ "00000000" ^ "20000000" ^ "02000000"
    ^ "01000000" ^ "07000000" ^ "63000000" ^ "00000000" ^ "33000000" ^ "00000000" ^ "00000000" in
  Alcotest.(check @@ result hexcheck string)
    "c_test"
    (Ok (Nvlist.unhex v))
    (Nvlist.fnvlist_pack @@
     [ (       "a",  Nvlist [ (       "b", Int32 (0x9l))]);
       (       "c", Int64 (0x33L))]) *)

let test_encode_nested_2 () =
  (* nvlist size: 100 *)
  let v =
    "00010000" ^ "00000000" ^ "01000000" ^ "30000000" ^ "02000000" ^ "01000000"
    ^ "13000000" ^ "61000000" ^ "00000000" ^ "00000000" ^ "01000000" ^ "00000000" ^ "00000000" ^ "00000000" ^ "00000000" ^ "20000000"
    ^ "02000000" ^ "01000000" ^ "05000000" ^ "62000000" ^ "00000000" ^ "33000000" ^ "00000000" ^ "00000000" ^ "00000000" ^ "" in
  Alcotest.(check @@ result hexcheck string)
    "ab_test"
    (Ok (Nvlist.unhex v))
    (Nvlist.fnvlist_pack @@
     [ (       "a", Nvlist  [ (       "b", Int32 (0x33l))])]    )

let test_encode_nested_3 () =
  (* nvlist size: 204 *)
  let v =
    "00010000" ^ "00000000" ^ "01000000" ^ "30000000" ^ "02000000" ^ "01000000"
    ^ "13000000" ^ "61000000" ^ "00000000" ^ "00000000" ^ "01000000" ^ "00000000" ^ "00000000" ^ "00000000" ^ "00000000" ^ "30000000"
    ^ "02000000" ^ "01000000" ^ "13000000" ^ "62000000" ^ "00000000" ^ "00000000" ^ "01000000" ^ "00000000" ^ "00000000" ^ "00000000"
    ^ "00000000" ^ "30000000" ^ "02000000" ^ "01000000" ^ "13000000" ^ "63000000" ^ "00000000" ^ "00000000" ^ "01000000" ^ "00000000"
    ^ "00000000" ^ "00000000" ^ "00000000" ^ "20000000" ^ "02000000" ^ "01000000" ^ "05000000" ^ "64000000" ^ "00000000" ^ "33000000"
    ^ "00000000" ^ "00000000" ^ "00000000" ^ "00000000" ^ "00000000" ^ "" in
  Alcotest.(check @@ result hexcheck string)
    "abcd_test"
    (Ok (Nvlist.unhex v))
    (Nvlist.fnvlist_pack @@
     [ (       "a", Nvlist  [
           (       "b", Nvlist  [
                       (       "c", Nvlist [
                                   (       "d", Int32 (0x33l))]
                       )
                     ]
           )]
         )])

let test_encode_nested_int32,
    test_decode_nested_int32 =
  (* nvlist size: 164 *)
  let v =
    "00010000" ^ "00000000" ^ "01000000" ^ "30000000" ^ "02000000" ^ "01000000"
    ^ "13000000" ^ "78000000" ^ "00000000" ^ "00000000" ^ "01000000" ^ "00000000" ^ "00000000" ^ "00000000" ^ "00000000" ^ "20000000"
    ^ "03000000" ^ "01000000" ^ "05000000" ^ "61610000" ^ "00000000" ^ "05000000" ^ "00000000" ^ "20000000" ^ "04000000" ^ "01000000"
    ^ "05000000" ^ "62626200" ^ "00000000" ^ "06000000" ^ "00000000" ^ "20000000" ^ "05000000" ^ "01000000" ^ "05000000" ^ "63636363"
    ^ "00000000" ^ "07000000" ^ "00000000" ^ "00000000" ^ "00000000" ^ "" in
  let exp =[ (       "x", Nvlist.Nvlist  [
      (      "aa", Int32 (0x5l));
      (     "bbb", Int32 (0x6l));
      (    "cccc", Int32 (0x7l));
    ])]in
  (fun() ->
     Alcotest.(check @@ result hexcheck string)
       "c_test"
       (Ok (Nvlist.unhex v))
       (Nvlist.fnvlist_pack exp)
  ),
  (fun() ->
     Alcotest.(check @@ result nvlist string)
       "c_test_decode"    (Ok exp)    (Nvlist.fnvlist_unpack @@ Nvlist.unhex v))

let test_encode_byte () =
  (* nvlist size: 48 *)
  let v =
    "00010000" ^ "00000000" ^ "01000000" ^ "20000000" ^ "02000000" ^ "01000000"
    ^ "02000000" ^ "61000000" ^ "00000000" ^ "62000000" ^ "00000000" ^ "00000000" ^ "" in
  Alcotest.(check @@ result hexcheck string)
    "c_test"
    (Ok (Nvlist.unhex v))
    (Nvlist.fnvlist_pack @@
     [ (       "a", Byte 'b')]    )

let test_encode_nvlistarray_empty () =
  (* nvlist size: 40 *)
  let v =
    "00010000" ^ "00000000" ^ "01000000" ^ "18000000" ^ "02000000" ^ "00000000"
    ^ "14000000" ^ "61000000" ^ "00000000" ^ "00000000" ^ "" in
  Alcotest.(check @@ result hexcheck string)
    "c_test"
    (Ok (Nvlist.unhex v))
    (Nvlist.fnvlist_pack @@
     [ (       "a", NvlistArray [])]    )

let test_decode_nvlistarray_empty () =
  (* nvlist size: 40 *)
  let v =
    "00010000" ^ "00000000" ^ "01000000" ^ "18000000" ^ "02000000" ^ "00000000"
    ^ "14000000" ^ "61000000" ^ "00000000" ^ "00000000" ^ "" in
  Alcotest.(check @@ result nvlist string)
    "nvlistarray_empty"
    (Ok (     [ (       "a", NvlistArray [])]    ))
    (Nvlist.fnvlist_unpack @@ Nvlist.unhex v)

let test_encode_nvlistarray_single_empty ,
    test_decode_nvlistarray_single_empty =
  (* nvlist size: 76 *)
  let v =
    "00010000" ^ "00000000" ^ "01000000" ^ "38000000" ^ "02000000"
    ^ "01000000" (* el_count *)
    ^ "14000000" ^ "61000000" ^ "00000000" ^ "00000000"
    ^ "00000000" (* zero*)
    ^ "00000000" ^ "01000000" ^ "00000000" ^ "00000000" ^ "00000000" ^ "00000000" (* nvlist_header *)
    ^ "00000000" ^ "00000000" ^ "" in
  let exp = [ (       "a", Nvlist.NvlistArray [ []])] in
  (fun () ->
     Alcotest.(check @@ result hexcheck string)
    "encode"
    (Ok (Nvlist.unhex v))
    (Nvlist.fnvlist_pack exp)
  ),
  (fun () ->
     Alcotest.(check @@ result nvlist string)
       "decode" (Ok exp) (Nvlist.fnvlist_unpack @@ Nvlist.unhex v)
  )

let test_encode_nvlistarray_single_a,
    test_decode_nvlistarray_single_a
  =
  (* nvlist size: 108 *)
  let v =
    "00010000" ^ "00000000" ^ "01000000" ^ "38000000" ^ "02000000" ^ "01000000"
    ^ "14000000" ^ "61000000" ^ "00000000" ^ "00000000"
    ^ "00000000"
    ^ "00000000" ^ "01000000" ^ "00000000" ^ "00000000" ^ "00000000" ^ "00000000"
    ^ "20000000" ^ "02000000" ^ "01000000" ^ "05000000" ^ "41000000" ^ "00000000" ^ "15000000" ^ "00000000" ^ "00000000"
    ^ "00000000" ^ "" in
  let exp =[ (       "a", Nvlist.NvlistArray [ [ ("A", Int32 (0x15l))]])]  in
  (fun () -> Alcotest.(check @@ result hexcheck string)
    "nvlistarray_single_a"
    (Ok (Nvlist.unhex v))
    (Nvlist.fnvlist_pack exp)),
  (fun () ->
     Alcotest.(check @@ result nvlist string)
       "nvlistarray_single_a"
       (Ok exp)
       (Nvlist.fnvlist_unpack @@ Nvlist.unhex v)
  )


let test_encode_nvlistarray_ab,
    test_decode_nvlistarray_ab =
  (* nvlist size: 176 *)
  let v =
    "00010000" ^ "00000000" ^ "01000000" ^ "58000000" ^ "02000000" ^ "02000000"
    ^ "14000000" ^ "61000000" ^ "00000000" ^ "00000000" ^ "00000000" ^ "00000000" ^ "00000000" ^ "00000000" ^ "01000000" ^ "00000000"
    ^ "00000000" ^ "00000000" ^ "00000000" ^ "00000000" ^ "01000000" ^ "00000000" ^ "00000000" ^ "00000000" ^ "00000000" ^ "20000000"
    ^ "02000000" ^ "01000000" ^ "05000000" ^ "61000000" ^ "00000000" ^ "21000000" ^ "00000000" ^ "00000000" ^ "20000000" ^ "02000000"
    ^ "01000000" ^ "05000000" ^ "62000000" ^ "00000000" ^ "22000000" ^ "00000000" ^ "00000000" ^ "00000000" ^ "" in
  let exp = [ (       "a", Nvlist.NvlistArray [
      [ (       "a", Int32 (0x21l))] ;
      [ (       "b", Int32 (0x22l))]])] in
  (fun () ->
     Alcotest.(check @@ result hexcheck string)
       "nvlistarray_ab"
       (Ok (Nvlist.unhex v))
       (Nvlist.fnvlist_pack @@ exp)
  ),
  (fun () ->
     Alcotest.(check @@ result nvlist string)
       "nvlistarray_ab"
       (Ok exp)
       (Nvlist.fnvlist_unpack @@ (Nvlist.unhex v))
  )

let test_encode_nvlistarray_abc () =
  (* nvlist size: 244 *)
let v =
   "00010000" ^ "00000000" ^ "01000000" ^ "78000000" ^ "02000000" ^ "03000000"
 ^ "14000000" ^ "61000000" ^ "00000000" ^ "00000000" ^ "00000000" ^ "00000000" ^ "00000000" ^ "00000000" ^ "00000000" ^ "00000000"
 ^ "01000000" ^ "00000000" ^ "00000000" ^ "00000000" ^ "00000000" ^ "00000000" ^ "01000000" ^ "00000000" ^ "00000000" ^ "00000000"
 ^ "00000000" ^ "00000000" ^ "01000000" ^ "00000000" ^ "00000000" ^ "00000000" ^ "00000000" ^ "20000000" ^ "02000000" ^ "01000000"
 ^ "05000000" ^ "61000000" ^ "00000000" ^ "15000000" ^ "00000000" ^ "00000000" ^ "20000000" ^ "02000000" ^ "01000000" ^ "05000000"
 ^ "62000000" ^ "00000000" ^ "33000000" ^ "00000000" ^ "00000000" ^ "20000000" ^ "02000000" ^ "01000000" ^ "05000000" ^ "63000000"
 ^ "00000000" ^ "35000000" ^ "00000000" ^ "00000000" ^ "00000000" ^ "" in
Alcotest.(check @@ result hexcheck string)
    "nvlistarray_abc"
(Ok (Nvlist.unhex v))
    (Nvlist.fnvlist_pack @@
     [ (       "a",
               NvlistArray [
                 [ (       "a", Int32 (0x15l))] ;
                 [ (       "b", Int32 (0x33l))] ;
                 [ (       "c", Int32 (0x35l))]])]    )


let test_encode_int16array_9 () =
  (* nvlist size: 64 *)
  let v =
    "00010000" ^ "00000000" ^ "01000000" ^ "30000000" ^ "04000000" ^ "09000000"
    ^ "0b000000" ^ "61626300" ^ "00000000" ^ "01000200" ^ "03000400" ^ "05000600" ^ "07000800" ^ "09000000" ^ "00000000" ^ "00000000"
    ^ "" in
  Alcotest.(check @@ result hexcheck string)
    "int16array_9"
    (Ok (Nvlist.unhex v))
    (Nvlist.fnvlist_pack @@
     [ (     "abc", Int16Array [1;2;3;4;5;6;7;8;9])]    )


let test_encode_int16array_nested_complex,
    test_decode_int16array_nested_complex=
  (* nvlist size: 240 *)
  let v =
    "00010000" ^ "00000000" ^ "01000000" ^ "20000000" ^ "04000000" ^ "02000000"
    ^ "0b000000" ^ "61626300" ^ "00000000" ^ "01000200" ^ "00000000" ^ "20000000" ^ "04000000" ^ "02000000" ^ "0b000000" ^ "64656600"
    ^ "00000000" ^ "03000400" ^ "00000000" ^ "58000000" ^ "04000000" ^ "02000000" ^ "14000000" ^ "78797a00" ^ "00000000" ^ "00000000"
    ^ "00000000" ^ "00000000" ^ "00000000" ^ "00000000" ^ "01000000" ^ "00000000" ^ "00000000" ^ "00000000" ^ "00000000" ^ "00000000"
    ^ "01000000" ^ "00000000" ^ "00000000" ^ "00000000" ^ "00000000" ^ "20000000" ^ "04000000" ^ "02000000" ^ "0b000000" ^ "41424300"
    ^ "00000000" ^ "05000600" ^ "00000000" ^ "00000000" ^ "20000000" ^ "04000000" ^ "02000000" ^ "0b000000" ^ "44454600" ^ "00000000"
    ^ "07000800" ^ "00000000" ^ "00000000" ^ "00000000" ^ "" in
  let exp =[ (     "abc", Nvlist.Int16Array [1;2]);
             (     "def", Int16Array [3;4]);
             (     "xyz", NvlistArray [
                       [ (     "ABC", Int16Array [5;6])];
                       [ (     "DEF", Int16Array [7;8])]])] in
  (fun() ->
     Alcotest.(check @@ result hexcheck string)
       "c_test"
       (Ok (Nvlist.unhex v))
       (Nvlist.fnvlist_pack exp)
  ),
  (fun() ->
     Alcotest.(check @@ result nvlist string)
       "c_test_decode"    (Ok exp)    (Nvlist.fnvlist_unpack @@ Nvlist.unhex v))

let test_decode_empty () =
  Alcotest.(check @@ result nvlist string)
    "decode empty"
    (Ok ([]))
    (Nvlist.fnvlist_unpack
       (Nvlist.unhex @@
        "00010000" (* little-endian, native encoding *)
        ^ "00000000" (* version *)
        ^"01000000" (*magic byte endianness check *)
        ^"00000000" (* length *)
       ))

let test_self x () =
  Alcotest.(check @@ result nvlist string)
    (Fmt.strf "decode(encode(%a))" pp_nvlist x)
    (Ok x)
    (Nvlist.fnvlist_unpack (match Nvlist.fnvlist_pack x with
         | Ok v ->
           Fmt.pr "packed:\n%s\n%!" (Nvlist.hex v) ;
           v
         | Error e -> failwith e))

let test_c_empty () =
  Alcotest.check Alcotest.string "empty nvl"
    "a"
    (Nvlist.to_c [])

let test_c_yo_int32 () =
  let open Nvlist in
  let x = [
    "ab", StrArray (Array.make 3 "hello" |> Array.to_list)
    ]; in
  Alcotest.check Alcotest.string "yo:123l nvl"
    "a"
    (to_c x)

let test_encode_str_array_3_hello,
    test_decode_str_array_3_hello =
  (* nvlist size: 88 *)
  let v =
    "00010000" ^ "00000000" ^ "01000000" ^ "48000000" ^ "03000000" ^ "03000000"
    ^ "11000000" ^ "61620000" ^ "00000000" ^ "00000000" ^ "00000000" ^ "00000000" ^ "00000000" ^ "00000000" ^ "00000000" ^ "68656c6c"
    ^ "6f006865" ^ "6c6c6f00" ^ "68656c6c" ^ "6f000000" ^ "00000000" ^ "00000000" ^ "" in
  let exp =[ "ab", (Nvlist.StrArray ["hello";"hello";"hello"]) ]in
  (fun() ->
     Alcotest.(check @@ result hexcheck string)
       "c_test"
       (Ok (Nvlist.unhex v))
       (Nvlist.fnvlist_pack exp)
  ),
  (fun() ->
     Alcotest.(check @@ result nvlist string)
       "c_test_decode"    (Ok exp)    (Nvlist.fnvlist_unpack @@ Nvlist.unhex v))


let test_encode_str_array_12_hello,
    test_decode_str_array_12_hello =
  (* nvlist size: 208 *)
  let v =
    "00010000" ^ "00000000" ^ "01000000" ^ "c0000000" ^ "03000000" ^ "0c000000"
    ^ "11000000" ^ "61620000" ^ "00000000" ^ "00000000" ^ "00000000" ^ "00000000" ^ "00000000" ^ "00000000" ^ "00000000" ^ "00000000"
    ^ "00000000" ^ "00000000" ^ "00000000" ^ "00000000" ^ "00000000" ^ "00000000" ^ "00000000" ^ "00000000" ^ "00000000" ^ "00000000"
    ^ "00000000" ^ "00000000" ^ "00000000" ^ "00000000" ^ "00000000" ^ "00000000" ^ "00000000" ^ "68656c6c" ^ "6f006865" ^ "6c6c6f00"
    ^ "68656c6c" ^ "6f006865" ^ "6c6c6f00" ^ "68656c6c" ^ "6f006865" ^ "6c6c6f00" ^ "68656c6c" ^ "6f006865" ^ "6c6c6f00" ^ "68656c6c"
    ^ "6f006865" ^ "6c6c6f00" ^ "68656c6c" ^ "6f006865" ^ "6c6c6f00" ^ "00000000" ^ "" in
  let exp =[ (      "ab", (Nvlist.StrArray ["hello";"hello";"hello";"hello";"hello";"hello";"hello";"hello";"hello";"hello";"hello";"hello"]))]in
  (fun() ->
     Alcotest.(check @@ result hexcheck string)
       "c_test"
       (Ok (Nvlist.unhex v))
       (Nvlist.fnvlist_pack exp)
  ),
  (fun() ->
     Alcotest.(check @@ result nvlist string)
       "c_test_decode"    (Ok exp)    (Nvlist.fnvlist_unpack @@ Nvlist.unhex v))

let test_encode_str_array_32,
    test_decode_str_array_32 =
  (* nvlist size: 360 *)
  let v =
    "00010000" ^ "00000000" ^ "01000000" ^ "58010000" ^ "03000000" ^ "20000000"
    ^ "11000000" ^ "61620000" ^ "00000000" ^ "00000000" ^ "00000000" ^ "00000000" ^ "00000000" ^ "00000000" ^ "00000000" ^ "00000000"
    ^ "00000000" ^ "00000000" ^ "00000000" ^ "00000000" ^ "00000000" ^ "00000000" ^ "00000000" ^ "00000000" ^ "00000000" ^ "00000000"
    ^ "00000000" ^ "00000000" ^ "00000000" ^ "00000000" ^ "00000000" ^ "00000000" ^ "00000000" ^ "00000000" ^ "00000000" ^ "00000000"
    ^ "00000000" ^ "00000000" ^ "00000000" ^ "00000000" ^ "00000000" ^ "00000000" ^ "00000000" ^ "00000000" ^ "00000000" ^ "00000000"
    ^ "00000000" ^ "00000000" ^ "00000000" ^ "00000000" ^ "00000000" ^ "00000000" ^ "00000000" ^ "00000000" ^ "00000000" ^ "00000000"
    ^ "00000000" ^ "00000000" ^ "00000000" ^ "00000000" ^ "00000000" ^ "00000000" ^ "00000000" ^ "00000000" ^ "00000000" ^ "00000000"
    ^ "00000000" ^ "00000000" ^ "00000000" ^ "00000000" ^ "00000000" ^ "00000000" ^ "00000000" ^ "31003100" ^ "31003100" ^ "31003100"
    ^ "31003100" ^ "31003100" ^ "31003100" ^ "31003100" ^ "31003100" ^ "31003100" ^ "31003100" ^ "31003100" ^ "31003100" ^ "31003100"
    ^ "31003100" ^ "31003100" ^ "31003100" ^ "00000000" ^ "" in
  let exp =[ (      "ab", (Nvlist.StrArray ["1";"1";"1";"1";"1";"1";"1";"1";"1";"1";"1";"1";"1";"1";"1";"1";"1";"1";"1";"1";"1";"1";"1";"1";"1";"1";"1";"1";"1";"1";"1";"1"]))]in
  (fun() ->
     Alcotest.(check @@ result hexcheck string)
       "c_test"
       (Ok (Nvlist.unhex v))
       (Nvlist.fnvlist_pack exp)
  ),
  (fun() ->
     Alcotest.(check @@ result nvlist string)
       "c_test_decode"    (Ok exp)    (Nvlist.fnvlist_unpack @@ Nvlist.unhex v))


let test_encode_boolean_array,
    test_decode_boolean_array =
  (* nvlist size: 56 *)
  let v =
    "00010000" ^ "00000000" ^ "01000000" ^ "28000000" ^ "04000000" ^ "04000000"
    ^ "18000000" ^ "64756200" ^ "00000000" ^ "01000000" ^ "00000000" ^ "01000000" ^ "01000000" ^ "00000000" ^ "" in
  let v_2_bad =
    (* boolean values should be 0x0000_0001 or 0x0000_0000 ; nothing else.
     * below we set the first element to 0xff000001, which should fail: *)
    "00010000" ^ "00000000" ^ "01000000" ^ "28000000" ^ "04000000" ^ "04000000"
    ^ "18000000" ^ "64756200" ^ "00000000" ^ "010000ff" ^ "00000000" ^ "01000000" ^ "01000000" ^ "00000000" ^ "" in

  (* nvlist size: 48 *)
  let v_empty =
   "00010000" ^ "00000000" ^ "01000000" ^ "20000000" ^ "0b000000" ^ "00000000"
   ^ "18000000" ^ "656d7074" ^ "79656d70" ^ "74790000" ^ "00000000" ^ "00000000" ^ "" in
  let exp =[ "dub", Nvlist.BooleanArray [true;false;true;true] ]in
  let exp_empty =[ "emptyempty", Nvlist.BooleanArray []] in
  (fun() ->
     Alcotest.(check @@ result hexcheck string)
       "tftt"
       (Ok (Nvlist.unhex v))
       (Nvlist.fnvlist_pack exp) ;
     Alcotest.(check @@ result hexcheck string)
       "empty"
       (Ok (Nvlist.unhex v_empty))
       (Nvlist.fnvlist_pack exp_empty)
  ),
  (fun() ->
     Alcotest.(check @@ result nvlist string)
       "tftt"    (Ok exp)    (Nvlist.fnvlist_unpack @@ Nvlist.unhex v);
     Alcotest.(check @@ result nvlist pass)
       "nonexact"    (Error "should be exactly le32 (0 | 1)")
       (Nvlist.fnvlist_unpack @@ Nvlist.unhex v_2_bad);
     Alcotest.(check @@ result nvlist string)
       "empty" (Ok exp_empty) (Nvlist.fnvlist_unpack @@ Nvlist.unhex v_empty))


let test_encode_hrtime,
    test_decode_hrtime =
  (* nvlist size: 48 *)
  let v =
    "00010000" ^ "00000000" ^ "01000000" ^
    "20000000" ^ (* nvlist length *)
    "04000000" ^ (* key namesz of "dub\x00" *)
    "01000000" ^ (* element count (1) *)
    "12000000" ^ (* type tag *)
    "64756200" ^ "00000000" ^ (* key "dub" *)
    "78563412" ^ "78563412" ^ (* 0x1234567812345678_L little-endian *)
    "00000000" in
  let exp =[ "dub", Nvlist.HrTime 1311768465173141112_L ]in
(fun() ->
   Alcotest.(check @@ result hexcheck string)
     "c_test"
     (Ok (Nvlist.unhex v))
     (Nvlist.fnvlist_pack exp)
),
(fun() ->
   Alcotest.(check @@ result nvlist string)
     "c_test_decode"    (Ok exp)    (Nvlist.fnvlist_unpack @@ Nvlist.unhex v))


let test_encode_string_array_short,
    test_decode_string_array_short =
  (* nvlist size: 72 *)
  let v =
    "00010000" ^ "00000000" ^ "01000000" ^
    "38000000" ^ "05000000" ^ "03000000"
    ^ "11000000" ^ "73747273" ^ "00000000" ^
    "00000000" ^ "00000000" ^ "00000000" ^ "00000000" ^ "00000000" ^ "00000000" ^
    "61000062" ^ "00000000" ^
    "00000000" in
  let exp =[ (    "strs", (Nvlist.StrArray ["a";"";"b"]))]in
  (fun() ->
     Alcotest.(check @@ result hexcheck string)
       "string_array_short"
       (Ok (Nvlist.unhex v))
       (Nvlist.fnvlist_pack exp)
  ),
  (fun() ->
     Alcotest.(check @@ result nvlist string)
       "string_array_short"(Ok exp)(Nvlist.fnvlist_unpack @@ Nvlist.unhex v))

let test_encode_boolean_value_f,
    test_decode_boolean_value_f =
  (* nvlist size: 48 *)
  let v =
    "00010000" ^ "00000000" ^ "01000000" ^ "20000000" ^ "02000000" ^ "01000000"
    ^ "15000000" ^ "61000000" ^ "00000000" ^ "00000000" ^ "00000000" ^ "00000000" ^ "" in
  let v_2 = (* with padding: ffff_ffff *)
    "00010000" ^ "00000000" ^ "01000000" ^ "20000000" ^ "02000000" ^ "01000000"
    ^ "15000000" ^ "61000000" ^ "00000000" ^ "00000000" ^ "ffffffff" ^ "00000000" ^ "" in
  let v_3_bad = (* with padding: ffff_ffff *)
    "00010000" ^ "00000000" ^ "01000000" ^ "20000000" ^ "02000000" ^ "01000000"
    ^ "15000000" ^ "61000000" ^ "00000000" ^ "0000ff00" ^ "ffffffff" ^ "00000000" ^ "" in
  let exp =[ (       "a", Nvlist.BooleanValue false)]in
  (fun() ->
     Alcotest.(check @@ result hexcheck string)
       "c_test"
       (Ok (Nvlist.unhex v))
       (Nvlist.fnvlist_pack exp)
  ),
  (fun() ->
     Alcotest.(check @@ result nvlist string)
       "clean"    (Ok exp)    (Nvlist.fnvlist_unpack @@ Nvlist.unhex v) ;
     Alcotest.(check @@ result nvlist string)
       "with padding"    (Ok exp)    (Nvlist.fnvlist_unpack @@ Nvlist.unhex v_2) ;
     Alcotest.(check @@ result nvlist pass)
       "nonexact" (Error "should be 0 or 1")
       (Nvlist.fnvlist_unpack @@ Nvlist.unhex v_3_bad)

  )


let test_encode_boolean_value_t,
    test_decode_boolean_value_t =
  (* nvlist size: 48 *)
  let v =
    "00010000" ^ "00000000" ^ "01000000" ^ "20000000" ^ "02000000" ^ "01000000"
    ^ "15000000" ^ "61000000" ^ "00000000" ^ "01000000" ^ "00000000" ^ "00000000" ^ "" in
  let exp =[ (       "a", Nvlist.BooleanValue true)]in
  (fun() ->
     Alcotest.(check @@ result hexcheck string)
       "c_test"
       (Ok (Nvlist.unhex v))
       (Nvlist.fnvlist_pack exp)
  ),
  (fun() ->
     Alcotest.(check @@ result nvlist string)
       "c_test_decode"    (Ok exp)    (Nvlist.fnvlist_unpack @@ Nvlist.unhex v))

let test_encode_int64_array,
    test_decode_int64_array =
  (* nvlist size: 280 *)
  let v =
    "00010000" ^ "00000000" ^ "01000000" ^ "30000000" ^ "02000000" ^ "03000000"
    ^ "10000000" ^ "61000000" ^ "00000000" ^ "01000000" ^ "00000000" ^ "02000000" ^ "00000000" ^ "03000000" ^ "00000000" ^ "28000000"
    ^ "14000000" ^ "00000000" ^ "10000000" ^ "31323334" ^ "35363738" ^ "39303132" ^ "33343536" ^ "37383900" ^ "00000000" ^ "30000000"
    ^ "02000000" ^ "03000000" ^ "0f000000" ^ "62000000" ^ "00000000" ^ "01000000" ^ "00000000" ^ "02000000" ^ "00000000" ^ "03000000"
    ^ "00000000" ^ "28000000" ^ "15000000" ^ "00000000" ^ "0f000000" ^ "62313233" ^ "34353637" ^ "38393031" ^ "32333435" ^ "36373839"
    ^ "00000000" ^ "58000000" ^ "19000000" ^ "05000000" ^ "10000000" ^ "62626262" ^ "62626262" ^ "62626262" ^ "62626262" ^ "62626262"
    ^ "62626262" ^ "00000000" ^ "00000000" ^ "05000000" ^ "00000000" ^ "06000000" ^ "00000000" ^ "07000000" ^ "00000000" ^ "08000000"
    ^ "00000000" ^ "09000000" ^ "00000000" ^ "00000000" ^ "" in
  let exp =[
    (       "a", Nvlist.Uint64Array [1_L;2_L;3_L]);
    ("1234567890123456789", Uint64Array []);
    (       "b", Int64Array [1_L;2_L;3_L]);
    ("b1234567890123456789", Int64Array []);
    ("bbbbbbbbbbbbbbbbbbbbbbbb", Uint64Array [5_L;6_L;7_L;8_L;9_L])]in
  (fun() ->
     Alcotest.(check @@ result hexcheck string)
       "c_test"
       (Ok (Nvlist.unhex v))
       (Nvlist.fnvlist_pack exp)
  ),
  (fun() ->
     Alcotest.(check @@ result nvlist string)
       "c_test_decode"    (Ok exp)    (Nvlist.fnvlist_unpack @@ Nvlist.unhex v))


let test_encode_complex2,
    test_decode_complex2 =
  (* nvlist size: 104 *)
  let v =
    "00010000" ^ "00000000" ^ "01000000" ^ "28000000" ^ "0f000000" ^ "01000000"
    ^ "08000000" ^ "61626364" ^ "65666768" ^ "696a6b6c" ^ "6d6e0000" ^ "4e61bc00" ^ "00000000" ^ "30000000" ^ "0b000000" ^ "01000000"
    ^ "09000000" ^ "73313233" ^ "34353637" ^ "38390000" ^ "00000000" ^ "31323334" ^ "35363738" ^ "39000000" ^ "00000000" ^ "00000000"
    ^ "" in
  let exp =[ ("abcdefghijklmn", Nvlist.Uint64 (12345678_L));
             ("s123456789", Str "123456789")]in
  (fun() ->
     Alcotest.(check @@ result hexcheck string)
       "c_test"
       (Ok (Nvlist.unhex v))
       (Nvlist.fnvlist_pack exp)
  ),
  (fun() ->
     Alcotest.(check @@ result nvlist string)
       "c_test_decode"    (Ok exp)    (Nvlist.fnvlist_unpack @@ Nvlist.unhex v))


let test_encode_complex1,
    test_decode_complex1 =
  (* nvlist size: 272 *)
  let v =
    "00010000" ^ "00000000" ^ "01000000" ^ "28000000" ^ "0f000000" ^ "01000000"
    ^ "06000000" ^ "61626364" ^ "65666768" ^ "696a6b6c" ^ "6d6e0000" ^ "4e61bc00" ^ "00000000" ^ "20000000" ^ "02000000" ^ "01000000"
    ^ "05000000" ^ "62000000" ^ "00000000" ^ "02000000" ^ "00000000" ^ "20000000" ^ "03000000" ^ "01000000" ^ "09000000" ^ "73310000"
    ^ "00000000" ^ "00000000" ^ "00000000" ^ "20000000" ^ "05000000" ^ "01000000" ^ "09000000" ^ "73313233" ^ "00000000" ^ "31323300"
    ^ "00000000" ^ "20000000" ^ "06000000" ^ "01000000" ^ "09000000" ^ "73313233" ^ "34000000" ^ "31323334" ^ "00000000" ^ "28000000"
    ^ "09000000" ^ "01000000" ^ "09000000" ^ "73313233" ^ "34353637" ^ "00000000" ^ "00000000" ^ "31323334" ^ "35363700" ^ "30000000"
    ^ "0a000000" ^ "01000000" ^ "09000000" ^ "73313233" ^ "34353637" ^ "38000000" ^ "00000000" ^ "31323334" ^ "35363738" ^ "00000000"
    ^ "00000000" ^ "00000000" ^ "" in
  let exp =[
    ("abcdefghijklmn", Nvlist.Uint32 (12345678_l));
    (       "b", Int32 (0x2l));
    (      "s1", Str "");
    (    "s123", Str "123");
    (   "s1234", Str "1234");
    ("s1234567", Str "1234567");
    ("s12345678", Str "12345678")]in
  (fun() ->
     Alcotest.(check @@ result hexcheck string)
       "c_test"
       (Ok (Nvlist.unhex v))
       (Nvlist.fnvlist_pack exp)
  ),
  (fun() ->
     Alcotest.(check @@ result nvlist string)
       "c_test_decode"    (Ok exp)    (Nvlist.fnvlist_unpack @@ Nvlist.unhex v))


let test_encode_uint16array_libnv,
    test_decode_uint16array_libnv =
  (* nvlist size: 72 *)
  let v =
      "00010000" ^ "00000000" ^ "01000000"
      ^ "38000000" (* diff *)
      ^ "02000000" ^ "10000000"
    ^ "0c000000"
    ^ "7a000000" ^ "00000000"
    ^ "17ffffff" ^ "ffffff00" ^ "01000000"
    ^ "00000001"
    ^ "00000020" ^ "00f0ff03" ^ "00000008"
    ^ "00000019" ^ "00000000" in
  let v_2 = (* same as v, but different padding *)
    Nvlist.unhex @@
    "00010000" ^ "00000000" ^ "01000000"
    ^ "38000000"
    ^ "02000000" ^ "10000000"
    ^ "0c000000"
    ^ "7a00c1c2" ^ "c3c4c5c6" (* diff padding *)
    ^ "17ffffff" ^ "ffffff00" ^ "01000000"
    ^ "00000001"
    ^ "00000020" ^ "00f0ff03" ^ "00000008"
    ^ "00000019" ^ "00000000"
  in
  let v_bad =
    (* libnvpair's fnvlist_unpack rejects this: *)
    Nvlist.unhex @@
        "00010000" ^ "00000000" ^ "01000000"
        ^ "20000000" (* diff nvlist length *)
        ^ "02000000" ^ "10000000"
      ^ "0c000000"
      ^ "7a000000" ^ "00000000"
      ^ "17ffffff" ^ "ffffff00" ^ "01000000"
      ^ "00000001"
      ^ "00000020" ^ "00f0ff03" ^ "00000008"
      ^ "00000019" ^ "00000000"
  in
  let exp =[ (       "z", Nvlist.Uint16Array [
      65303;65535;65535;255;1;0;0;256;0;8192;61440;1023;0;2048;0;6400])]in
  (fun() ->
     Alcotest.(check @@ result hexcheck string)
       "c_test"
       (Ok (Nvlist.unhex v))
       (Nvlist.fnvlist_pack exp)
  ),
  (fun() ->
     Alcotest.(check @@ result nvlist string)
       "c_test_decode" (Ok exp)
       (Nvlist.fnvlist_unpack @@ Nvlist.unhex v) ;
     Alcotest.(check @@ result nvlist string)
       "decode v_2" (Ok exp)
       (Nvlist.fnvlist_unpack @@ v_2) ;
     Alcotest.(check @@ result nvlist string)
       "bad"
       (Error "bad appendix length")
       (Nvlist.fnvlist_unpack v_bad)
  )


let test_encode_int8array_18,
    test_decode_int8array_18 =
  (* nvlist size: 64 *)
  let v =
    "00010000" ^ "00000000" ^ "01000000" ^ "30000000" ^ "03000000" ^ "12000000"
    ^ "19000000" ^ "796f0000" ^ "00000000" ^ "09000000" ^ "00ffff00" ^ "20000000" ^ "03000000" ^ "01000000" ^ "00000000" ^ "00000000"
    ^ "" in
  let exp =[ "yo",
             Nvlist.Int8Array [9;0;0;0;0;-1;-1;0;32;0;0;0;3;0;0;0;1;0]]in
  (fun() ->
     Alcotest.(check @@ result hexcheck string)
       "int8array_18_encode"
       (Ok (Nvlist.unhex v))
       (Nvlist.fnvlist_pack exp)
  ),
  (fun() ->
     Alcotest.(check @@ result nvlist string)
       "int8array_18_decode"    (Ok exp)    (Nvlist.fnvlist_unpack @@ Nvlist.unhex v))

let test_encode_string_97,
    test_decode_string_97 =
  (* nvlist size: 144 *)
  let v =
    "00010000" ^ "00000000" ^ "01000000" ^ "80000000" ^ "04000000" ^ "01000000"
    ^ "09000000" ^ "61626300" ^ "00000000" ^ "41414141" ^ "41414141" ^ "41414141" ^ "41414141" ^ "41414141" ^ "41414141" ^ "41414141"
    ^ "41414141" ^ "41414141" ^ "41414141" ^ "41414141" ^ "41414141" ^ "41414141" ^ "41414141" ^ "41414141" ^ "41414141" ^ "41414141"
    ^ "41414141" ^ "41414141" ^ "41414141" ^ "41414141" ^ "41414141" ^ "41414141" ^ "41414141" ^ "41000000" ^ "00000000" ^ "00000000"
    ^ "" in
  let exp =[ (     "abc", Nvlist.Str "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA")]in
  (fun() ->
     Alcotest.(check @@ result hexcheck string)
       "c_test"
       (Ok (Nvlist.unhex v))
       (Nvlist.fnvlist_pack exp)
  ),
  (fun() ->
     Alcotest.(check @@ result nvlist string)
       "c_test_decode"    (Ok exp)    (Nvlist.fnvlist_unpack @@ Nvlist.unhex v))


let test_encode_string,
    test_decode_string =
  (* nvlist size: 48 *)
  let v =
    "00010000" ^ "00000000" ^ "01000000" ^ "20000000" ^ "04000000" ^ "01000000"
    ^ "09000000" ^ "61626300" ^ "00000000" ^ "41424300" ^ "00000000" ^ "00000000" ^ "" in
  let exp =[ (     "abc", Nvlist.Str "ABC")]in
  (fun() ->
     Alcotest.(check @@ result hexcheck string)
       "encode_string"
       (Ok (Nvlist.unhex v))
       (Nvlist.fnvlist_pack exp)
  ),
  (fun() ->
     Alcotest.(check @@ result nvlist string)
       "decode_string"    (Ok exp)    (Nvlist.fnvlist_unpack @@ Nvlist.unhex v))

let test_encode_string_empty,
    test_decode_string_empty =
  (* nvlist size: 48 *)
  let v =
    "00010000" ^ "00000000" ^ "01000000" ^ "20000000" ^ "04000000" ^ "01000000"
    ^ "09000000" ^ "61626300" ^ "00000000" ^ "00000000" ^ "00000000" ^ "00000000" ^ "" in
  let exp =[ (     "abc", Nvlist.Str "")]in
  (fun() ->
     Alcotest.(check @@ result hexcheck string)
       "c_test"
       (Ok (Nvlist.unhex v))
       (Nvlist.fnvlist_pack exp)
  ),
  (fun() ->
     Alcotest.(check @@ result nvlist string)
       "c_test_decode"    (Ok exp)    (Nvlist.fnvlist_unpack @@ Nvlist.unhex v))

let test_encode_string_7,
    test_decode_string_7 =
  (* nvlist size: 48 *)
  let v =
    "00010000" ^ "00000000" ^ "01000000" ^ "20000000" ^ "04000000" ^ "01000000"
    ^ "09000000" ^ "61626300" ^ "00000000" ^ "31323334" ^ "35363700" ^ "00000000" ^ "" in
  let exp =[ (     "abc", Nvlist.Str "1234567")]in
  (fun() ->
     Alcotest.(check @@ result hexcheck string)
       "c_test"
       (Ok (Nvlist.unhex v))
       (Nvlist.fnvlist_pack exp)
  ),
  (fun() ->
     Alcotest.(check @@ result nvlist string)
       "c_test_decode"    (Ok exp)    (Nvlist.fnvlist_unpack @@ Nvlist.unhex v))

let test_encode_string_8,
    test_decode_string_8 =
  (* nvlist size: 56 *)
  let v =
    "00010000" ^ "00000000" ^ "01000000" ^ "28000000" ^ "04000000" ^ "01000000"
    ^ "09000000" ^ "61626300" ^ "00000000" ^ "31323334" ^ "35363738" ^ "00000000" ^ "00000000" ^ "00000000" ^ "" in
  let exp =[ (     "abc", Nvlist.Str "12345678")]in
  (fun() ->
     Alcotest.(check @@ result hexcheck string)
       "c_test"
       (Ok (Nvlist.unhex v))
       (Nvlist.fnvlist_pack exp)
  ),
  (fun() ->
     Alcotest.(check @@ result nvlist string)
       "c_test_decode"    (Ok exp)    (Nvlist.fnvlist_unpack @@ Nvlist.unhex v))

let test_encode_nest2_nonempty_unalign,
    test_decode_nest2_nonempty_unalign =
  (* nvlist size: 300 *)
  let v =
    "00010000" ^ "00000000" ^ "01000000" ^ "30000000" ^ "06000000" ^ "01000000"
    ^ "13000000" ^ "6e657374" ^ "31000000" ^ "00000000" ^ "01000000" ^ "00000000" ^ "00000000" ^ "00000000" ^ "00000000" ^ "30000000"
    ^ "08000000" ^ "01000000" ^ "13000000" ^ "6e657374" ^ "325f6100" ^ "00000000" ^ "01000000" ^ "00000000" ^ "00000000" ^ "00000000"
    ^ "00000000" ^ "20000000" ^ "06000000" ^ "01000000" ^ "09000000" ^ "325f615f" ^ "61000000" ^ "68657900" ^ "00000000" ^ "20000000"
    ^ "06000000" ^ "01000000" ^ "08000000" ^ "325f615f" ^ "62000000" ^ "b168de3a" ^ "00000000" ^ "00000000" ^ "30000000" ^ "08000000"
    ^ "01000000" ^ "13000000" ^ "6e657374" ^ "325f6200" ^ "00000000" ^ "01000000" ^ "00000000" ^ "00000000" ^ "00000000" ^ "00000000"
    ^ "20000000" ^ "06000000" ^ "01000000" ^ "05000000" ^ "325f625f" ^ "61000000" ^ "20000000" ^ "00000000" ^ "20000000" ^ "06000000"
    ^ "01000000" ^ "09000000" ^ "325f625f" ^ "62000000" ^ "325f625f" ^ "62000000" ^ "00000000" ^ "00000000" ^ "00000000" ^ "" in
  let exp =[ (   "nest1", Nvlist.Nvlist  [
      ( "nest2_a", Nvlist  [
            ("2_a_a", Str "hey");
            (   "2_a_b", Uint64 (0x3ade68b1_L))]);
      ( "nest2_b", Nvlist  [
            (   "2_b_a", Int32 (0x20l));
            (   "2_b_b", Str "2_b_b")]
      )]
    )]in
  (fun() ->
     Alcotest.(check @@ result hexcheck string)
       "c_test"
       (Ok (Nvlist.unhex v))
       (Nvlist.fnvlist_pack exp)
  ),
  (fun() ->
     Alcotest.(check @@ result nvlist string)
       "c_test_decode"    (Ok exp)    (Nvlist.fnvlist_unpack @@ Nvlist.unhex v))


let test_encode_int16array_0,
    test_decode_int16array_0 =
  (* nvlist size: 40 *)
  let v =
    "00010000" ^ "00000000" ^ "01000000" ^ "18000000" ^ "04000000" ^ "00000000"
    ^ "0b000000" ^ "61626300" ^ "00000000" ^ "00000000" ^ "" in
  let exp =[ (     "abc", Nvlist.Int16Array [])]in
  (fun() ->
     Alcotest.(check @@ result hexcheck string)
       "c_test"
       (Ok (Nvlist.unhex v))
       (Nvlist.fnvlist_pack exp)
  ),
  (fun() ->
     Alcotest.(check @@ result nvlist string)
       "c_test_decode"    (Ok exp)    (Nvlist.fnvlist_unpack @@ Nvlist.unhex v))

let test_encode_int32s,
    test_decode_int32s =
  (* nvlist size: 176 *)
  let v =
    "00010000" ^ "00000000" ^ "01000000" ^ "20000000" ^ "02000000" ^ "01000000"
    ^ "05000000" ^ "61000000" ^ "00000000" ^ "05000000" ^ "00000000" ^ "20000000" ^ "02000000" ^ "01000000" ^ "05000000" ^ "62000000"
    ^ "00000000" ^ "06000000" ^ "00000000" ^ "20000000" ^ "02000000" ^ "01000000" ^ "05000000" ^ "63000000" ^ "00000000" ^ "07000000"
    ^ "00000000" ^ "20000000" ^ "02000000" ^ "01000000" ^ "06000000" ^ "64000000" ^ "00000000" ^ "08000000" ^ "00000000" ^ "20000000"
    ^ "02000000" ^ "01000000" ^ "06000000" ^ "65000000" ^ "00000000" ^ "09000000" ^ "00000000" ^ "00000000" ^ "" in
  let exp =[ (       "a", Nvlist.Int32 (0x5l));
             (       "b", Int32 (0x6l));
             (       "c", Int32 (0x7l));
             (       "d", Uint32 (8l));
             (       "e", Uint32 (9l))]in
  (fun() ->
     Alcotest.(check @@ result hexcheck string)
       "c_test"
       (Ok (Nvlist.unhex v))
       (Nvlist.fnvlist_pack exp)
  ),
  (fun() ->
     Alcotest.(check @@ result nvlist string)
       "c_test_decode"    (Ok exp)    (Nvlist.fnvlist_unpack @@ Nvlist.unhex v))


let test_bytearray_empty_encode,
    test_bytearray_empty_decode
  =
  (* nvlist size: 40 *)
  let v =
    "00010000" ^ "00000000" ^ "01000000" ^ "18000000" ^ "02000000" ^ "00000000"
    ^ "0a000000" ^ "62000000" ^ "00000000" ^ "00000000" ^ "" in
  let exp =[ (       "b", Nvlist.ByteArray "")]in
  (fun() ->
     Alcotest.(check @@ result hexcheck string)
       "bytearray_empty_encode"
       (Ok (Nvlist.unhex v))
       (Nvlist.fnvlist_pack exp)
  ),
  (fun() ->
     Alcotest.(check @@ result nvlist string)
       "bytearray_empty_encode" (Ok exp)
       (Nvlist.fnvlist_unpack @@ Nvlist.unhex v))

let test_bytearray_1_encode,
    test_bytearray_1_decode =
  (* nvlist size: 48 *)
  let v =
    "00010000" ^ "00000000" ^ "01000000" ^ "20000000" ^ "02000000" ^ "01000000"
    ^ "0a000000" ^ "62000000" ^ "00000000" ^ "30000000" ^ "00000000" ^ "00000000" ^ "" in
  let exp =[ (       "b", Nvlist.ByteArray "0")]in
  (fun() ->
     Alcotest.(check @@ result hexcheck string)
       "c_test"
       (Ok (Nvlist.unhex v))
       (Nvlist.fnvlist_pack exp)
  ),
  (fun() ->
     Alcotest.(check @@ result nvlist string)
       "c_test_decode"    (Ok exp)    (Nvlist.fnvlist_unpack @@ Nvlist.unhex v))

let test_bytearray_2_encode,
    test_bytearray_2_decode =
  (* nvlist size: 48 *)
  let v =
    "00010000" ^ "00000000" ^ "01000000" ^ "20000000" ^ "02000000" ^ "02000000"
    ^ "0a000000" ^ "62000000" ^ "00000000" ^ "30310000" ^ "00000000" ^ "00000000" ^ "" in
  let exp =[ (       "b", Nvlist.ByteArray "01")]in
  (fun() ->
     Alcotest.(check @@ result hexcheck string)
       "c_test"
       (Ok (Nvlist.unhex v))
       (Nvlist.fnvlist_pack exp)
  ),
  (fun() ->
     Alcotest.(check @@ result nvlist string)
       "c_test_decode"    (Ok exp)    (Nvlist.fnvlist_unpack @@ Nvlist.unhex v))

let test_bytearray_3_encode,
    test_bytearray_3_decode =
  (* nvlist size: 48 *)
  let v =
    "00010000" ^ "00000000" ^ "01000000" ^ "20000000" ^ "02000000" ^ "03000000"
    ^ "0a000000" ^ "62000000" ^ "00000000" ^ "30313200" ^ "00000000" ^ "00000000" ^ "" in
  let exp =[ (       "b", Nvlist.ByteArray "012")]in
  (fun() ->
     Alcotest.(check @@ result hexcheck string)
       "c_test"
       (Ok (Nvlist.unhex v))
       (Nvlist.fnvlist_pack exp)
  ),
  (fun() ->
     Alcotest.(check @@ result nvlist string)
       "c_test_decode"    (Ok exp)    (Nvlist.fnvlist_unpack @@ Nvlist.unhex v))

let test_bytearray_4_encode,
    test_bytearray_4_decode =
(* nvlist size: 48 *)
let v =
   "00010000" ^ "00000000" ^ "01000000" ^ "20000000" ^ "02000000" ^ "04000000"
 ^ "0a000000" ^ "62000000" ^ "00000000" ^ "30313233" ^ "00000000" ^ "00000000" ^ "" in
let exp =[ (       "b", Nvlist.ByteArray "0123")]in
(fun() ->
  Alcotest.(check @@ result hexcheck string)
    "c_test"
    (Ok (Nvlist.unhex v))
    (Nvlist.fnvlist_pack exp)
),
(fun() ->
  Alcotest.(check @@ result nvlist string)
    "c_test_decode"    (Ok exp)    (Nvlist.fnvlist_unpack @@ Nvlist.unhex v))

let test_bytearray_7_encode,
    test_bytearray_7_decode =
  (* nvlist size: 48 *)
  let v =
    "00010000" ^ "00000000" ^ "01000000" ^ "20000000" ^ "02000000" ^ "07000000"
    ^ "0a000000" ^ "62000000" ^ "00000000" ^ "30313233" ^ "34353600" ^ "00000000" ^ "" in
  let exp =[ (       "b", Nvlist.ByteArray "0123456")]in
  (fun() ->
     Alcotest.(check @@ result hexcheck string)
       "c_test"
       (Ok (Nvlist.unhex v))
       (Nvlist.fnvlist_pack exp)
  ),
  (fun() ->
     Alcotest.(check @@ result nvlist string)
       "c_test_decode"    (Ok exp)    (Nvlist.fnvlist_unpack @@ Nvlist.unhex v))

let test_bytearray_8_encode,
    test_bytearray_8_decode =
  (* nvlist size: 48 *)
  let v =
    "00010000" ^ "00000000" ^ "01000000" ^ "20000000" ^ "02000000" ^ "08000000"
    ^ "0a000000" ^ "62000000" ^ "00000000" ^ "30313233" ^ "34353637" ^ "00000000" ^ "" in
  let exp =[ (       "b", Nvlist.ByteArray "01234567")]in
  (fun() ->
     Alcotest.(check @@ result hexcheck string)
       "c_test"
       (Ok (Nvlist.unhex v))
       (Nvlist.fnvlist_pack exp)
  ),
  (fun() ->
     Alcotest.(check @@ result nvlist string)
       "c_test_decode"    (Ok exp)    (Nvlist.fnvlist_unpack @@ Nvlist.unhex v))

let test_bytearray_9_encode,
    test_bytearray_9_decode =
  (* nvlist size: 56 *)
  let v =
    "00010000" ^ "00000000" ^ "01000000" ^ "28000000" ^ "02000000" ^ "09000000"
    ^ "0a000000" ^ "62000000" ^ "00000000" ^ "30313233" ^ "34353637" ^ "38000000" ^ "00000000" ^ "00000000" ^ "" in
  let exp =[ (       "b", Nvlist.ByteArray "012345678")]in
  (fun() ->
     Alcotest.(check @@ result hexcheck string)
       "c_test"
       (Ok (Nvlist.unhex v))
       (Nvlist.fnvlist_pack exp)
  ),
  (fun() ->
     Alcotest.(check @@ result nvlist string)
       "c_test_decode"    (Ok exp)    (Nvlist.fnvlist_unpack @@ Nvlist.unhex v))

let test_encode_duplicate () =
  let bad = [ "a", Nvlist.Boolean; "a", Boolean ] in
  let bad_nest = [ "a", Nvlist.Nvlist [
      "b", Boolean ;
      "b", BooleanValue false ;
    ] ] in
  let bad_arr = [ "a", Nvlist.NvlistArray [
      ["b", Boolean ; "b", BooleanValue false ;]
    ] ] in
  let good_arr =
    (* no collisions here *)
    [ "a", Nvlist.NvlistArray [
          ["a", Nvlist ["a", Boolean] ;
           "b", NvlistArray [["a", Str "x"]]
          ];
      ["a", BooleanValue false ;]
        ] ] in
  let good_simple = [
    "a", Nvlist.Nvlist [
      "a", Boolean ;
      "b", Boolean ;
    ] ;
    "b", Nvlist.Nvlist [
      "a", Boolean ;
      "b", Boolean ;
    ]
  ] in
  let libnvlist_good =
    (* This one decodes to:
nvlist version: 0
uint64:	"ab" = 0xffff00000000
uint64:	"ab" = 0x1e00000000000001

*)
    Nvlist.unhex @@
    "00010000" ^ (* native, little-endian; reserved1=0x00; reserved2=0x200 *)
    "0000000001000000" ^
    "20000000" ^ (* first appendix =32=0x20 *)
    "030000000100000008000000" ^ (* namesz=3; elcnt=1, typ=Uint64(x08) *)
    "6162000000000000" ^ (* "ab\x00" + pad *)
    "00000000ffff0000" ^ (* 0x0000ffff00000000_L *)
    "20000000" ^ (* second appendix =32=0x20 *)
    "030000000100000008000000" ^ (* namesz=3l; elcnt=1; typ=Uint64 *)
    "6162000000000000" ^ (* "ab\x00" + pad *)
    "010000000000001e" ^ (* 0x1e00000000000001_L *)
    "00000000" (* end marker, third appendix = 0 *)
  in
  Alcotest.(check @@ result nvlist string)
    "libnvlist says this is good"
    (Ok ["ab", Uint64 0xffff00000000_L ;
         "ab", Uint64 0x1e00000000000001_L ;
        ])
    (Nvlist.fnvlist_unpack libnvlist_good) ;

  Alcotest.(check @@ result hexcheck string)
    "good_arr:no collisions here despite lots of As"
    (Ok (Nvlist.unhex "000100000000000001000000580000000200000002000000140000006100000000000000000000000000000000000000000000000000000001000000000000000000000000000000000000000000000001000000000000000000000000000000000000003000000002000000010000001300000061000000000000000000000001000000000000000000000000000000000000001800000002000000000000000100000061000000000000000000000038000000020000000100000014000000620000000000000000000000000000000000000001000000000000000000000000000000000000002000000002000000010000000900000061000000000000007800000000000000000000000000000020000000020000000100000015000000610000000000000000000000000000000000000000000000"))
    (Nvlist.fnvlist_pack good_arr) ;
  Alcotest.(check @@ result hexcheck string)
    "good_simple:no collisions here"
    (Ok (Nvlist.unhex "000100000000000001000000300000000200000001000000130000006100000000000000000000000100000000000000000000000000000000000000180000000200000000000000010000006100000000000000180000000200000000000000010000006200000000000000000000003000000002000000010000001300000062000000000000000000000001000000000000000000000000000000000000001800000002000000000000000100000061000000000000001800000002000000000000000100000062000000000000000000000000000000"))
    (Nvlist.fnvlist_pack good_simple) ;
  Alcotest.(check @@ result hexcheck pass)
    "bad_arr:invalid due to unique flag"
    (Error "invalid due to unique flag")
    (Nvlist.fnvlist_pack bad_arr) ;
  Alcotest.(check @@ result hexcheck pass)
    "bad_nest:invalid due to unique flag"
    (Error "invalid due to unique flag")
    (Nvlist.fnvlist_pack bad_nest) ;
  Alcotest.(check @@ result hexcheck pass)
    "bad:invalid due to unique flag"
    (Error "invalid due to unique flag")
    (Nvlist.fnvlist_pack bad)

let test_decode_duplicate () =
  let invalid_booleans = Nvlist.unhex @@
    "00010000000000000100000018000000020000000000000001000000610000000000000018000000020000000000000001000000610000000000000000000000"
  in
  let valid_booleans = Nvlist.unhex @@
    "00010000000000000000000018000000020000000000000001000000610000000000000018000000020000000000000001000000610000000000000000000000"
  in
  let invalid_nest =
    (* TODO libnv seems happy with this *)
    Nvlist.unhex @@
    "00010000000000000100000030000000020000000100000013000000610000000000000000000000010000000000000000000000000000000000000018000000020000000000000001000000620000000000000020000000020000000100000015000000620000000000000000000000000000000000000000000000"
  in
  let invalid_nest_2 = Nvlist.unhex @@
    "00010000000000000000000030000000020000000100000013000000610000000000000000000000010000000000000000000000000000000000000018000000020000000000000001000000620000000000000020000000020000000100000015000000620000000000000000000000000000000000000000000000"
  in
  let valid_nest = Nvlist.unhex @@
    "00010000000000000000000030000000020000000100000013000000610000000000000000000000000000000000000000000000000000000000000018000000020000000000000001000000620000000000000020000000020000000100000015000000620000000000000000000000000000000000000000000000"
  in
  let good_arr = Nvlist.unhex @@
    "000100000000000001000000580000000200000002000000140000006100000000000000000000000000000000000000000000000000000001000000000000000000000000000000000000000000000001000000000000000000000000000000000000003000000002000000010000001300000061000000000000000000000001000000000000000000000000000000000000001800000002000000000000000100000061000000000000000000000038000000020000000100000014000000620000000000000000000000000000000000000001000000000000000000000000000000000000002000000002000000010000000900000061000000000000007800000000000000000000000000000020000000020000000100000015000000610000000000000000000000000000000000000000000000"
  in
  let bad_arr = Nvlist.unhex @@
    "000100000000000001000000580000000200000002000000140000006100000000000000000000000000000000000000000000000000000001000000000000000000000000000000000000000000000001000000000000000000000000000000000000003000000002000000010000001300000061000000000000000000000001000000000000000000000000000000000000001800000002000000000000000100000061000000000000000000000038000000020000000100000014000000610000000000000000000000000000000000000001000000000000000000000000000000000000002000000002000000010000000900000061000000000000007800000000000000000000000000000020000000020000000100000015000000610000000000000000000000000000000000000000000000"
  in
  let bad_arr_2 =
    (* no global unique flag, but there is a local one*)
    Nvlist.unhex @@
    "000100000000000000000000580000000200000002000000140000006100000000000000000000000000000000000000000000000000000001000000000000000000000000000000000000000000000001000000000000000000000000000000000000003000000002000000010000001300000061000000000000000000000001000000000000000000000000000000000000001800000002000000000000000100000061000000000000000000000038000000020000000100000014000000610000000000000000000000000000000000000001000000000000000000000000000000000000002000000002000000010000000900000061000000000000007800000000000000000000000000000020000000020000000100000015000000610000000000000000000000000000000000000000000000"
  in
  let bad_arr_3 =
    (* no local unique flag, but there is a global one *)
    Nvlist.unhex @@
    "000100000000000001000000580000000200000002000000140000006100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001000000000000000000000000000000000000003000000002000000010000001300000061000000000000000000000001000000000000000000000000000000000000001800000002000000000000000100000061000000000000000000000038000000020000000100000014000000610000000000000000000000000000000000000001000000000000000000000000000000000000002000000002000000010000000900000061000000000000007800000000000000000000000000000020000000020000000100000015000000610000000000000000000000000000000000000000000000"
  in
  let good_arr_2 =
    (* like bad_arr, but with the unique flags off *)
    Nvlist.unhex @@
    "000100000000000000000000580000000200000002000000140000006100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001000000000000000000000000000000000000003000000002000000010000001300000061000000000000000000000001000000000000000000000000000000000000001800000002000000000000000100000061000000000000000000000038000000020000000100000014000000610000000000000000000000000000000000000001000000000000000000000000000000000000002000000002000000010000000900000061000000000000007800000000000000000000000000000020000000020000000100000015000000610000000000000000000000000000000000000000000000"
  in
  let good_simple = Nvlist.unhex @@
    "000100000000000001000000300000000200000001000000130000006100000000000000000000000100000000000000000000000000000000000000180000000200000000000000010000006100000000000000180000000200000000000000010000006200000000000000000000003000000002000000010000001300000062000000000000000000000001000000000000000000000000000000000000001800000002000000000000000100000061000000000000001800000002000000000000000100000062000000000000000000000000000000"
  in
  let bad_simple =
    (* {a:{a:,a:}, b:{a:,:a}} global unique: off
       local unique_1: on
       local_unique_2: off
    *)
    Nvlist.unhex @@
    "0001000000000000000000003000000002000000010000001300000061000000000000000000000001000000000000000000000x0000000000000000180000000200000000000000010000006100000000000000180000000200000000000000010000006100000000000000000000003000000002000000010000001300000062000000000000000000000000000000000000000000000000000000000000001800000002000000000000000100000061000000000000001800000002000000000000000100000061000000000000000000000000000000"
  in
  let good_simple_2 =
    (* {a:{a:,a:}, a:{a:,:a}} global unique: off
       local unique_1: off
       local_unique_2: off
    *)
    Nvlist.unhex @@
    "000100000000000000000000300000000200000001000000130000006100000000000000000000000000000000000000000000000000000000000000180000000200000000000000010000006100000000000000180000000200000000000000010000006100000000000000000000003000000002000000010000001300000061000000000000000000000000000000000000000000000000000000000000001800000002000000000000000100000061000000000000001800000002000000000000000100000061000000000000000000000000000000"
  in
  let nv_good_1 =
    (* libnvpair has no problem with unique values in this, but we used to:*)
    Nvlist.unhex @@
    "00013030303030003030303020000000040030300100000009000000f8303000303030303030303030300e002000000004003030020000001a0000003033300064303030303030dd3030303020000000040030300100000009000000f03030003030cf303030300030303030200000000400303001000000090000000a3030003030303030303030303030002000000004000000010000001a0000003033300030303030303030dd30304d6200000000040030300100000009000000f83030003030cf303001303044303002200000000000303030303030303030303030303030303000200000000000"
  in
  Alcotest.(check @@ result pass string)
    "nv_good_1 : should work"
    (Ok [])
    (Nvlist.fnvlist_unpack nv_good_1) ;
  Alcotest.(check @@ result nvlist string)
    "a:boolean,a:boolean - no unique flag"
    (Ok ["a", Boolean; "a", Boolean])
    (Nvlist.fnvlist_unpack valid_booleans) ;
  Alcotest.(check @@ result nvlist pass)
    "a:boolean,a:boolean"
    (Error "unique constraint violated")
    (Nvlist.fnvlist_unpack invalid_booleans) ;
  Alcotest.(check @@ result nvlist string)
    "valid_nest"
    (Ok ["a", Nvlist.Nvlist ["b", Boolean; "b", BooleanValue false]])
    (Nvlist.fnvlist_unpack valid_nest) ;
  Alcotest.(check @@ result nvlist pass)
    "invalid_nest"
    (Error "unique constraint violated")
    (Nvlist.fnvlist_unpack invalid_nest) ;
  Alcotest.(check @@ result nvlist pass)
    "invalid_nest_2: no global unique flag, but there is a local one"
    (Error "unique constraint violated")
    (Nvlist.fnvlist_unpack invalid_nest_2) ;
  Alcotest.(check @@ result pass string)
    "good_arr: no collisions"
    (Ok [])
    (Nvlist.fnvlist_unpack good_arr) ;
  Alcotest.(check @@ result nvlist pass)
    "bad_arr : collisions"
    (Error "not good")
    (Nvlist.fnvlist_unpack bad_arr) ;
  Alcotest.(check @@ result nvlist pass)
    "bad_arr_2 : collisions"
    (Error "not good")
    (Nvlist.fnvlist_unpack bad_arr_2) ;
  Alcotest.(check @@ result nvlist pass)
    "bad_arr_3 : collisions"
    (Error "not good")
    (Nvlist.fnvlist_unpack bad_arr_3) ;
  Alcotest.(check @@ result pass string)
    "good_arr: no collisions"
    (Ok [])
    (Nvlist.fnvlist_unpack good_arr_2) ;
  Alcotest.(check @@ result pass string)
    "good_simple: no collisions"
    (Ok [])
    (Nvlist.fnvlist_unpack good_simple) ;
  Alcotest.(check @@ result nvlist pass)
    "bad_simple: collisions"
    (Error "bad")
    (Nvlist.fnvlist_unpack bad_simple) ;
  Alcotest.(check @@ result nvlist string)
    "good_simple_2: unique flags off"
    (Ok ["a", Nvlist.Nvlist ["a", Boolean;"a",Boolean];
         "a", Nvlist.Nvlist ["a", Boolean;"a", Boolean]
        ])
    (Nvlist.fnvlist_unpack good_simple_2)

let test_bad_input_1 () =
  (* at various stages of development these inputs resulted
     in exceptions being thrown *)
  let inputs = [
    "000100000000000001000000dfc70021fc0000f9e920000009000000209201df22f2f5ff0000f06fff00";
    "000100000000000001000000200000000300ebff0000000005000000796f14f80000000009000000000000002000000003000000010000000500001d6c6f0000";
    "00010000000000000100000020000000030000000100000005000000796f0000000000000900000003000000200000000300000001000000050000006c6f0000";
    "010001010100010001000100010001000101010001000100010901000100" ;
    "00010000000000000100000020000000030000000100000005000000796ffeffffff00000900000000000000200000000300000001000000050000006c6f0000" ;
    (*  5 *)
    "000100000000000001000000000001000300edff017f000006000000796f20e92200000009000000100000002000000000000000000900000b000000002000000003000000010000000300000001000000050000006c6f0000";
    "000100000000000001000000000001000300edff6400000006000000796f20e921fffff709000000100301000000000000050000006c6f0000" ;
    "0001000000000000010000003e00ea0000010000017f000005000000796f40000000000009e0ff000000000020000000000000000009000010000000002000000003000000010000000300000001000000050000006c6f0000" ;
    "0001000000000000010000002000ea000af7ffff007f000005000000796f0000000000000900000010000000200000000000000000090000050000000020000000030000000100000003000001000000050000006c6f0000" ;
    "000100000000000001000000000000fff600edff017f000002000000796f18fffffffc2e09000000100000001fffe1000001000000050000006c6f0000" ;
    (* 10 *)
    "000100000000000001000000000001017f0000051d" ;
    "000100000000000001000000000001000300edff017f00001a000000796f18ffffffff805d000000100000001fffe100000000ff000000e9ffff00002e5d000000100000001fffe10000000001000000000500001d6c6f0000" ;
    "000100000000000001000000000001000300edff017f000002000000796f18ff087f0000000000001000000020000000800000002000000003000000010000000300000001000000050000006c6f0000" ;
    "0001000000000000010000001900ea0003000000017f000005000000796f000000000000090000000000000000050000006c6f0000" ;
    "000100000000000001000000001001000300edff017f000005000000796fffe810000000090000000000000000000500db006c6f0008" ;
    (* 15 *)
    "00010000000000000100000020000000030000f4000000000f000000796f0000000000000900000000000000200000000300000001000000050000006c6f0000" ;
    "00010000000000000100000020000000030000000100000005000000796f0000000000000900000000000000200000000300c00001000000140000006c6f0000" ;
    "00010000000000000100000020000000030000000100000005000000796efff1000000000900000000000000200000000300000001000000140000006c6f0000" ;
    "00010000000000000100000020000000030000000100000005000000796effffff0000000900000000000000200000000300000001000000140000006c6f0000" ;
    "00010000000000000100000020000000030000000100000005000000796ffeffffff00000900000000000000200000000300000001000000140000006c6f0000" ;
    (* 20 *)
    "00010000000000000100000020000000030000000100000013000000796f0000000000000900000020000000200000000300000001000000050000006c6f0000" ;
    "00010000000000000100000020000000030000000100000013000000790000000900000000000000200000000300000001000000050000006c6f00006c6f0000" ;
    "00010000000000000100000020000000030000000100000013000000796f0000000100000000000001000000200000000300000001000000050000006c6f0000" ;
    "0001000000000000010000002000000003000000f2ff000014000000796f0000000000000900000000000000200000000300000001000000050000006c530000" ;
    "00010000000000000100000020000000039ceb2000000002000000007a00"
    ^ "ff8000ff000009000001000000f901dfffff0025101a6100000064000200"
    ^ "7a00000100000000000001000000200000de170000f901dfffff09000000"
    ^ "679279920d2208e6ff0008ff1601020000010000000000002000ffff0000"
    ^ "0000000000a60100dd0020df22f2f50aed006c6fff00";
    (* 25 *)
    "00010000000000000100000020daff1919fa0000fa003007032d11e022000000190000007a000100000000000001000000200000000300000001000000050000640000007a00000100000000000001000000200000de170000f901dfffff09000000799279920d2208e6ff0008ff1601020000010000000000002000ffff00000000000000a60100dd0020df22f2f50aed006c6fff00" ;
    "00010000000000000100000030000000080000000178000013000000f105ffff053637000000000001000000000000000000000000000000000000000000000000000000"; (* el_count wrong for Nvlist *)
    "00010000000000000100000020000000030000000100000005000000796f0000000000000900000000000000200000000300000001000000050000006c6f0000";
    "0001000000000000010000003000feff080000000100000013000000f1323334353637000000000001000000000000000000000000000000000000000000000000000000"; (* libnv rejects this because of feff *)
    "0001000000000000010000003000fe00080000000100000013000000f1323334353637000000000001000000000000000000000000000000000000000000000000000000"; (* like above, the fe is bad again *)
    "000100000000000001000000300000ff080000000100000013000000f1323334353637000000000001000000000000000000000000000000000000000000000000000000"; (* like above, ff is bad *)
    "0001000000000000010000002000fff5030000000100000009000000797f00000200110117ffffff0922000100000000000003000000010000000000050000006c6f00000000000000002020202020202020202020202020"; (* libnv doesn't like this *)
    "0001000000000000010000002000000003000000180000000f00000078190080feffff0009ff160102000001796f1a001e00009cebff04ff10000000feff000000000100000020002000000003000600010000f901dfffff0900000079920d2208e6ff0008ff16012000000003000600010008f901dfffff09ffff0079920d2208e6ff0008ff160102000001000000000000010000002000000001000000000000010000000000000000000000a60d000000000100000000000001000000200000de170000f901dfffff090000007992000079920d2208e6ff0008ff1601020000010022000000000100000020000000000000087f0000a601000c00000100000000000001000000" ; (* libnv doesn't like this *)
    "00010000000000000100000020000000030000000100000009000000795c00000200110117ffffffffffffffff0affffffffffffff09ff0d000100000000000001000000200000ff030000000100000009000000796f0000" ; (* libnv unhappy *)
    "00010000000000000100000020000000040000000200000019000000303030000000000030300000000000002000000004000000020000001a00000030307000000000003030000000000000200000000400000002000000180000003030300000000000000100000000000000000000" ; (* libnv complains because the key 0x303030 ("000") is duplicate and the unique flag is set *)
    "0001000000000000010000003000000003000000010000001300000061620000000000380000000300000000000000000100000000000000000000000000000000000000"; (* libnv doesn't like but we do? *)
  ]
  in
  List.iteri (fun idx inp ->
  Alcotest.(check @@ result nvlist pass)
    ("known_bad_" ^ string_of_int idx)
    (Error "")
    (Nvlist.fnvlist_unpack @@ Nvlist.unhex inp)
    ) inputs

let regression_01_hang () =
  let inp = Nvlist.unhex @@
      "00010000000000000100000020e2ff000325101af2ffffff0c000000d2ffff800000"
in
  for i = 1 to String.length inp do
    Alcotest.(check @@ result nvlist pass)
      ("01_hang_" ^ (string_of_int i))
      (Error "")
      (Nvlist.fnvlist_unpack @@
       String.sub inp 0 (i-1))
  done

let regression_02_invalid_el_count () =
  (* TODO this test vector has wrong appendix/namesz/el_count lengths *)
  let inp = Nvlist.unhex @@
    "000100000000000001000000000001000300edff017f000002000000796f18ff087f"
    ^ "00000000000010000000200000008000000020000000030000000100000003000"
    ^ "0000100000101ecff0000000000" in
  Alcotest.(check @@ result reject pass)
    "invalid elt counts should fail to parse"
    (Error "")
    (Nvlist.fnvlist_unpack inp)

let regression_03_bad_nvlist_length_encode,
    regression_03_bad_nvlist_length_decode =
  let v =
    "00010000" ^ "00000000" ^ "01000000" ^ "30000000" ^ "08000000" ^ "01000000"
    ^ "13000000" ^ "31323334" ^ "35363700" ^ "00000000" ^ "01000000" ^ "00000000" ^ "00000000" ^ "00000000" ^ "00000000" ^ "00000000"
    ^ "00000000" ^ "" in
  let exp =[ ("1234567", Nvlist.Nvlist  [])]in
  (fun() ->
     Alcotest.(check @@ result hexcheck string)
       "encode_trail_nvlist_empty"
       (Ok (Nvlist.unhex v))
       (Nvlist.fnvlist_pack exp)
  ),
  (fun() ->
     Alcotest.(check @@ result nvlist string)
       "bad_nvlist_length" (Ok exp) (Nvlist.fnvlist_unpack @@ Nvlist.unhex v) ;
     (* below we test with 30 replaced with 38, which the encoder used to
        output, and which is incorrect: *)
     let v2 = Nvlist.unhex @@
       "00010000" ^ "00000000" ^ "01000000" ^ "38000000" ^ "08000000" ^ "01000000"
       ^ "13000000" ^ "31323334" ^ "35363700" ^ "00000000" ^ "01000000" ^ "00000000" ^ "00000000" ^ "00000000" ^ "00000000" ^ "00000000"
    ^ "00000000" ^ "" in
     Alcotest.(check @@ result reject pass)
       "decode wrong length" (Error "any") (Nvlist.fnvlist_unpack v2) ;
  )

let regression_04_reject_valid () =
  let inp = Nvlist.unhex @@
    "000100000000000001000000300000000300000001000000130000006162000000000000000000000100000000000000000000000000000000000000200000000200000001000000050000006200000000000000330000000"
    ^"0c000000000000000000000" in
  let exp = [
    "ab", Nvlist.Nvlist [ "b" , Int32 0x33_l]
  ] in
  Alcotest.(check @@ result nvlist reject)
    "libnvpair is happy with this"
    (Ok exp)
    (Nvlist.fnvlist_unpack inp) ;
  let inp_2 = Nvlist.unhex @@
    "000100000000000001000000300000000300000001000000130000006162000000000000000000000100000000000000000000000000000000000000200000000200000001000000050000006200000000000000330000000"
    ^"00000000000000000000000" in
  Alcotest.(check @@ result nvlist reject)
    "libnvpair is also happy with this"
    (Ok exp)
    (Nvlist.fnvlist_unpack inp_2)

let regression_05_hang_json () =
  (* This did not trigger a bug in this library, but
   * a bug in
   * libnvpair:nvlist_print_json->nvlist_print_json_string
   * where an invalid multibyte string was handled incorrectly.
   * Given the invalid string "A\x80\x00" (with 0x80 being invalid), it will
   * first emit (c="A"), then the second call to
   * mbrtowc() fails, returning a negative error code.
   * The problem is that mbrtowc() > 0 is only
   * false when sz == 0, not when
   * (sz=(size_t)-1) or (sz=(size_t)-2).
   * On Linux/glibc (probably depending on locale)
   * this also has the side effect of putting &mbr
   * in a failed state (which is implementation-defined).
   * When that happens, it will continue to return errors,
   * disregarding the incremented &input pointer which
   * eventually was meant to stop when it hit '\x00'.
   * As a result the function enters an infinite loop
   * printing AAAAAAAAA...
   * source code ref:
   *  https://github.com/openzfs/zfs/blob/5b525165e9113e7faabd230b504ae4e9b85d35a5/lib/libnvpair/libnvpair_json.c#L56
   * fix: https://github.com/openzfs/zfs/pull/12176
  *)
  let inp = Nvlist.unhex @@
    "000100000000000001000000200000000300000001000000090000006162000000000000418000000900000000000000"
  in
  let exp = ["ab", Nvlist.Str "A\x80"] in
  Alcotest.(check @@ result nvlist string)
    "avoid infinite loop"
    (Ok exp)
    (Nvlist.fnvlist_unpack inp)

let regression_06_namesz () =
  (* the "bad" value used to be accepted, while
   * libnvpair rejected it. *)
  let good = Nvlist.unhex @@
    "00010000000000000100000020000000"^
    "03000000" ^
    "010000000f000000796f" ^
    "000000000000090000000000010000000000"
  in
  let bad = Nvlist.unhex @@
    "00010000000000000100000020000000" ^
    "03400000" ^ (* too long namesz *)
    "010000000f000000796f" ^
    "000000000000090000000000010000000000"
  in
  let good_2 = Nvlist.unhex @@
    "00010000000000000100000020000000" ^
    "0300cccc" ^ (* valid (3_l) namesz with pad *)
    "010000000f000000796f" ^
    "000000000000090000000000010000000000"
  in
  let exp =
    ["yo", Nvlist.Int64Array [281474976710665_L]]
  in
  Alcotest.(check @@ result nvlist string)
    "can decode valid representation"
    (Ok exp)
    (Nvlist.fnvlist_unpack good) ;
  Alcotest.(check @@ result nvlist string)
    "can decode with weird namesz padding"
    (Ok exp)
    (Nvlist.fnvlist_unpack good_2) ;
  Alcotest.(check @@ result nvlist pass)
    "can't decode invalid representation"
    (Error "libnvpair doesn't like this")
    (Nvlist.fnvlist_unpack bad)

let regression_07_xxx () =
  (* the "bad" value used to be accepted, while
   * libnvpair rejected it. *)
  let good = Nvlist.unhex @@
    "000100000000000001000000" ^
    "300000000800000001000000130000003132" ^
    "3334353637000000000001000000000000000" ^
    "000000000000000000000000000000000000000"
  in
  let bad = Nvlist.unhex @@
    "000100000000000001000000" ^
    "200000000800000001000000130000003132" ^
    "3334353637000000000001000000000000000" ^
    "000000000000000000000000000000000000000"
  in
  let exp =
    [ "1234567", Nvlist.Nvlist [] ]
  in
  Alcotest.(check @@ result nvlist string)
    "can decode valid representation"
    (Ok exp)
    (Nvlist.fnvlist_unpack good) ;
  Alcotest.(check @@ result nvlist pass)
    "can't decode invalid representation"
    (Error "libnvpair doesn't like this")
    (Nvlist.fnvlist_unpack bad)

let regression_08_booleanvalue_exact () =
  let bad = Nvlist.unhex @@
    "000130303030303030303030" ^
    "20000000" ^ "0400" ^ "3030" ^ (* nvlen=0x20; namesz=0x0004; pad=x3030 *)
    "01000000" ^ "15000000" ^ (* elcount=1; typ=0x15 *)
    "30303000" ^ (* key "333\x00" *)
    "30303003" ^ (* key/name str pad *)
    "000" ^ "f" ^ "0002" ^ (* bool *)
    "0000003000000000"
  in
  let good_3 = Nvlist.unhex @@
    "000130303030303030303030" ^
    "20000000" ^ "0400" ^ "3030" ^ (* nvlen=0x20; namesz=0x0004 *)
    "01000000" ^ "15000000" ^ (* elcount=1; typ=0x15 *)
    "30303000" ^ (* key/name "333\x00" *)
    "30303003" ^ (* key/name str pad *)
    "000" ^ "0" ^ "0000" ^ (* bool *)
    "0000003000000000"
  in
  let good_2 = Nvlist.unhex @@
    "000130303030303030303030000000000400303001000000150000003030" ^
    "300030303003000f0002" ^ "0000003000000000"
  in
  let good = Nvlist.unhex @@
    "000100000000000001000000200000000400000001000000150000003030300000000000000000000000000000000000"
  in
  let exp = [
    "000", Nvlist.BooleanValue false
  ] in
  Alcotest.(check @@ result nvlist string)
    "good"
    (Ok exp)
    (Nvlist.fnvlist_unpack good) ;
  Alcotest.(check @@ result nvlist string)
    "good: length capped"
    (Ok [])
    (Nvlist.fnvlist_unpack good_2) ;
  Alcotest.(check @@ result nvlist string)
    "good: 2 changed to 0"
    (Ok exp)
    (Nvlist.fnvlist_unpack good_3) ;
  Alcotest.(check @@ result nvlist pass)
    "bad"
    (Error "bad")
    (Nvlist.fnvlist_unpack bad)

let regression_09_xxx () =
  let bad = Nvlist.unhex @@
    "00010000000000000100000030000000" ^
    "0300000001000000130000006162000000000008" ^ (* namesz;elcnt;typ;"ab"*)
    "00000003" ^
    "00000000" ^
    "00000000" ^
    "0100000000000000000000000000000000000000"
  in
  let bad_2 = Nvlist.unhex @@
    "00010000000000000100000030000000" ^
    "08000000" ^ "0100000013000000" ^ (* namesz=8; elcnt; typ=nvlist *)
    "3132333435363700" ^ "fffffff4" ^ (* name + pad *)
    "01000000" ^ (* nv header version *)
    "00000000" ^ (* nvflag *)
    "0000000000000000" ^ (* priv *)
    "00000000" ^ (* header flag *)
    "00000000" ^ (* pad *)
    "00000000" (* end list *)
  in
  let good = Nvlist.unhex @@
    "00010000000000000100000030000000" ^
    "0300000001000000130000006162000000000008" ^ (* namesz;elcnt;typ;"ab"*)
    "00000000" ^ (* nvl version? *)
    "00000000" ^ (* nvflag *)
    "00000000" ^ "01000000" ^ (* priv *)
    "00000000" ^ (* nv header flag *)
    (*nv header pad*)
    "00000000" ^ (* pad *)
    "00000000" ^ (* end nested nvlist *)
    "00000000" (* end whole thing *)
  in
  Alcotest.(check @@ result nvlist pass)
    "embedded nvlist header version should be 0"
    (Error "version not 0")
    (Nvlist.fnvlist_unpack bad) ;
  Alcotest.(check @@ result nvlist pass)
    "embedded nvlist header version should be 0"
    (Error "version not 0")
    (Nvlist.fnvlist_unpack bad_2) ;
  Alcotest.(check @@ result nvlist string)
    "embedded nvlist header is 0"
    (Ok ["ab", Nvlist.Nvlist []])
    (Nvlist.fnvlist_unpack good)

let test_encode_boolean,
    test_decode_boolean =
  (* nvlist size: 40 *)
  let v =
    "00010000" ^ "00000000" ^ "01000000" ^ "18000000" ^ "02000000" ^ "00000000"
    ^ "01000000" ^ "61000000" ^ "00000000" ^ "00000000" ^ "" in
  let exp =[ (       "a", Nvlist.Boolean)]in
  (fun() ->
     Alcotest.(check @@ result hexcheck string)
       "boolean_enc"
       (Ok (Nvlist.unhex v))
       (Nvlist.fnvlist_pack exp)
  ),
  (fun() ->
     Alcotest.(check @@ result nvlist string)
       "boolean_dec"    (Ok exp)    (Nvlist.fnvlist_unpack @@ Nvlist.unhex v))

let test_encode_strarray,
    test_decode_strarray =
  (* nvlist size: 40 *)
  let v =
    "00010000" ^ "00000000" ^ "01000000" ^ "18000000" ^ "02000000" ^ "00000000"
    ^ "11000000" ^ "61000000" ^ "00000000" ^ "00000000" ^ "" in
  let exp =[ (       "a", (Nvlist.StrArray []))]in
  (fun() ->
     Alcotest.(check @@ result hexcheck string)
       "c_test"
       (Ok (Nvlist.unhex v))
       (Nvlist.fnvlist_pack exp)
  ),
  (fun() ->
     Alcotest.(check @@ result nvlist string)
       "c_test_decode"    (Ok exp)    (Nvlist.fnvlist_unpack @@ Nvlist.unhex v))

let tests = [
  "hex", [
    "constants", `Quick, test_hex ;
  ] ;
  "pad_str", [
    "1..9", `Quick, test_pad_str ;
  ] ;
  "to_c", [
    (*"empty", `Quick, test_c_empty ;*)
    (*"yo_int32", `Quick, test_c_yo_int32 ;*)
  ] ;
  "encode", [
    "empty", `Quick, test_encode_empty ;
    "boolean", `Quick, test_encode_boolean ;
    "boolean_t", `Quick, test_encode_boolean_value_t ;
    "boolean_f",`Quick,test_encode_boolean_value_f ;
    "boolean_array", `Quick, test_encode_boolean_array ;
    "duplicate", `Quick, test_encode_duplicate ;
    "hrtime", `Quick, test_encode_hrtime ;
    "byte", `Quick, test_encode_byte ;
    "simple", `Quick, test_encode_simple ;
    "simple_small", `Quick, test_encode_simple_small ;
    "string", `Quick, test_encode_string ;
    "string_empty", `Quick, test_encode_string_empty ;
    "string_7", `Quick, test_encode_string_7 ;
    "string_8", `Quick, test_encode_string_8 ;
    "string_97", `Quick, test_encode_string_97 ;
    "strarray", `Quick, test_encode_strarray ;
    "str_array_3_hello", `Quick, test_encode_str_array_3_hello ;
    "str_array_12_hello", `Quick, test_encode_str_array_12_hello ;
    "str_array_32", `Quick, test_encode_str_array_32 ;
    "complex2", `Quick, test_encode_complex2 ;
    "complex1", `Quick, test_encode_complex1 ;
    "int8array_18", `Quick, test_encode_int8array_18;
    "int16array", `Quick, test_encode_int16array ;
    "uint16array_libnv", `Quick, test_encode_uint16array_libnv ;
    "int64_array", `Quick, test_encode_int64_array;
    "nvlistarray_empty", `Quick, test_encode_nvlistarray_empty ;
    "nvlistarray_single_empty", `Quick, test_encode_nvlistarray_single_empty ;
    "nvlistarray_single_a", `Quick, test_encode_nvlistarray_single_a ;
    "nested", `Quick, test_encode_nested ;
    "nested_2", `Quick, test_encode_nested_2 ;
    "int16array_nested_complex", `Quick, test_encode_int16array_nested_complex ;
    "nested_int32", `Quick, test_encode_nested_int32 ;
    "nested_3", `Quick, test_encode_nested_3 ;
    "int16array_0", `Quick, test_encode_int16array_0 ;
    "int16array_9", `Quick, test_encode_int16array ;
    "nvlistarray_ab", `Quick, test_encode_nvlistarray_ab ;
    "nvlistarray_abc", `Quick, test_encode_nvlistarray_abc ;
    "bytearray_empty_encode", `Quick, test_bytearray_empty_encode ;
    "bytearray_1_encode", `Quick, test_bytearray_1_encode ;
    "bytearray_2_encode", `Quick, test_bytearray_2_encode ;
    "bytearray_3_encode", `Quick, test_bytearray_3_encode ;
    "bytearray_4_encode", `Quick, test_bytearray_4_encode ;
    "bytearray_7_encode", `Quick, test_bytearray_7_encode ;
    "bytearray_8_encode", `Quick, test_bytearray_9_encode ;
    "bytearray_9_encode", `Quick, test_bytearray_8_encode ;
    "int32s", `Quick, test_encode_int32s ;
    "nest2_nonempty_unalign", `Quick, test_encode_nest2_nonempty_unalign;
    "string_array_short", `Quick, test_encode_string_array_short ;
  ] ;
  "decode", [
    "empty", `Quick, test_decode_empty ;
    "boolean", `Quick, test_decode_boolean ;
    "boolean_t", `Quick, test_decode_boolean_value_t ;
    "boolean_f",`Quick,test_decode_boolean_value_f ;
    "duplicate", `Quick, test_decode_duplicate ;
    "hrtime", `Quick, test_decode_hrtime ;
    "strarray", `Quick, test_decode_strarray ;
    "str_array_3_hello", `Quick, test_decode_str_array_3_hello ;
    "str_array_12_hello", `Quick, test_decode_str_array_12_hello ;
    "str_array_32", `Quick, test_decode_str_array_32 ;
    "nested_int32", `Quick, test_decode_nested_int32 ;
    "string", `Quick, test_decode_string ;
    "string_empty", `Quick, test_decode_string_empty ;
    "string_7", `Quick, test_decode_string_7 ;
    "string_8", `Quick, test_decode_string_8 ;
    "string_97", `Quick, test_decode_string_97 ;
    "complex2", `Quick, test_decode_complex2 ;
    "complex1", `Quick, test_decode_complex1 ;
    "int64_array", `Quick, test_decode_int64_array;
    "boolean_array", `Quick, test_decode_boolean_array ;
    "nvlistarray_empty", `Quick, test_decode_nvlistarray_empty ;
    "nvlistarray_single_empty", `Quick, test_decode_nvlistarray_single_empty ;
    "nvlistarray_single_a", `Quick, test_decode_nvlistarray_single_a ;
    "nvlistarray_ab", `Quick, test_decode_nvlistarray_ab ;
    "int8array_18", `Quick, test_decode_int8array_18;
    "int16array_0", `Quick, test_decode_int16array_0 ;
    "uint16array_libnv", `Quick, test_decode_uint16array_libnv ;
    "int16array_nested_complex", `Quick, test_decode_int16array_nested_complex ;
    "bytearray_empty_decode", `Quick, test_bytearray_empty_decode ;
    "bytearray_1_decode", `Quick, test_bytearray_1_decode ;
    "bytearray_2_decode", `Quick, test_bytearray_2_decode ;
    "bytearray_3_decode", `Quick, test_bytearray_3_decode ;
    "bytearray_4_decode", `Quick, test_bytearray_4_decode ;
    "bytearray_7_decode", `Quick, test_bytearray_7_decode ;
    "bytearray_8_decode", `Quick, test_bytearray_9_decode ;
    "bytearray_9_decode", `Quick, test_bytearray_8_decode ;
    "int32s", `Quick, test_decode_int32s ;
    "nest2_nonempty_unalign", `Quick, test_decode_nest2_nonempty_unalign;
    "string_array_short", `Quick, test_decode_string_array_short ;
  ];
  "selftest", [
    "self empty", `Quick, test_self [];
    "self a:5l", `Quick, test_self ["a", Int32 5l];
    "self a:1l,b:2l", `Quick, test_self ["a", Int32 1l ; "b", Int32 2l];
    "self complex1", `Quick, test_self [
      "abcdefghijklmn", Uint32 12345678l ;
      "b", Int32 2l ;
      "s1", Str "" ;
      "s123", Str "123";
      "s1234", Str "1234";
      "s1234567", Str "1234567";
      "s12345678", Str "12345678";
    ];
    "self complex2", `Quick, test_self [
      "abcdefghijklmn", Uint64 12345678_L ;
      "s123456789", Str "123456789";
    ];
    "self_int32s", `Quick, test_self [
      "a", Int32 5l ;
      "b", Int32 6l;
      "c", Int32 7l;
      "d", Uint32 8l;
      "e", Uint32 9l;
    ];
    "self nest1_empty", `Quick, test_self [
      "nest1", Nvlist [];
    ];
    "self nest2_empty", `Quick, test_self [
      "nest1", Nvlist [
        "nest2_a", Nvlist [];
        "nest2_b", Nvlist [];
      ];
    ];
    "self nest3_empty", `Quick, test_self [
      "nest1", Nvlist [
        "nest2", Nvlist [
          "nest3", Nvlist []]]
    ];
    "self nest4_empty", `Quick, test_self [
      "nest1", Nvlist [
        "nest2", Nvlist [
          "nest3", Nvlist [
            "nest4", Nvlist []]]]
    ];
    "self nest1_nonempty", `Quick, test_self [
      "nest1", Nvlist [
        "abcdefghijklmn", Uint64 12345678_L ;
        "s123456789", Str "123456789";
      ];
    ];
    "self nest2_nonempty", `Quick, test_self [
      "nest1", Nvlist [
        "nest2_a", Nvlist [
          "2_a_a", Str "hey" ;
          "2_a_b", Uint64 987654321_L;
        ];
        "nest2_b", Nvlist [
          "2_b_b", Str "2_b_b" ;
          "2_b_a", Uint64 32_L ;
        ];
      ];
    ];
    "self_nested_int32", `Quick, test_self [
      "x", Nvlist ["a", Int32 5l ;
                   "b", Int32 6l;
                   "c", Int32 7l;
                  ];
    ];
    "self_nested_int16", `Quick, test_self [
      "x", Nvlist ["a", Int16 5 ;
                   "b", Int16 6 ;
                   "c", Int16 7 ;
                  ];
    ];
    "self_nest1_int32_2", `Quick, test_self [
      "x", Nvlist ["y", Int32 8l ;
                   "z", Int32 9l;
                  ];
      "a", Int32 255l;
    ];
    "self_nest1_int16_2", `Quick, test_self [
      "x", Nvlist ["y", Int16 8 ;
                   "z", Int16 9 ;
                  ];
      "a", Uint16 255 ;
    ];
    "self string", `Quick, test_self [
      "abc", Str "ABC" ;
    ];
    "self string empty", `Quick, test_self [
      "abc", Str "" ;
    ];
    "self string 50", `Quick, test_self [
      "abc", Str (String.make 50 'a') ;
    ];
    "self string 8", `Quick, test_self [
      "abc", Str "12345678" ;
    ];
    "self nest2_string", `Quick, test_self [
      "nest1", Nvlist [
        "nest2", Nvlist [
          "abc", Str "ABC" ;
        ];
      ];
    ];
    "self nest2_nonempty_unalign", `Quick, test_self [
      "nest1", Nvlist [
        "nest2_a", Nvlist [
          "2_a_a", Str "hey" ;
          "2_a_b", Uint64 987654321_L;
        ];
        "nest2_b", Nvlist [
          "2_b_a", Int32 32l ;
          "2_b_b", Str "2_b_b" ;
        ];
      ];
    ];
    "self_int16array_0", `Quick, test_self [
      "abc", Int16Array []];
    "self_int16array_1", `Quick, test_self [
      "abc", Int16Array [1]];
    "self_int16array_2", `Quick, test_self [
      "abc", Int16Array [1;2]];
    "self_int16array_3", `Quick, test_self [
      "abc", Int16Array [1;2;3]];
    "self_int16array_4", `Quick, test_self [
      "abc", Int16Array [1;2;3;4]];
    "self_int16array_5", `Quick, test_self [
      "abc", Int16Array [1;2;3;4;5]];
    "self_int16array_9", `Quick, test_self [
      "abc", Int16Array [1;2;3;4;5;6;7;8;9]];
    "self_int16array_minmax", `Quick, test_self [
      "abc", Int16Array  [-32768; -32767; -1]];
    "self_uint16array_minmax", `Quick, test_self [
      "abc", Uint16Array [32768; 32769; 65535]];
    "self_uint16array", `Quick, test_self [
      "a", Uint16Array [1;2;3;4;5]];
    "self_int16array_check_alignment", `Quick, test_self [
      "abc", Int16Array [1;2] ;
      "def", Int16Array [3;4] ;
      "xyz", Int16Array [5;6] ;
    ];
    "self_int16array_nested_complex", `Quick, test_self [
      "abc", Int16Array [1;2] ;
      "def", Int16Array [3;4] ;
      "xyz", NvlistArray [["ABC", Int16Array [5;6]] ;
                          ["DEF", Int16Array [7;8]]] ;
    ];
    "self_uint32array", `Quick, test_self [
      "a", Uint32Array [1l;2l;123456789_l;4l;5l]];
    "self_uint64array", `Quick, test_self [
      "a", Uint64Array [1L;2L;1234567890_L;4_L;5_L]];
    "self_int8array", `Quick, test_self [
      "a", Int8Array [0;1;2;-1;-128]];
    "self_uint8array", `Quick, test_self [
      "a", Uint8Array [0;1;2;255;128]];
    "self_bytearray_empty", `Quick, test_self [
      "a", ByteArray ""];
    "self_bytearray_xyz", `Quick, test_self [
      "a", ByteArray "xyz"];
    "self_bytearray_123456789", `Quick, test_self [
      "a", ByteArray "123456789"];
    "self_byte_b", `Quick, test_self [
      "a", Byte 'b'];
    "self_byte_00", `Quick, test_self [
      "a", Byte '\x00'];
    "self_nvlistarray_empty", `Quick, test_self [
      "a", NvlistArray []];
    "self_nvlistarray_single_empty", `Quick, test_self [
      "a", NvlistArray [ [];]];
    "self_nvlistarray_a", `Quick, test_self [
      "a", NvlistArray [["a", Int32 33l];]];
    "self_nvlistarray_ab", `Quick, test_self [
      "a", NvlistArray [["a", Int32 33l];
                        ["b", Int32 34l]]];
    "self_nvlistarray_abc", `Quick, test_self [
      "a", NvlistArray [["a", Int32 33l];
                        ["b", Int32 34l];
                        ["c", Int32 35l];]];
    "self_boolean", `Quick, test_self [
      "b", Boolean
    ];
    "self_boolean_value_false", `Quick, test_self [
      "b", BooleanValue false
    ];
    "self_boolean_value_true", `Quick, test_self [
      "b", BooleanValue true
    ];
    "self_int8_uint8", `Quick, test_self [
      "i8", Int8 123;
      "u8", Uint8 0xff;
    ];
    "self_string_array_short", `Quick, test_self [
      "strs", StrArray ["a";"";"b"]
    ];
    "self_booleanarr_empty", `Quick, test_self ["b", BooleanArray [] ] ;
    "self_booleanarr_true", `Quick, test_self ["b", BooleanArray [true] ] ;
    "self_booleanarr_false", `Quick, test_self ["b", BooleanArray [false] ] ;
    "self_booleanarr_5", `Quick, test_self ["b", BooleanArray [
        true ; false ; false ; true ;true
      ] ] ;
    "self_hrtime", `Quick, test_self ["hr", HrTime 1_L ] ;
    "self_double", `Quick, test_self ["d", Double] ;
  ] ; (* selftest *)
  "knownbad", [
    "bad_1", `Quick, test_bad_input_1
  ];
  "regressions", [
    "01_hang", `Quick, regression_01_hang ;
    "02_not_checking_el_count", `Quick, regression_02_invalid_el_count ;
    "03_bad_nvlist_length_encode", `Quick, regression_03_bad_nvlist_length_encode ;
    "03_bad_nvlist_length_decode", `Quick, regression_03_bad_nvlist_length_decode ;
    "regression_04_reject_valid", `Quick, regression_04_reject_valid ;
    "regression_05_hang_json", `Quick, regression_05_hang_json ;
    "regression_06_namesz", `Quick, regression_06_namesz ;
    "regression_07_xxx", `Quick, regression_07_xxx ;
    "regression_08_boolean_value_exact", `Quick, regression_08_booleanvalue_exact ;
    "regression_09_xxx", `Quick, regression_09_xxx ;
  ];
]

let () =
  Alcotest.run "nvlist pure tests" tests

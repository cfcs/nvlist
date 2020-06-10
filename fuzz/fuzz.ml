let () =
  let fd = Unix.openfile Sys.argv.(1) Unix.[O_RDONLY] 0 in
  let {Unix.st_size = len ; _ } = Unix.fstat fd in
  let input =
    really_input_string (Unix.in_channel_of_descr fd) len
  in
  AflPersistent.run (fun () ->
      match Nvlist.fnvlist_unpack input with
      | Error _ -> ()
      | Ok unp ->
        begin match Nvlist.fnvlist_pack unp with
          | Ok pack ->
            if pack <> input
            then begin
              (* check that repacking is decoded the same: *)
              assert (Ok unp = Nvlist.fnvlist_unpack pack);
              (* optional: *)
              (* failwith "assert canonical" *)
              ()
            end
          | Error "unique name present twice" ->
            (* fair enough, we should support passing in the flag again *)
            ()
          | Error _ ->
            failwith "failed to repack"
        end
    )

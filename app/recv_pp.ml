open Nvlist_zfs

open Rresult

let jump () token override_toname override_toguid override_obj override_bytes override_offset filename =
  let _ = token, override_toname, override_toguid, override_obj, override_bytes, override_offset in
  Logs.app (fun m -> m "jumping %s\n%!" filename);
  let content, size =
    let fd = open_in_bin filename in
    let sz = in_channel_length fd in
    let x = really_input_string fd sz in
    close_in fd ; x, sz in
  let left = ref size in
  let fletcher4 = ref Fletcher4.empty in
  while !left > 0 do
    let consumed, cksum_new = DRR.parse_drr ~fletcher4:!fletcher4 (
        String.sub content (String.length content - !left) !left
        |> Bytes.of_string) in
    left := !left - consumed ;
    fletcher4 := cksum_new
  done ;
  Ok ()

let setup_log style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ~dst:Format.std_formatter ())

open Cmdliner

let setup_log =
  Term.(const setup_log
        $ Fmt_cli.style_renderer ()
        $ Logs_cli.level ())

(*
let () =
  let x = Resumetoken.parse Sys.argv.(1)
        |> Rresult.R.get_ok in
  let z = Fmt.strf "%a" Nvlist.pp_nvl x in
  Fmt.pr "%s" z
  *)

let token =
  let doc = "resumetok to modify" in
  Arg.(required & pos 0 (some string) None & info [] ~doc ~docv:"CONFIG")

let toguid =
  let doc = "Override toguid property" in
  Arg.(value & opt (some int64) None & info ["toguid"] ~doc)


let toname =
  let doc = "Override toname property" in
  Arg.(value & opt (some string) None & info ["toname"] ~doc)

let arg_obj =
  let doc = "Override obj property" in
  Arg.(value & opt (some int64) None & info ["obj"] ~doc)

let arg_bytes =
  let doc = "Override bytes= property" in
  Arg.(value & opt (some int64) None & info ["bytes"] ~doc)

let arg_offset =
  let doc = "Override offset= property" in
  Arg.(value & opt (some int64) None & info ["offset"] ~doc)

let arg_file =
  let doc = "file containing nvlist" in
  Arg.(required & pos 0 (some file) None & info [] ~doc ~docv:"FILE")

let cmd =
  Term.(term_result (const jump $ setup_log $ token $ toname $ toguid $ arg_obj $ arg_bytes $ arg_offset $ arg_file)),
  Term.info "recv_pp" ~version:"%%VERSION_NUM%%"


let () = match Term.eval cmd with `Ok () -> exit 0 | _ -> exit 1

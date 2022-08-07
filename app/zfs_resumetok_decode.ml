open Nvlist_zfs

open Rresult

let jump () token override_toname override_toguid override_obj override_bytes override_offset =
  (Resumetoken.parse token
   |> R.reword_error (fun x -> `Msg x))
  >>= fun nvl ->
  let obj = Nvlist.lookup_uint64 "object" nvl in
  let offset = Nvlist.lookup_uint64 "offset" nvl in
  let bytes = Nvlist.lookup_uint64 "bytes" nvl in
  let toguid = Nvlist.lookup_uint64 "toguid" nvl in
  let toname = Nvlist.lookup_string "toname" nvl in
  (Resumetoken.create
     ~obj:(match override_obj with None -> obj | Some x -> x)
     ~offset:(match override_offset with None -> offset | Some x -> x)
     ~bytes:(match override_bytes with None -> bytes | Some x -> x)
     ~toguid:(match override_toguid with None -> toguid | Some x -> x)
     ~toname:(match override_toname with None -> toname | Some x -> x)
   |> R.reword_error (fun x -> `Msg x)) >>= fun encoded ->
  Fmt.pr "%!";
  Fmt.epr "%!";
  Fmt.pr "ENCODED: %s\n" encoded ;
  Fmt.pr "ORIG: %s" token ;
  Fmt.pr "%!";
  Fmt.pr "%a\n%!" Nvlist.pp_nvl nvl ;
  (Resumetoken.parse encoded
   |> R.reword_error (fun x -> `Msg x)) >>= fun nvl_enc ->
  Fmt.epr "%!";
  Fmt.pr "%!\nOUTpp: %a\n%!" Nvlist.pp_nvl nvl_enc ;
  Fmt.pr "%!\nOUTenc: %s\n%!" encoded ;
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

let (cmd_v, cmd_info) =
  Term.(term_result (const jump $ setup_log $ token $ toname $ toguid $ arg_obj $ arg_bytes $ arg_offset)),
  Cmd.info "zfs_resumetok_decode" ~version:"%%VERSION_NUM%%"


let () =
  exit @@ Cmd.eval (Cmd.v cmd_info cmd_v)

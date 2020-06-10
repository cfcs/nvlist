open Rresult

let jump () filename =
  begin
    let fd = open_in_bin filename in
    let content = really_input_string fd (in_channel_length fd) in
    Nvlist.fnvlist_unpack content >>= fun nvl ->
    Fmt.pr "%a\n%!" Nvlist.pp_nvl nvl ;
    begin match
        Nvlist.fnvlist_pack nvl
      with
      | Ok packed ->
        Fmt.pr "repacked:\n%s\n" (Nvlist.hex packed) ;
        Ok ()
      | Error "unique name present twice" ->
        (* TODO this is a known limitation at this point *)
        Ok ()
      | Error _ as err -> err
    end
  end |> R.reword_error (fun x -> `Msg x)

open Cmdliner


let setup_log =
  let setup_log style_renderer level =
    Fmt_tty.setup_std_outputs ?style_renderer ();
    Logs.set_level level;
    Logs.set_reporter (Logs_fmt.reporter ~dst:Format.std_formatter ())
  in
  Term.(const setup_log
        $ Fmt_cli.style_renderer ()
        $ Logs_cli.level ())

let arg_file =
  let doc = "file containing nvlist" in
  Arg.(required & pos 0 (some file) None & info [] ~doc ~docv:"FILE")

let cmd =
  Term.(term_result (const jump $ setup_log $ arg_file)),
  Term.info "nvlist_pp" ~version:"%%VERSION_NUM%%"


let () = match Term.eval cmd with `Ok () -> exit 0 | _ -> exit 1

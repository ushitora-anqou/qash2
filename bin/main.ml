let version () =
  Logs.info (fun m -> m "version: 0.1.0");
  ()

let setup_logs () =
  Fmt.set_style_renderer Fmt.stderr `Ansi_tty;
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.set_level (Some Logs.Debug);
  ()

let () =
  setup_logs ();
  let open Cmdliner in
  Cmd.(
    group (info "qash2") [ v (info "version") Term.(const version $ const ()) ]
    |> eval |> exit)

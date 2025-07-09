(*---------------------------------------------------------------------------
   Copyright (c) 2019 The b0caml programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open Result.Syntax

let run_main f = Log.time (fun _ m -> m "total time b0caml %%VERSION%%") f
let exit_main = function
| B0caml.Exit.Code c -> exit c
| B0caml.Exit.Exec (exe, cmd) ->
    exit @@ Log.if_error ~use:B0caml.Exit.(code some_error) @@
    let argv0 = Fpath.to_string (Cmd.find_tool cmd |> Option.get) in
    let cmd = Cmd.set_tool exe cmd in
    Result.bind (Os.Cmd.execv ~argv0 cmd) @@ fun _ -> assert false

let main_without_cli script_file script_args =
  let res =
    run_main @@ fun () ->
    Log.if_error ~header:"" ~use:B0caml.Exit.comp_error @@
    let* conf = B0caml.Conf.of_env () in
    let* script = B0caml.get_script conf script_file in
    let* exe = B0caml.compile_script conf script in
    Ok (B0caml.Exit.Exec (exe, Cmd.list (script_file :: script_args)))
  in
  exit_main res

let main_with_cli () =
  let res = run_main @@ fun () -> Cmdliner.Cmd.eval_value (B0caml_cli.cmd ()) in
  let exit = match res with
  | Ok (`Ok res) -> res
  | Ok `Version ->  B0caml.Exit.ok
  | Ok `Help -> B0caml.Exit.ok
  | Error `Term -> B0caml.Exit.conf_error
  | Error `Parse -> B0caml.Exit.Code Cmdliner.Cmd.Exit.cli_error
  | Error `Exn -> B0caml.Exit.Code Cmdliner.Cmd.Exit.internal_error
  in
  exit_main exit

let main () =
  try match List.tl (Array.to_list Sys.argv) with
  | file :: args when String.exists Fpath.is_dir_sep_char file ->
      main_without_cli file args
  | _ -> main_with_cli ()
  with
  | e ->
      let bt = Printexc.get_raw_backtrace () in
      Fmt.epr "%s: @[internal error, uncaught exception:@\n%a@]@."
        (Filename.basename Sys.argv.(0)) Fmt.exn_backtrace (e, bt);
      exit (Cmdliner.Cmd.Exit.internal_error)

let () = if !Sys.interactive then () else main ()

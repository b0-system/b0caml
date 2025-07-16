(*---------------------------------------------------------------------------
   Copyright (c) 2019 The b0caml programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open Result.Syntax

let delete_script_cache conf script =
  let* script_file = B0caml.get_script_file conf script in
  let build_dir = B0caml.script_build_dir conf ~script_file in
  let* exists = Os.Dir.exists build_dir in
  if not exists then Ok () else
  let log = B0caml.script_build_log ~build_dir in
  let* log = B0_memo_log.read log in
  let* _existed = Os.Path.delete ~recurse:true build_dir in
  let keys =
    let add_key acc o = match B0_zero.Op.hash o with
    | k when B0_hash.is_nil k -> acc | k -> B0_hash.to_hex k :: acc
    in
    List.fold_left add_key [] (B0_memo_log.ops log)
  in
  let dir = B0caml.Conf.b0_cache_dir conf in
  let kind = `Any and used = (* ignored *) String.Set.empty in
  let* _existed = B0_memo_cli.File_cache.delete ~dir ~kind ~used (`Keys keys) in
  Ok ()

let output_script_path conf script =
  let* script_file = B0caml.get_script_file conf script in
  let build_dir = B0caml.script_build_dir conf ~script_file in
  Ok (Fmt.pr "%a@." Fpath.pp_unquoted build_dir)

let cache ~conf ~action ~scripts =
  (* The notion of cache for `b0caml` is a bit different from a build
     system one. So B0_memo_cli.File_cache cannot simply be reused off the
     shelf. Some adjustements should be made here or generalization there. *)
  Log.if_error ~use:B0caml.Exit.conf_error @@
  let* conf in match action with
  | `Delete ->
      begin match scripts with
      | [] ->
          let cache_dir = B0caml.Conf.cache_dir conf in
          let pp_path = Fmt.st' [`Bold] Fpath.pp_unquoted in
          Log.stdout begin fun m ->
            m "Deleting %a, this may take some time..." pp_path cache_dir
          end;
          Log.if_error' ~use:B0caml.Exit.some_error @@
          let* _existed = Os.Path.delete ~recurse:true cache_dir in
          Ok B0caml.Exit.ok
      | scripts ->
          let delete acc script =
            let ok () = acc in
            let error e = Log.if_error ~use:B0caml.Exit.some_error (Error e) in
            Result.fold ~ok ~error (delete_script_cache conf script)
          in
          Ok (List.fold_left delete B0caml.Exit.ok scripts)
      end
  | `Gc ->
      Log.stdout (fun m -> m "the gc subcommand is TODO");
      Ok B0caml.Exit.some_error
  | `Path ->
      begin match scripts with
      | [] ->
          Log.stdout
            (fun m -> m "%a" Fpath.pp_unquoted (B0caml.Conf.cache_dir conf));
          Ok B0caml.Exit.some_error
      | scripts ->
          let output_path acc script =
            let ok () = acc in
            let error e = Log.if_error ~use:B0caml.Exit.some_error (Error e) in
            Result.fold ~ok ~error (output_script_path conf script)
          in
          Ok (List.fold_left output_path B0caml.Exit.ok scripts)
      end
  | `Stats ->
      let dir = B0caml.Conf.b0_cache_dir conf in
      let used = String.Set.empty in (* TODO *)
      let* _ = B0_memo_cli.File_cache.stats ~dir ~used in
      Ok B0caml.Exit.ok
  | `Trim ->
      Log.stdout (fun m -> m "the trim subcommand is TODO");
      Ok B0caml.Exit.some_error

let deps ~conf ~script_file ~raw ~directory ~mod_use ~root =
  Log.if_error ~use:B0caml.Exit.conf_error @@
  let* conf in
  Log.if_error' ~header:"" ~use:B0caml.Exit.comp_error @@
  let* script = B0caml.get_script conf script_file in
  if root then
    let directories = B0caml_script.directories script in
    let root (d, _) = B0caml_ocamlpath.logical_path_root_name d in
    let roots = String.distinct (List.filter_map root directories) in
    let pp_roots = Fmt.(vbox @@ list string) in
    if roots <> [] then Log.stdout (fun m -> m "%a" pp_roots roots);
    Ok B0caml.Exit.ok
  else
  let directories c s raw =
    if raw then Ok (List.map fst (B0caml_script.directories s)) else
    let ocamlpath = B0caml.Conf.ocamlpath c in
    Result.map_error (B0caml.Err.directories ~ocamlpath) @@
    let* dirs = B0caml_script.resolve_directories ~ocamlpath s in
    Ok (List.map B0caml_script.directory_resolution_dir dirs)
  in
  let mod_uses s raw =
    if raw then Ok (List.map fst (B0caml_script.mod_uses s)) else
    Result.map_error B0caml.Err.mod_uses @@
    let* files = B0caml_script.resolve_mod_uses s in
    Ok (List.concat_map B0caml_script.mod_use_resolution_files files)
  in
  Log.if_error' ~header:"" ~use:B0caml.Exit.miss_dep_error @@
  let all = not directory && not mod_use in
  let ds =
    if (directory || all) then directories conf script raw else Ok []
  in
  let ms = if (mod_use || all) then mod_uses script raw else Ok [] in
  let deps = match ds, ms with
  | Error de, Error me -> Error (String.concat "\n\n" [de; me])
  | Error e, _ | _, Error e -> Error e
  | Ok ds, Ok ms -> Ok (List.append ds ms)
  in
  let pp_deps = Fmt.(vbox @@ list Fpath.pp_unquoted) in
  let* deps in
  if deps <> [] then Log.stdout (fun m -> m "%a" pp_deps deps);
  Ok B0caml.Exit.ok

let exec ~conf ~mode ~script_file ~script_args =
  Log.if_error ~use:B0caml.Exit.conf_error @@
  let* conf in
  Log.if_error' ~header:"" ~use:B0caml.Exit.comp_error @@
  let* script = B0caml.get_script conf script_file in
  match mode with
  | `Source ->
      let* src = B0caml.get_source conf script in
      Log.stdout (fun m -> m "%s" src);
      Ok B0caml.Exit.ok
  | `Compile ->
      let* exe = B0caml.compile_script conf script in
      Ok B0caml.Exit.ok
  | `Exec ->
      let* exe = B0caml.compile_script conf script in
      let cmd = Cmd.list (script_file :: script_args) in
      Ok (B0caml.Exit.Exec (exe, cmd))
  | `Utop -> failwith "TODO"
  | `Top -> failwith "TODO"

let log ~conf ~script_file ~no_pager ~format ~output_details ~query =
  Log.if_error ~use:B0caml.Exit.conf_error @@
  let* conf in
  let no_pager = no_pager || format = `Trace_event in
  let* pager = B0_pager.find ~no_pager () in
  let* () = B0_pager.page_stdout pager in
  let* script_file = B0caml.get_script_file conf script_file in
  let build_dir = B0caml.script_build_dir conf ~script_file in
  let log_path = B0caml.script_build_log ~build_dir in
  Log.if_error' ~use:B0caml.Exit.miss_log_error @@
  let* log = B0_memo_log.read log_path in
  let pp =
    B0_memo_cli.Log.pp ~format ~output_details ~query ~path:log_path ()
  in
  Fmt.pr "@[<v>%a@]@?" pp log;
  Ok B0caml.Exit.ok

(* Command line interface *)

open Cmdliner
open Cmdliner.Term.Syntax

let cmd () = (* Behind a thunk, no need to evaluate on scripts *)
  let exit_info c doc = match c with
  | B0caml.Exit.Code c -> Cmd.Exit.info c ~doc
  | _ -> assert false
  in
  let exits =
    exit_info B0caml.Exit.conf_error "on configuration error." ::
    Cmd.Exit.defaults
  in
  let some_error_exit =
    exit_info B0caml.Exit.some_error
      "on indiscriminate errors reported on stderr."
  in
  let exec_exits =
    Cmd.Exit.info 0 ~max:255
      ~doc:"on script execution, the script exit code." ::
    exit_info B0caml.Exit.comp_error "on script compilation error." ::
    exits
  in
  let conf = B0caml.Conf.of_cli () in
  let sdocs = Manpage.s_common_options in
  let s_exec_modes = "EXECUTION MODES" in
  let man_see_manual = `Blocks
      [ `S Manpage.s_see_also;
        `P "Consult $(b,odig doc b0caml) for a tutorial and more details."]
  in
  let script_file =
    let doc = "The script." and docv = "SCRIPT" in
    Arg.(required & pos 0 (some string) None & info [] ~doc ~docv)
  in
  let cache_cmd =
    (* TODO redo *)
    let doc = "Manage the script cache" in
    let man = [
      `S Manpage.s_synopsis;
      `P "$(tool) $(cmd) $(i,ACTION) [$(i,OPTION)]...";
      `S Manpage.s_description;
      `P "The $(cmd) command operates on the script cache.";
      `S "ACTIONS";
      `I ("$(b,delete) [$(i,SCRIPT)]...",
          "Delete the cache or the build of the given $(i,SCRIPT).");
      `I ("$(b,gc)", "Delete unused keys (need a file systems with \
                      hardlinks)");
      `I ("$(b,path) [$(i,SCRIPT)]...", "Display the path to the cache or \
           the given script caches.");
      `I ("$(b,stats)", "Output cache statistics.");
      `I ("$(b,trim)", "Trim the cache to 50% of its size.");
      man_see_manual; ]
    in
    let exits = some_error_exit :: exits in
    Cmd.make (Cmd.info "cache" ~doc ~sdocs ~exits ~man) @@
    let+ conf
    and+ action =
      let action =
        [ "delete", `Delete; "gc", `Gc; "path", `Path; "stats", `Stats;
          "trim", `Trim ]
      in
      let doc =
        Fmt.str "The action to perform. $(docv) must be one of %s."
          (Arg.doc_alts_enum action)
      in
      let action = Arg.enum action in
      Arg.(required & pos 0 (some action) None & info [] ~doc ~docv:"ACTION")
    and+ scripts =
      let doc = "The script(s)." and docv = "SCRIPT" in
      Arg.(value & pos_right 0 string [] & info [] ~doc ~docv)
    in
    cache ~conf ~action ~scripts
  in
  let deps_cmd =
    let doc = "Output script dependencies" in
    let man = [
      `S Manpage.s_description;
      `P "$(cmd) outputs the $(b,#directory) and \
          $(b,#mod_use) dependency resolutions of a script. If \
          $(b,--raw) is specified directive arguments are output without \
          resolution.";
      `P "Without options outputs the result of both $(b,--directory) and \
          $(b,--mod-use).";
      man_see_manual; ]
    in
    let exits =
      exit_info B0caml.Exit.miss_dep_error
        "on missing $(b,#mod_use) or $(b,#directory) path." :: exits
    in
    Cmd.make (Cmd.info "deps" ~doc ~sdocs ~exits ~man) @@
    let flag f doc = Arg.(value & flag & info [f] ~doc) in
    let+ conf and+ script_file
    and+ directory =
      flag "directory"
        "Output $(b,#directory) resolutions. These resolutions \
         always have a trailing directory seperator; this can be used to
         distinguish them from $(b,--mod-use) resolutions."
    and+ mod_use = flag "mod-use" "Output $(b,#mod_use) resolutions."
    and+ root =
      flag "root" "Only list root names of $(b,+) $(b,#directory) directives. \
                   Takes over other options."
    and+ raw =
      flag "raw" "Output raw directive arguments without resolving them."
    in
    deps ~conf ~script_file ~raw ~directory ~mod_use ~root
  in
  let exec_cmd, exec_term =
    let doc = "Execute script (default command)" in
    let man = [
      `S Manpage.s_description;
      `P "The $(cmd) command executes a script with the given arguments.";
      `S s_exec_modes;
      man_see_manual;
    ]
    in
    let term =
      let+ conf
      and+ mode =
        let docs = s_exec_modes in
        let m arg doc = Arg.info [arg] ~doc ~docs in
        let modes =
          [ `Source, m "source" "Output final script source. No execution.";
            `Compile, m "compile" "Compile and cache the script. No execution.";
            `Top, m "top" "Load script in the $(b,ocaml) interactive toplevel.";
            `Utop,
            m "utop" "Load script in the $(b,utop) interactive toplevel.";
            `Exec, m "exec" "Compile and execute script (default)."; ]
        in
        Arg.(value & vflag `Exec modes)
      and+ script_file =
        let doc = "The script. Needs to contain a path separator to be \
                   recognized as such."
        in
        let docv = "SCRIPT" in
        Arg.(required & pos 0 (some string) None & info [] ~doc ~docv)
      and+ script_args =
        let doc = "Argument for the script." and docv = "ARG" in
        Arg.(value & pos_right 0 string [] & info [] ~doc ~docv)
      in
      exec ~conf ~mode ~script_file ~script_args
    in
    Cmd.make (Cmd.info "exec" ~doc ~sdocs ~exits ~man) term, term
  in
  let log_cmd =
    let doc = "Output script build logs" in
    let man = [
      `S Manpage.s_description;
      `P "$(cmd) outputs the build log of a script.";
      `Blocks B0_memo_cli.Op.query_man;
      `S Manpage.s_arguments;
      `S B0_std_cli.s_output_details_options;
      `S B0_memo_cli.Log.s_output_format_options;
      `S B0_memo_cli.Op.s_selection_options;
    ]
    in
    let exits =
      exit_info B0caml.Exit.miss_dep_error "on missing log." :: exits
    in
    Cmd.make (Cmd.info "log" ~doc ~sdocs ~exits ~man) @@
    let+ conf and+ script_file
    and+ no_pager = B0_pager.no_pager ()
    and+ format = B0_memo_cli.Log.format_cli ()
    and+ output_details = B0_std_cli.output_details ()
    and+ query = B0_memo_cli.Op.query_cli () in
    log ~conf ~script_file ~no_pager  ~format ~output_details ~query
  in
  (* Main command *)
  let doc = "Easy OCaml scripts" in
  let man = [
    `S Manpage.s_synopsis;
    `Pre "$(tool) $(b,--) $(i,SCRIPT) [$(i,ARG)]..."; `Noblank;
    `Pre "$(tool) [$(i,OPTION)]... $(i,SCRIPT) -- [$(i,ARG)]..."; `Noblank;
    `Pre "$(tool) $(i,COMMAND) [$(i,OPTION)]...";
    `S Manpage.s_description;
    `P "$(tool) executes OCaml scripts. More information is available \
        in the manual, see $(b,odig doc b0caml).";
    `P "If the first argument of $(tool) has a path separator then it \
        assumes a path to script $(i,SCRIPT) and executes it with all
          remaining arguments. This is what happens on shebang execution.";
    `Pre "Use '$(tool) $(b,--help)' for help."; `Noblank;
    `Pre "Use '$(tool) $(i,COMMAND) $(b,--help)' for help on \
          command $(i,COMMAND).";
    `S Manpage.s_arguments;
    `S s_exec_modes;
    `S Manpage.s_common_options;
    man_see_manual;
    `S Manpage.s_bugs;
    `P "Report them, see $(i,https://erratique.ch/software/b0caml) for \
        contact information."; ]
  in
  let exits = exec_exits in
  let version = "%%VERSION%%" in
  let info = Cmd.(info "b0caml" ~version ~doc ~sdocs ~exits ~man) in
  Cmd.group info ~default:exec_term @@
  [cache_cmd; deps_cmd; exec_cmd; log_cmd;]

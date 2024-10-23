(*---------------------------------------------------------------------------
   Copyright (c) 2019 The b0caml programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open B0_std

let delete_script_cache c script =
  Result.bind (B0caml.get_script_file c script) @@ fun script_file ->
  let build_dir = B0caml.script_build_dir c ~script_file in
  Result.bind (Os.Dir.exists build_dir) @@ function
  | false -> Ok ()
  | true ->
      let log = B0caml.script_build_log ~build_dir in
      Result.bind (B0_memo_log.read log) @@ fun log ->
      Result.bind (Os.Path.delete ~recurse:true build_dir) @@ fun _ ->
      let add_key acc o = match B0_zero.Op.hash o with
      | k when Hash.is_nil k -> acc | k -> Hash.to_hex k :: acc
      in
      let keys = List.fold_left add_key [] (B0_memo_log.ops log) in
      let dir = B0caml.Conf.b0_cache_dir c in
      Result.bind (B0_cli.File_cache.delete ~dir (`Keys keys)) @@
      fun _ -> Ok ()

let show_script_path c script =
  Result.bind (B0caml.get_script_file c script) @@ fun script_file ->
  let build_dir = B0caml.script_build_dir c ~script_file in
  Ok (Log.stdout (fun m -> m "%a" Fpath.pp_unquoted build_dir))

let cache_cmd c action scripts =
  (* The notion of cache for `b0caml` is a bit different from a build
     system one. So B00_ui.File_cache cannot simply be reused off the
     shelf. Some adjustements should be made here or generalization there. *)
  Log.if_error ~use:B0caml.Exit.conf_error @@
  Result.bind c @@ fun c ->
  match action with
  | `Delete ->
      begin match scripts with
      | [] ->
          let cache_dir = B0caml.Conf.cache_dir c in
          let pp_path = Fmt.st' [`Bold] Fpath.pp_unquoted in
          Log.stdout begin fun m ->
            m "Deleting %a, this may take some time..." pp_path cache_dir
          end;
          Log.if_error' ~use:B0caml.Exit.some_error @@
          Result.bind (Os.Path.delete ~recurse:true cache_dir) @@ fun _ ->
          Ok B0caml.Exit.ok
      | scripts ->
          let delete acc script =
            let ok () = acc in
            let error e = Log.if_error ~use:B0caml.Exit.some_error (Error e) in
            Result.fold ~ok ~error (delete_script_cache c script)
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
            (fun m -> m "%a" Fpath.pp_unquoted (B0caml.Conf.cache_dir c));
          Ok B0caml.Exit.some_error
      | scripts ->
          let show_path acc script =
            let ok () = acc in
            let error e = Log.if_error ~use:B0caml.Exit.some_error (Error e) in
            Result.fold ~ok ~error (show_script_path c script)
          in
          Ok (List.fold_left show_path B0caml.Exit.ok scripts)
      end
  | `Stats ->
      let dir = B0caml.Conf.b0_cache_dir c in
      let used = String.Set.empty in (* TODO *)
      Result.bind (B0_cli.File_cache.stats ~dir ~used) @@
      fun _ -> Ok B0caml.Exit.ok
  | `Trim ->
      Log.stdout (fun m -> m "the trim subcommand is TODO");
      Ok B0caml.Exit.some_error

let deps_cmd c script_file raw directory mod_use root =
  Log.if_error ~use:B0caml.Exit.conf_error @@
  Result.bind c @@ fun c ->
  Log.if_error' ~header:"" ~use:B0caml.Exit.comp_error @@
  Result.bind (B0caml.get_script c script_file) @@ fun s ->
  match root with
  | true ->
      let directories = B0caml_script.directories s in
      let root (d, _) = B0caml_ocamlpath.logical_path_root_name d in
      let roots = String.distinct (List.filter_map root directories) in
      let pp_roots = Fmt.(vbox @@ list string) in
      if roots <> [] then Log.stdout (fun m -> m "%a" pp_roots roots);
      Ok B0caml.Exit.ok
  | false ->
      let directories c s raw =
        if raw then Ok (List.map fst (B0caml_script.directories s)) else
        let ocamlpath = B0caml.Conf.ocamlpath c in
        Result.map_error (B0caml.Err.directories ~ocamlpath) @@
        Result.bind (B0caml_script.resolve_directories ~ocamlpath s) @@
        fun dirs -> Ok (List.map B0caml_script.directory_resolution_dir dirs)
      in
      let mod_uses s raw =
        if raw then Ok (List.map fst (B0caml_script.mod_uses s)) else
        Result.map_error B0caml.Err.mod_uses @@
        Result.bind (B0caml_script.resolve_mod_uses s) @@ fun files ->
        Ok (List.concat_map B0caml_script.mod_use_resolution_files files)
      in
      Log.if_error' ~header:"" ~use:B0caml.Exit.miss_dep_error @@
      let all = not directory && not mod_use in
      let ds = if (directory || all) then directories c s raw else Ok [] in
      let ms = if (mod_use || all) then mod_uses s raw else Ok [] in
      let deps = match ds, ms with
      | Error de, Error me -> Error (String.concat "\n\n" [de; me])
      | Error e, _ | _, Error e -> Error e
      | Ok ds, Ok ms -> Ok (List.append ds ms)
      in
      let pp_deps = Fmt.(vbox @@ list Fpath.pp_unquoted) in
      Result.bind deps @@ fun deps ->
      if deps <> [] then Log.stdout (fun m -> m "%a" pp_deps deps);
      Ok B0caml.Exit.ok

let exec_cmd c mode script_file script_args =
  Log.if_error ~use:B0caml.Exit.conf_error @@
  Result.bind c @@ fun c ->
  Log.if_error' ~header:"" ~use:B0caml.Exit.comp_error @@
  Result.bind (B0caml.get_script c script_file) @@ fun s ->
  match mode with
  | `Source ->
      Result.bind (B0caml.get_source c s) @@ fun src ->
      Log.stdout (fun m -> m "%s" src); Ok B0caml.Exit.ok
  | `Compile ->
      Result.bind (B0caml.compile_script c s) @@ fun exe ->
      Ok B0caml.Exit.ok
  | `Exec ->
      Result.bind (B0caml.compile_script c s) @@ fun exe ->
      let cmd = Cmd.list (script_file :: script_args) in
      Ok (B0caml.Exit.Exec (exe, cmd))
  | `Utop -> failwith "TODO"
  | `Top -> failwith "TODO"

let log_cmd c script_file no_pager format details op_selector =
  Log.if_error ~use:B0caml.Exit.conf_error @@
  Result.bind c @@ fun c ->
  let don't = no_pager || format = `Trace_event in
  Result.bind (B0_pager.find ~don't ()) @@ fun pager ->
  Result.bind (B0_pager.page_stdout pager) @@ fun () ->
  Result.bind (B0caml.get_script_file c script_file) @@ fun script_file ->
  let build_dir = B0caml.script_build_dir c ~script_file in
  let log = B0caml.script_build_log ~build_dir in
  Log.if_error' ~use:B0caml.Exit.miss_log_error @@
  Result.bind (B0_memo_log.read log) @@ fun l ->
  B0_cli.Memo.Log.out Fmt.stdout format details op_selector ~path:log l;
  Ok B0caml.Exit.ok

(* Command line interface *)

open Cmdliner

let conf () =
  let docs = Manpage.s_common_options in
  let comp_target =
    let env =
      let doc = "Force default compilation target to $(b,byte), $(b,native) \
                 or $(b,auto). See $(b,--byte) and $(b,--native) options."
      in
      Cmd.Env.info ~doc B0caml.Env.comp_target
    in
    let targets =
      let t enum arg doc = Some enum, Arg.info [arg] ~doc ~docs ~env in
      [ t `Byte "byte" "Compile to bytecode (default if no native code).";
        t `Native "native" "Compile to native code (default if available)." ]
    in
    let cli_arg = Arg.(value & vflag None targets) in
    (* cmdliner doesn't support env for vflag we do it manually here *)
    let target = function
    | Some _ as t -> t
    | None ->
        Log.if_error ~use:None @@
        Os.Env.find' ~empty_is_none:true
          B0caml.Conf.comp_target_of_string B0caml.Env.comp_target
    in
    Term.(const target $ cli_arg)
  in
  let cache_dir =
    let env = Cmd.Env.info B0caml.Env.cache_dir in
    let doc = "Cache directory." and docv = "PATH" in
    let none = "$(b,XDG_CACHE_HOME)/b0caml" in
    Arg.(value & opt (Arg.some ~none B0_std_cli.fpath) None &
         info ["cache-dir"] ~doc ~docv ~docs ~env)
  in
  let color =
    let env = Cmd.Env.info B0caml.Env.color in
    B0_std_cli.color ~docs ~env ()
  in
  let log_level =
    let env = Cmd.Env.info B0caml.Env.verbosity in
    B0_std_cli.log_level ~docs ~env ()
  in
  let conf cache_dir color log_level comp_target =
    B0caml.Conf.setup_with_cli ~cache_dir ~comp_target ~color ~log_level ()
  in
  Term.(const conf $ cache_dir $ color $ log_level $ comp_target)

let cmd () =
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
  let conf = conf () in
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
    let man_xrefs = [`Main; `Tool "b00-cache"] in
    let man = [
      `S Manpage.s_synopsis;
      `P "$(mname) $(tname) $(i,ACTION) [$(i,OPTION)]...";
      `S Manpage.s_description;
      `P "The $(tname) command operates on the script cache.";
      `S "ACTIONS";
      `I ("$(b,delete) [$(i,SCRIPT)]...",
          "Delete the cache or the build of the given $(i,SCRIPT).");
      `I ("$(b,gc)", "Delete unused keys (need a file systems with \
                      hardlinks)");
      `I ("$(b,path) [$(i,SCRIPT)]...", "Display the path to the cache or \
           the given script caches.");
      `I ("$(b,stats)", "Show cache statistics.");
      `I ("$(b,trim)", "Trim the cache to 50% of its size.");
      man_see_manual; ]
    in
    let exits = some_error_exit :: exits in
    let action =
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
    in
    let scripts =
      let doc = "The script(s)." and docv = "SCRIPT" in
      Arg.(value & pos_right 0 string [] & info [] ~doc ~docv)
    in
    Cmd.v (Cmd.info "cache" ~doc ~sdocs ~exits ~man ~man_xrefs)
      Term.(const cache_cmd $ conf $ action $ scripts)
  in
  let deps_cmd =
    let doc = "Show script dependencies" in
    let man_xrefs = [`Main ] in
    let man = [
      `S Manpage.s_description;
      `P "The $(tname) command shows the $(b,#directory) and \
          $(b,#mod_use) dependency resolutions of a script. If \
          $(b,--raw) is specified directive arguments are shown without \
          resolution.";
      `P "Without options shows the result of both $(b,--directory) and \
          $(b,--mod-use).";
      man_see_manual; ]
    in
    let exits =
      exit_info B0caml.Exit.miss_dep_error
        "on missing $(b,#mod_use) or $(b,#directory) path." :: exits
    in
    let flag f doc = Arg.(value & flag & info [f] ~doc) in
    let directory =
      flag "directory"
        "Show $(b,#directory) resolutions. These resolutions \
         always have a trailing directory seperator; this can be used to
         distinguish them from $(b,--mod-use) resolutions."
    in
    let mod_use = flag "mod-use" "Show $(b,#mod_use) resolutions." in
    let root =
      flag "root" "Only list root names of $(b,+) $(b,#directory) directives. \
                   Takes over other options."
    in
    let raw =
      flag "raw" "Show raw directive arguments without resolving them."
    in
    Cmd.v (Cmd.info "deps" ~doc ~sdocs ~exits ~man ~man_xrefs)
      Term.(const deps_cmd $ conf $ script_file $ raw $ directory $
            mod_use $ root)
  in
  let exec_cmd, exec_term =
    let doc = "Execute script (default command)" and  man_xrefs = [`Main ] in
    let man = [
      `S Manpage.s_description;
      `P "The $(tname) command executes a script with the given arguments.";
      `S s_exec_modes;
      man_see_manual;
    ]
    in
    let mode =
      let docs = s_exec_modes in
      let modes =
        let m arg doc = Arg.info [arg] ~doc ~docs in
        [ `Source, m "source" "Output final script source. No execution.";
          `Compile, m "compile" "Compile and cache the script. No execution.";
          `Top, m "top" "Load script in the $(b,ocaml) interactive toplevel.";
          `Utop, m "utop" "Load script in the $(b,utop) interactive toplevel.";
          `Exec, m "exec" "Compile and execute script (default)."; ]
      in
      Arg.(value & vflag `Exec modes)
    in
    let script_file =
      let doc = "The script. Needs to contain a path separator to be \
                 recognized as such."
      in
      let docv = "SCRIPT" in
      Arg.(required & pos 0 (some string) None & info [] ~doc ~docv)
    in
    let args =
      let doc = "Argument for the script." and docv = "ARG" in
      Arg.(value & pos_right 0 string [] & info [] ~doc ~docv)
    in
    let term = Term.(const exec_cmd $ conf $ mode $ script_file $ args) in
    Cmd.v (Cmd.info "exec" ~doc ~sdocs ~exits ~man ~man_xrefs) term, term
  in
  let log_cmd =
    let doc = "Show script build logs" in
    let man_xrefs = [`Main; `Tool "b00-log"] in
    let docs_format = "OUTPUT FORMAT" in
    let docs_details = "OUTPUT DETAILS" in
    let docs_select = "OPTIONS FOR SELECTING OPERATIONS" in
    let envs = B0_pager.Env.infos in
    let man = [
      `S Manpage.s_description;
      `P "The $(tname) command shows build log of a script.";
      `Blocks B0_cli.Op.query_man;
      `S docs_format;
      `S docs_details;
      `S docs_select; ]
    in
    let exits =
      exit_info B0caml.Exit.miss_dep_error "on missing log." :: exits
    in
    Cmd.v (Cmd.info "log" ~doc ~sdocs ~exits ~envs ~man ~man_xrefs)
      Term.(const log_cmd $ conf $ script_file $ B0_pager.don't () $
            B0_cli.Memo.Log.out_format_cli ~docs:docs_details () $
            B0_std_cli.output_format ~docs:docs_format () $
            B0_cli.Op.query_cli ~docs:docs_select ())
  in
  let main_cmd =
    let doc = "Easy OCaml scripts" in
    let man = [
      `S Manpage.s_synopsis;
      `Pre "$(mname) $(i,SCRIPT) [$(i,ARG)]..."; `Noblank;
      `Pre "$(mname) [$(i,OPTION)]... $(i,SCRIPT) -- [$(i,ARG)]..."; `Noblank;
      `Pre "$(mname) $(i,COMMAND) [$(i,OPTION)]...";
      `S Manpage.s_description;
      `P "$(mname) executes OCaml scripts. More information is available \
          in the manual, see $(b,odig doc b0caml).";
      `P "If the first argument of $(mname) has a path separator then it \
          assumes a path to script $(i,SCRIPT) and executes it with all
          remaining arguments. This is what happens on shebang execution.";
      `Pre "Use '$(mname) $(b,--help)' for help."; `Noblank;
      `Pre "Use '$(mname) $(i,COMMAND) $(b,--help)' for help on \
            command $(i,COMMAND).";
      `S Manpage.s_arguments;
      `S s_exec_modes;
      `S Manpage.s_common_options;
      man_see_manual;
      `S Manpage.s_bugs;
      `P "Report them, see $(i,%%PKG_HOMEPAGE%%) for contact information."; ]
    in
    let exits = exec_exits in
    Cmd.group
      Cmd.(info "b0caml" ~version:"%%VERSION%%" ~doc ~sdocs ~exits ~man)
      ~default:exec_term [cache_cmd; deps_cmd; exec_cmd; log_cmd;]
  in
  main_cmd

(*---------------------------------------------------------------------------
   Copyright (c) 2019 The b0caml programmers

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)

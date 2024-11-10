(*---------------------------------------------------------------------------
   Copyright (c) 2019 The b0caml programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open B0_std
open Fut.Syntax

module Exit = struct
  type t = Code of int | Exec of Fpath.t * Cmd.t
  let code = function Code c -> c | _ -> invalid_arg "not an exit code"
  let conf_error = Code 123
  let comp_error = Code 127
  let ok = Code 0
  let miss_dep_error = Code 1
  let miss_log_error = Code 1
  let some_error = Code 122
end

module Env = struct
  let cache_dir = "B0CAML_CACHE_DIR"
  let color = "B0CAML_COLOR"
  let comp_target = "B0CAML_COMPILATION_TARGET"
  let verbosity = "B0CAML_VERBOSITY"
end

module Conf = struct
  type comp_target = [ `Auto | `Byte | `Native ]
  let get_comp_target = Option.value ~default:`Auto
  let comp_target_of_string s = match String.trim s with
  | "auto" -> Ok `Auto
  | "byte" -> Ok `Byte
  | "native" -> Ok `Native
  | e ->
      let pp_target = Fmt.code in
      let kind = Fmt.any "compilation target" in
      let dom = ["auto"; "byte"; "native"] in
      Fmt.error "%a" Fmt.(unknown' ~kind pp_target ~hint:must_be) (e, dom)

  let get_cache_dir ~cwd = function
  | Some d -> Ok Fpath.(cwd // d)
  | None ->
      Result.bind (Os.Dir.cache ()) @@ fun cache ->
      Ok Fpath.(cache / "b0caml")

  let get_memo ~cwd ~cache_dir =
    let feedback =
      let op_howto ppf o = Fmt.pf ppf "b0caml log --id %d" (B0_zero.Op.id o) in
      let show_op = Log.Info and show_ui = Log.Error and level = Log.level () in
      B0_cli.Memo.pp_leveled_feedback ~op_howto ~show_op ~show_ui ~level
        Fmt.stderr
    in
    let trash_dir = Fpath.(cache_dir / B0_cli.Memo.trash_dir_name) in
    let jobs = 4 in
    B0_memo.make ~cwd ~cache_dir ~trash_dir ~jobs ~feedback ()

  type t =
    { cache_dir : Fpath.t;
      b0_cache_dir : Fpath.t;
      comp_target : comp_target;
      cwd : Fpath.t;
      log_level : Log.level;
      memo : (B0_memo.t, string) result Lazy.t;
      ocamlpath : B0caml_ocamlpath.t;
      fmt_styler : Fmt.styler; }

  let v ~cache_dir ~comp_target ~cwd ~log_level ~ocamlpath ~fmt_styler () =
    let b0_cache_dir = Fpath.(cache_dir / B0_cli.Memo.cache_dir_name) in
    let memo = lazy (get_memo ~cwd ~cache_dir:b0_cache_dir) in
    { cache_dir; b0_cache_dir; comp_target; cwd; log_level; memo; ocamlpath;
      fmt_styler }

  let cache_dir c = c.cache_dir
  let b0_cache_dir c = c.b0_cache_dir
  let comp_target c = c.comp_target
  let cwd c = c.cwd
  let log_level c = c.log_level
  let memo c = Lazy.force c.memo
  let ocamlpath c = c.ocamlpath
  let fmt_styler c = c.fmt_styler

  let env_find parse var =
    Os.Env.find' ~empty_is_none:true parse var |> Log.if_error ~use:None

  let setup ~cache_dir ~comp_target ~log_level ~color () =
    let fmt_styler = B0_std_cli.get_styler color in
    let log_level = B0_std_cli.get_log_level log_level in
    B0_std_cli.setup fmt_styler log_level ~log_spawns:Log.Debug;
    Result.bind (Os.Dir.cwd ()) @@ fun cwd ->
    Result.bind (get_cache_dir ~cwd cache_dir) @@ fun cache_dir ->
    let comp_target = get_comp_target comp_target in
    Result.bind (B0caml_ocamlpath.get None) @@ fun ocamlpath ->
    Ok (v ~cache_dir ~comp_target ~cwd ~log_level ~ocamlpath ~fmt_styler ())

  let setup_with_cli = setup
  let setup_without_cli () =
    let cache_dir = env_find Fpath.of_string Env.cache_dir in
    let comp_target = env_find comp_target_of_string Env.comp_target in
    let color = env_find B0_std_cli.styler_of_string Env.color in
    let log_level = env_find Log.level_of_string Env.verbosity in
    setup ~cache_dir ~comp_target ~color ~log_level ()
end

module Err = struct
  let pp_logical_suggestions ~logical_dirs ~uninstalled ppf dir =
    let dirs = B0caml_ocamlpath.logical_dir_suggestions ~logical_dirs dir in
    let pkg = Option.get (B0caml_ocamlpath.logical_path_root_name dir) in
    let pkgs = B0caml_opam.pkg_suggestions ~pkgs:uninstalled ~pkg in
    match dirs, pkgs with
    | None, None -> ()
    | Some dirs, None -> B0caml_ocamlpath.pp_did_you_mean_logical_dirs ppf dirs
    | None, Some opam -> B0caml_opam.pp_maybe_try_install ~alt:false ppf opam
    | Some dirs, Some opam ->
        B0caml_ocamlpath.pp_did_you_mean_logical_dirs ppf dirs;
        B0caml_opam.pp_maybe_try_install ~alt:true ppf opam

  let directories ~ocamlpath errs =
    let logical_dirs =
      Log.time (fun _ m -> m "logical dir domain") @@ fun () ->
      Log.if_error ~use:Fpath.Set.empty @@
      B0caml_ocamlpath.logical_dirs ocamlpath
    in
    let uninstalled =
      Log.time (fun _ m -> m "opam list uninstalled") @@ fun () ->
      Log.if_error ~use:String.Set.empty @@
      B0caml_opam.uninstalled ()
    in
    let directory (dir, m, err) = match err with
    | `Error e -> B0caml_script.loc_errf m " %s" e
    | `Miss ->
        let pp_bold pp = Fmt.st' [`Bold] pp in
        match B0caml_ocamlpath.classify_path dir with
        | `Concrete dir ->
            B0caml_script.loc_errf
              m " Missing directory %a" (pp_bold Fpath.pp_unquoted) dir
        | `Logical rdir ->
            B0caml_script.loc_errf
              m " @[<v>@[Directory %a not found in any %a directories.@]%a@]"
              (pp_bold Fpath.pp_unquoted) rdir
              (pp_bold Fmt.string) "OCAMLPATH"
              (pp_logical_suggestions ~logical_dirs ~uninstalled) dir
    in
    String.concat "\n\n" (List.map directory errs)

  let mod_uses errs =
    let mod_use (file, m, err) = match err with
    | `Error e -> B0caml_script.loc_errf m " %s" e
    | `Miss ->
        B0caml_script.loc_errf
          m " Missing file %a" (Fmt.st' [`Bold] Fpath.pp_unquoted) file
    in
    String.concat "\n\n" (List.map mod_use errs)
end

let get_script_file c file =
  if file = "-" then Ok Fpath.dash else
  Result.bind (Fpath.of_string file) @@ fun file ->
  Os.Path.realpath file

let script_build_log ~build_dir = Fpath.(build_dir / "log")

let script_build_dir c ~script_file =
  (* A bit unclear what we want to use here maybe add what
     affects compilation environment *)
  let file = Fpath.to_string @@ script_file in
  let hash = Hash.to_hex @@ Hash.Xxh3_64.string file in
  Fpath.(Conf.cache_dir c / hash)

let get_script c file =
  Result.bind (get_script_file c file) @@ fun script ->
  Result.bind (Os.File.read script) @@ fun src ->
  Result.bind (B0caml_script.of_string ~file:script src) @@ fun s -> Ok s

let get_source c s =
  let map_error = Result.map_error Err.mod_uses in
  Result.bind (map_error (B0caml_script.resolve_mod_uses s)) @@
  fun mod_use_resolutions -> B0caml_script.src ~mod_use_resolutions s

(* Compilation *)

let write_source m build_dir s ~mod_uses ~src_file =
  let mod_use_files = B0caml_script.mod_use_resolution_files in
  let mod_uses_files = List.concat_map mod_use_files mod_uses in
  let reads = B0caml_script.file s :: mod_uses_files in
  B0_memo.ready_files m reads;
  B0_memo.write m ~reads src_file @@ fun () ->
  B0caml_script.src ~mod_use_resolutions:mod_uses s

(* There are various way one could go about this.

   1. ocamldep to get approx. of module names, resolve these module names
      in [dirs]. Get archives and their deps
   2. compile with -c then ocamlobjinfo to get actual names. Then resolve
      these in -I dirs, find archives and their recursive deps.
   3. Brutal like 1. but don't even call ocamldep. Give all the archives
      present in the -I dirs and find their rec. deps. Only depend
      on these files. *)

let byte_comp m = B0_memo.tool m B0_ocaml.Tool.ocamlc, `Byte
let native_comp m = B0_memo.tool m B0_ocaml.Tool.ocamlopt, `Native

let find_comp c m = match Conf.comp_target c with
| `Byte -> Fut.return (byte_comp m)
| `Native -> Fut.return (native_comp m)
| `Auto ->
    let* ocamlopt = B0_memo.tool_opt m B0_ocaml.Tool.ocamlopt in
    Fut.return @@ match ocamlopt with
    | None -> byte_comp m
    | Some comp -> comp, `Native

let compile_source m (comp, code) r build_dir s ~dirs ~src_file =
  let dirs = List.map B0caml_script.directory_resolution_dir dirs in
  let ocamlpath = B0caml_resolver.ocamlpath r in
  (* Automatically add ocaml libs *)
  let dirs = B0caml_ocamlpath.ocaml_logical_dir ocamlpath :: dirs in
  let* archives = B0caml_resolver.find_archives_and_deps r ~code ~dirs in
  let archives = List.map B0_ocaml.Cobj.file archives in
  let incs = Cmd.unstamp @@ Cmd.paths ~slip:"-I" dirs in
  let base = Fpath.strip_ext src_file in
  let writes = match code with
  | `Byte -> [ Fpath.(base + ".cmo") ]
  | `Native -> [ Fpath.(base + ".cmx"); Fpath.(base + ".o") ]
  in
  let exe = Fpath.(build_dir / "exe" ) in
  let writes = exe :: Fpath.(base + ".cmi") :: writes in
  let reads = src_file :: archives (* FIXME add C libs. *) in
  B0_memo.ready_files m archives;
  B0_memo.spawn m ~reads ~writes @@
  comp Cmd.(arg "-o" %% unstamp (path exe) %% arg "-nostdlib" %%
            arg "-opaque" %%
            (unstamp @@ (incs %% paths archives %% path src_file)));
  Fut.return ()

let maybe_write_build_log m ~build_dir =
  (* Only write log if there's a failure or something was spawned *)
  (*
  let spawn_execs m =
    let spawn_exec o = match B0_zero.Op.kind o with
    | B0_zero.Op.Spawn _ when not (B0_zero.Op.revived o) -> true | _ -> false
    in
    List.exists spawn_exec (B0_memo.ops m)
  in
  if not (B0_memo.has_failures m) && not (spawn_execs m) then () else
*)
  (* For now always write log. Let's see how much it gets on the budget. *)
  let log_file = script_build_log ~build_dir in
  Log.if_error ~use:() (B0_memo_log.(write log_file (of_memo m)))

let compile_script c s =
  let ocamlpath = Conf.ocamlpath c in
  let dirs = B0caml_script.resolve_directories ~ocamlpath s in
  let mod_uses = B0caml_script.resolve_mod_uses s in
  match dirs, mod_uses with
  | Ok dirs, Ok mod_uses ->
      Result.bind (Conf.memo c) @@ fun m ->
      let memo_dir = Fpath.(Conf.cache_dir c / "lib_resolve") in
      let r = B0caml_resolver.create m ~memo_dir ~ocamlpath in
      let build_dir = script_build_dir c ~script_file:(B0caml_script.file s) in
      let src_file = Fpath.(build_dir / "src.ml") in
      let exe = Fpath.(build_dir / "exe") in
      begin
        ignore @@
        let* comp = find_comp c m in
        (* FIXME gets also rid of log, but is needed
           for src updates. Needs to fix b0
           B0_memo.delete m build_dir @@ fun () -> *)
        let* () = B0_memo.mkdir m build_dir in
        write_source m build_dir s ~mod_uses ~src_file;
        compile_source m comp r build_dir s ~dirs ~src_file
      end;
      B0_memo.stir m ~block:true;
      let ret = match B0_memo.status m with
      | Ok () -> Ok exe
      | Error e ->
          let s = B0caml_script.file s in
          let read_howto ppf _ =
            Fmt.pf ppf "b0caml log %a -r " Fpath.pp_quoted s
          in
          let write_howto ppf _ =
            Fmt.pf ppf "b0caml log %a -w " Fpath.pp_quoted s
          in
          Fmt.error "%a"
            (B0_zero_conv.Op.pp_aggregate_error ~read_howto ~write_howto ()) e
      in
      maybe_write_build_log m ~build_dir;
      ret
  | dirs, mod_uses ->
      let dir_errs = Result.map_error (Err.directories ~ocamlpath) dirs in
      let dir_errs = match dir_errs with Error e -> e | Ok _ -> "" in
      let mod_errs = Result.map_error Err.mod_uses mod_uses in
      let mod_errs = match mod_errs with Error e -> e | Ok _ -> "" in
      Error (String.concat "\n\n" [dir_errs; mod_errs])

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

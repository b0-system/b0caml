(*---------------------------------------------------------------------------
   Copyright (c) 2019 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open B0_std.Fut.Syntax

let ocamlpath_root_dirs ~ocamlpath =
  let add_dir acc dir =
    let add_root _ name p ds = String.Map.add_to_list name p ds in
    Log.if_error ~use:acc
      (Os.Dir.fold_dirs ~rel:false ~recurse:false add_root dir acc)
  in
  let ocamlpath = B0caml_ocamlpath.dirs ocamlpath in
  List.fold_left add_dir String.Map.empty (List.rev ocamlpath)

type t =
  { m : B0_memo.t;
    memo_dir : Fpath.t;
    ocamlpath : B0caml_ocamlpath.t;
    ocamlpath_root_dirs : Fpath.t list String.Map.t;
    mutable dir_dirs : Fpath.Set.t Fpath.Map.t;
    mutable dir_cobjs :
      B0_ocaml.Cobj.t list Fut.t Fpath.Map.t; (* Mapped by dir. *)
    mutable mod_ref_cobj : B0_ocaml.Cobj.t list B0_ocaml.Modref.Map.t; }

let create m ~memo_dir ~ocamlpath =
  { m; memo_dir; ocamlpath;
    ocamlpath_root_dirs = ocamlpath_root_dirs ~ocamlpath;
    dir_dirs = Fpath.Map.empty;
    dir_cobjs = Fpath.Map.empty; mod_ref_cobj = B0_ocaml.Modref.Map.empty }

let ocamlpath r = r.ocamlpath

let index_dir ~ext r dir =
  B0_memo.fail_if_error r.m @@
  let add st _ f acc = match st.Unix.st_kind with
  | Unix.S_DIR ->
      r.dir_dirs <- Fpath.Map.add_to_set (module Fpath.Set) dir f r.dir_dirs;
      acc
  | _ -> if Fpath.has_ext ext f then (f :: acc) else acc
  in
  (Os.Dir.fold ~recurse:false add dir [])

let get_cobjs_info r ~ext dir = match Fpath.Map.find dir r.dir_cobjs with
| info -> info
| exception Not_found ->
    let info, set_info = Fut.make () in
    r.dir_cobjs <- Fpath.Map.add dir info r.dir_cobjs;
    begin
      let cobjs = index_dir ~ext r dir in
      let o =
        let base = Fpath.basename dir in
        let uniq =
          Hash.to_hex (B0_memo.hash_string r.m (Fpath.to_string dir))
        in
        Fpath.(r.memo_dir / Fmt.str "%s-%s%s.info" base uniq ext)
      in
      B0_memo.ready_files r.m cobjs;
      if ext = ".cmxa" then begin
        List.iter (fun o -> B0_memo.ready_file r.m (Fpath.set_ext ".a" o)) cobjs
      end;
      B0_ocaml.Cobj.write r.m ~cobjs ~o;
      ignore @@
      let* cobjs = B0_ocaml.Cobj.read r.m o in
      let add_mod_ref cobj def =
        r.mod_ref_cobj <-
          B0_ocaml.Modref.Map.add_to_list def cobj r.mod_ref_cobj
      in
      let add_mod_refs cobj =
        B0_ocaml.Modref.Set.iter (add_mod_ref cobj) (B0_ocaml.Cobj.defs cobj)
      in
      List.iter add_mod_refs cobjs;
      set_info cobjs;
      Fut.return ()
    end;
    info

let try_find_mod_ref_root_dir r ref =
  let name = String.Ascii.lowercase (B0_ocaml.Modref.name ref) in
  match String.Map.find name r.ocamlpath_root_dirs with
  | p -> Some p
  | exception Not_found ->
      match String.cut_left ~sep:"_" name with
      | None -> None
      | Some (l, _) ->
          match String.Map.find name r.ocamlpath_root_dirs with
          | p -> Some p
          | exception Not_found -> None

let amb r ~ext ref cobjs =
  let pext = ".p" ^ ext in (* TODO doc filter out profile objects *)
  let not_pext cobj = not (Fpath.has_ext pext (B0_ocaml.Cobj.file cobj)) in
  match List.filter not_pext cobjs with
  | [cobj] -> Fut.return (Some cobj)
  | cobjs ->
      (* FIXME constraints. *)
      B0_memo.notify r.m `Info "@[<v>ambiguous resolution for %a:@,%a@]"
        B0_ocaml.Modref.pp ref (Fmt.list B0_ocaml.Cobj.pp) cobjs;
      Fut.return None

let find_archive_for_mod_ref r ~ext ref =
  match B0_ocaml.Modref.Map.find ref r.mod_ref_cobj with
  | [cobj] -> Fut.return (Some cobj)
  | cobjs -> amb r ~ext ref cobjs
  | exception Not_found ->
      match try_find_mod_ref_root_dir r ref with
      | None -> Fut.return None
      | Some roots ->
          let root = List.hd roots (* FIXME *) in
          let* _ = get_cobjs_info r ~ext root in
          match B0_ocaml.Modref.Map.find ref r.mod_ref_cobj with
          | [cobj] -> Fut.return (Some cobj)
          | cobjs -> amb r ~ext ref cobjs
          | exception Not_found ->
              Fut.return None (* TODO subdirs and eventually whole scan *)

let rec find_mod_refs r ~deps ~ext cobjs defined todo =
  match B0_ocaml.Modref.Set.choose todo with
  | exception Not_found ->
      let cobjs = B0_ocaml.Cobj.Set.elements cobjs in
      let cobjs, _ = B0_ocaml.Cobj.sort ~deps cobjs in
      Fut.return cobjs
  | ref ->
      let todo = B0_ocaml.Modref.Set.remove ref todo in
      match B0_ocaml.Modref.Set.mem ref defined with
      | true -> find_mod_refs r ~deps ~ext cobjs defined todo
      | false ->
          Fut.bind (find_archive_for_mod_ref r ~ext ref) @@ function
          | None ->
              B0_memo.notify r.m `Info
                "No resolution for %a" B0_ocaml.Modref.pp ref;
              find_mod_refs r ~deps ~ext cobjs defined todo
          | Some cobj ->
              let cobjs = B0_ocaml.Cobj.Set.add cobj cobjs in
              let defined =
                B0_ocaml.Modref.Set.union (B0_ocaml.Cobj.defs cobj) defined
              in
              let new_refs = B0_ocaml.Modref.Set.diff (deps cobj) defined in
              let todo = B0_ocaml.Modref.Set.union todo new_refs in
              find_mod_refs r ~deps ~ext cobjs defined todo

let find_archives_and_deps ?(deps = B0_ocaml.Cobj.link_deps) r ~code ~dirs =
  let ext = match code with `Byte -> ".cma" | `Native -> ".cmxa" in
  let* roots = Fut.of_list (List.map (get_cobjs_info r ~ext) dirs) in
  let roots = List.concat roots in
  let defined, to_find =
    let rec loop defs ldeps = function
    | [] -> defs, B0_ocaml.Modref.Set.diff ldeps defs
    | cobj :: cobjs ->
      let defs = B0_ocaml.Modref.Set.union (B0_ocaml.Cobj.defs cobj) defs in
      let ldeps = B0_ocaml.Modref.Set.union (deps cobj) ldeps in
      loop defs ldeps cobjs
    in
    loop B0_ocaml.Modref.Set.empty B0_ocaml.Modref.Set.empty roots
  in
  find_mod_refs r ~deps ~ext (B0_ocaml.Cobj.Set.of_list roots) defined to_find

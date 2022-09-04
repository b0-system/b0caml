(*---------------------------------------------------------------------------
   Copyright (c) 2019 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open B0_std
open B0_std.Fut.Syntax
open B00
open B00_ocaml

let ocamlpath_root_dirs ~ocamlpath =
  let add_dir acc dir =
    let add_root _ name p ds = String.Map.add_to_list name p ds in
    Log.if_error ~use:acc
      (Os.Dir.fold_dirs ~rel:false ~recurse:false add_root dir acc)
  in
  let ocamlpath = B0caml_ocamlpath.dirs ocamlpath in
  List.fold_left add_dir String.Map.empty (List.rev ocamlpath)

type t =
  { m : Memo.t;
    memo_dir : Fpath.t;
    ocamlpath : B0caml_ocamlpath.t;
    ocamlpath_root_dirs : Fpath.t list String.Map.t;
    mutable dir_dirs : Fpath.Set.t Fpath.Map.t;
    mutable dir_cobjs : Cobj.t list Fut.t Fpath.Map.t; (* Mapped by dir. *)
    mutable mod_ref_cobj : Cobj.t list Mod.Ref.Map.t; }

let create m ~memo_dir ~ocamlpath =
  { m; memo_dir; ocamlpath;
    ocamlpath_root_dirs = ocamlpath_root_dirs ~ocamlpath;
    dir_dirs = Fpath.Map.empty;
    dir_cobjs = Fpath.Map.empty; mod_ref_cobj = Mod.Ref.Map.empty }

let ocamlpath r = r.ocamlpath

let index_dir ~ext r dir =
  Memo.fail_if_error r.m @@
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
    let info, set_info = Fut.create () in
    r.dir_cobjs <- Fpath.Map.add dir info r.dir_cobjs;
    begin
      let cobjs = index_dir ~ext r dir in
      let o =
        let base = Fpath.basename dir in
        let uniq = Hash.to_hex (Memo.hash_string r.m (Fpath.to_string dir)) in
        Fpath.(r.memo_dir / Fmt.str "%s-%s%s.info" base uniq ext)
      in
      List.iter (Memo.file_ready r.m) cobjs;
      if ext = ".cmxa" then begin
        List.iter (fun o -> Memo.file_ready r.m (Fpath.set_ext ".a" o)) cobjs
      end;
      Cobj.write r.m ~cobjs ~o;
      ignore @@
      let* cobjs = Cobj.read r.m o in
      let add_mod_ref cobj def =
        r.mod_ref_cobj <-
          Mod.Ref.Map.add_to_list def cobj r.mod_ref_cobj
      in
      let add_mod_refs cobj =
        Mod.Ref.Set.iter (add_mod_ref cobj) (Cobj.defs cobj)
      in
      List.iter add_mod_refs cobjs;
      set_info cobjs;
      Fut.return ()
    end;
    info

let try_find_mod_ref_root_dir r ref =
  let name = String.Ascii.lowercase (Mod.Ref.name ref) in
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
  let not_pext cobj = not (Fpath.has_ext pext (Cobj.file cobj)) in
  match List.filter not_pext cobjs with
  | [cobj] -> Fut.return (Some cobj)
  | cobjs ->
      (* FIXME constraints. *)
      Memo.notify r.m `Info "@[<v>ambiguous resolution for %a:@,%a@]"
        Mod.Ref.pp ref (Fmt.list Cobj.pp) cobjs;
      Fut.return None

let find_archive_for_mod_ref r ~ext ref =
  match Mod.Ref.Map.find ref r.mod_ref_cobj with
  | [cobj] -> Fut.return (Some cobj)
  | cobjs -> amb r ~ext ref cobjs
  | exception Not_found ->
      match try_find_mod_ref_root_dir r ref with
      | None -> Fut.return None
      | Some roots ->
          let root = List.hd roots (* FIXME *) in
          let* _ = get_cobjs_info r ~ext root in
          match Mod.Ref.Map.find ref r.mod_ref_cobj with
          | [cobj] -> Fut.return (Some cobj)
          | cobjs -> amb r ~ext ref cobjs
          | exception Not_found ->
              Fut.return None (* TODO subdirs and eventually whole scan *)

let rec find_mod_refs r ~deps ~ext cobjs defined todo =
  match Mod.Ref.Set.choose todo with
  | exception Not_found ->
      let cobjs = Cobj.Set.elements cobjs in
      let cobjs, _ = Cobj.sort ~deps cobjs in
      Fut.return cobjs
  | ref ->
      let todo = Mod.Ref.Set.remove ref todo in
      match Mod.Ref.Set.mem ref defined with
      | true -> find_mod_refs r ~deps ~ext cobjs defined todo
      | false ->
          Fut.bind (find_archive_for_mod_ref r ~ext ref) @@ function
          | None ->
              Memo.notify r.m `Info "No resolution for %a" Mod.Ref.pp ref;
              find_mod_refs r ~deps ~ext cobjs defined todo
          | Some cobj ->
              let cobjs = Cobj.Set.add cobj cobjs in
              let defined = Mod.Ref.Set.union (Cobj.defs cobj) defined in
              let new_refs = Mod.Ref.Set.diff (deps cobj) defined in
              let todo = Mod.Ref.Set.union todo new_refs in
              find_mod_refs r ~deps ~ext cobjs defined todo

let find_archives_and_deps ?(deps = Cobj.link_deps) r ~code ~dirs =
  let ext = match code with `Byte -> ".cma" | `Native -> ".cmxa" in
  let* roots = Fut.of_list (List.map (get_cobjs_info r ~ext) dirs) in
  let roots = List.concat roots in
  let defined, to_find =
    let rec loop defs ldeps = function
    | [] -> defs, Mod.Ref.Set.diff ldeps defs
    | cobj :: cobjs ->
      let defs = Mod.Ref.Set.union (Cobj.defs cobj) defs in
      let ldeps = Mod.Ref.Set.union (deps cobj) ldeps in
      loop defs ldeps cobjs
    in
    loop Mod.Ref.Set.empty Mod.Ref.Set.empty roots
  in
  find_mod_refs r ~deps ~ext (Cobj.Set.of_list roots) defined to_find

(*---------------------------------------------------------------------------
   Copyright (c) 2019 The b0 programmers

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

(*---------------------------------------------------------------------------
   Copyright (c) 2019 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open B00_std
open B00

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
    mutable dir_cobjs :
      B00_ocaml.Cobj.t list Memo.Fut.t Fpath.Map.t; (* Mapped by dir. *)
    mutable mod_ref_cobj : B00_ocaml.Cobj.t list B00_ocaml.Mod_ref.Map.t; }

let create m ~memo_dir ~ocamlpath =
  { m; memo_dir; ocamlpath;
    ocamlpath_root_dirs = ocamlpath_root_dirs ~ocamlpath;
    dir_dirs = Fpath.Map.empty;
    dir_cobjs = Fpath.Map.empty; mod_ref_cobj = B00_ocaml.Mod_ref.Map.empty }

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
    let info, set_info = Memo.Fut.create r.m in
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
      B00_ocaml.Cobj.write r.m ~cobjs ~o;
      B00_ocaml.Cobj.read r.m o @@ fun cobjs ->
      let add_mod_ref cobj def =
        r.mod_ref_cobj <-
          B00_ocaml.Mod_ref.Map.add_to_list def cobj r.mod_ref_cobj
      in
      let add_mod_refs cobj =
        B00_ocaml.Mod_ref.Set.iter (add_mod_ref cobj) (B00_ocaml.Cobj.defs cobj)
      in
      List.iter add_mod_refs cobjs;
      set_info (Some cobjs)
    end;
    info

let try_find_mod_ref_root_dir r ref =
  let name = String.Ascii.lowercase (B00_ocaml.Mod_ref.name ref) in
  match String.Map.find name r.ocamlpath_root_dirs with
  | p -> Some p
  | exception Not_found ->
      match String.cut_left ~sep:"_" name with
      | None -> None
      | Some (l, _) ->
          match String.Map.find name r.ocamlpath_root_dirs with
          | p -> Some p
          | exception Not_found -> None

let amb r ~ext ref cobjs k =
  let pext = ".p" ^ ext in (* TODO doc filter out profile objects *)
  let not_pext cobj = not (Fpath.has_ext pext (B00_ocaml.Cobj.file cobj)) in
  match List.filter not_pext cobjs with
  | [cobj] ->
      k (Some cobj)
  | cobjs ->
      (* FIXME constraints. *)
      Memo.notify r.m `Info "@[<v>ambiguous resolution for %a:@,%a@]"
        B00_ocaml.Mod_ref.pp ref (Fmt.list B00_ocaml.Cobj.pp) cobjs;
      k None

let find_archive_for_mod_ref r ~ext ref k =
  match B00_ocaml.Mod_ref.Map.find ref r.mod_ref_cobj with
  | [cobj] -> k (Some cobj)
  | cobjs -> amb r ~ext ref cobjs k
  | exception Not_found ->
      match try_find_mod_ref_root_dir r ref with
      | None -> k None
      | Some roots ->
          let root = List.hd roots (* FIXME *) in
          Memo.Fut.await (get_cobjs_info r ~ext root) @@ fun _ ->
          match B00_ocaml.Mod_ref.Map.find ref r.mod_ref_cobj with
          | [cobj] -> k (Some cobj)
          | cobjs -> amb r ~ext ref cobjs k
          | exception Not_found ->
              k None (* TODO subdirs and eventually whole scan *)

let rec find_mod_refs r ~deps ~ext cobjs defined todo k =
  match B00_ocaml.Mod_ref.Set.choose todo with
  | exception Not_found ->
      let cobjs = B00_ocaml.Cobj.Set.elements cobjs in
      let cobjs, _ = B00_ocaml.Cobj.sort ~deps cobjs in
      k cobjs
  | ref ->
      let todo = B00_ocaml.Mod_ref.Set.remove ref todo in
      match B00_ocaml.Mod_ref.Set.mem ref defined with
      | true -> find_mod_refs r ~deps ~ext cobjs defined todo k
      | false ->
          find_archive_for_mod_ref r ~ext ref @@ function
          | None ->
              Memo.notify r.m `Info
                "No resolution for %a" B00_ocaml.Mod_ref.pp ref;
              find_mod_refs r ~deps ~ext cobjs defined todo k
          | Some cobj ->
              let cobjs = B00_ocaml.Cobj.Set.add cobj cobjs in
              let defined =
                B00_ocaml.Mod_ref.Set.union (B00_ocaml.Cobj.defs cobj) defined
              in
              let new_refs =
                B00_ocaml.Mod_ref.Set.diff (deps cobj) defined
              in
              let todo = B00_ocaml.Mod_ref.Set.union todo new_refs in
              find_mod_refs r ~deps ~ext cobjs defined todo k

let find_archives_and_deps ?(deps = B00_ocaml.Cobj.link_deps) r ~code ~dirs k =
  let ext = match code with
  | B00_ocaml.Cobj.Byte -> ".cma"
  | B00_ocaml.Cobj.Native -> ".cmxa"
  in
  let get_root_cobjs ~ext dirs k =
    let rec loop acc = function
    | [] -> k acc
    | cobjs :: cobjss ->
        Memo.Fut.await cobjs @@ fun cobjs ->
        loop (List.rev_append acc cobjs) cobjss
    in
    loop [] (List.map (get_cobjs_info r ~ext) dirs)
  in
  get_root_cobjs ~ext dirs @@ fun roots ->
  let defined, to_find =
    let rec loop defs ldeps = function
    | [] -> defs, B00_ocaml.Mod_ref.Set.diff ldeps defs
    | cobj :: cobjs ->
      let defs = B00_ocaml.Mod_ref.Set.union (B00_ocaml.Cobj.defs cobj) defs in
      let ldeps = B00_ocaml.Mod_ref.Set.union (deps cobj) ldeps in
      loop defs ldeps cobjs
    in
    loop B00_ocaml.Mod_ref.Set.empty B00_ocaml.Mod_ref.Set.empty roots
  in
  find_mod_refs r ~deps ~ext
    (B00_ocaml.Cobj.Set.of_list roots) defined to_find k

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

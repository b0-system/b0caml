(*---------------------------------------------------------------------------
   Copyright (c) 2019 The b0caml programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open B0_std

(* Logical paths *)

let classify_path d =
  let s = Fpath.to_string d in
  if s.[0] <> '+' then `Concrete d else
  let s = String.subrange ~first:1 s in
  if String.is_empty s then `Logical (Fpath.v ".") else `Logical (Fpath.v s)

let logical_path_root_name dep =
  let s = Fpath.to_string dep in
  if s.[0] <> '+' then None else
  let last = match String.index s Fpath.dir_sep_char with
  | exception Not_found -> String.length s - 1 | i -> i - 1
  in
  match String.subrange ~first:1 ~last s with "" -> None | s -> Some s

(* Lookup *)

type t =
  { dirs : Fpath.t list;
    ocaml_logical_dir : Fpath.t; }

let find dirs name =
  let exists name dir =
    Log.if_error ~use:false @@ Os.Dir.exists Fpath.(dir / name)
  in
  match List.find (exists name) dirs with
  | exception Not_found -> None
  | dir -> Some dir

let of_paths ps = match find ps "ocaml" with
| Some ocaml -> Ok { dirs = ps; ocaml_logical_dir = ocaml }
| None ->
    let pp = Fmt.st [`Bold] in
    Fmt.error "Could not find %a in %a" pp "+ocaml" pp "OCAMLPATH"

let get ?search = function
| Some ps -> of_paths ps
| None ->
    let var = "OCAMLPATH" and empty_is_none = false in
    let path = Os.Env.find' ~empty_is_none Fpath.list_of_search_path var in
    match Log.if_error ~use:None path with
    | Some ps -> of_paths ps
    | None ->
        let fpath_of_cmd cmd = match Os.Cmd.find ?search cmd with
        | None -> Ok None
        | Some cmd ->
            Result.bind (Os.Cmd.run_out ~trim:true cmd) @@ fun s ->
            Result.map Option.some (Fpath.of_string s)
        in
        let opam_lib = fpath_of_cmd Cmd.(arg "opam" % "var" % "lib") in
        let ocaml_where = fpath_of_cmd Cmd.(arg "ocamlc" % "-where") in
        Result.bind opam_lib @@ fun opam_lib ->
        Result.bind ocaml_where @@ fun ocaml_where ->
        match opam_lib, ocaml_where with
        | None, Some p -> Ok { dirs = [p]; ocaml_logical_dir = p }
        | Some p, None ->
            Ok { dirs = [p]; ocaml_logical_dir = Fpath.(p / "ocaml") }
        | None, None ->
            let pp = Fmt.st [`Bold] in
            Fmt.error "@[<v>Could not detect an OCaml install.@,\
                            Try setting the %a variable.@]" pp "OCAMLPATH"
        | Some opam, Some ocaml when Fpath.is_prefix opam ocaml ->
            Ok { dirs = [opam]; ocaml_logical_dir = ocaml }
        | Some opam, Some ocaml ->
            Ok { dirs = [opam; ocaml]; ocaml_logical_dir = ocaml }

let dirs p = p.dirs
let ocaml_logical_dir p = p.ocaml_logical_dir

let logical_dirs p =
  let add_dir acc dir =
    let add_path _ _ p ds = Fpath.Set.add Fpath.(v ("+" ^ to_string p)) ds in
    Result.error_to_failure @@
    Os.Dir.fold_dirs ~rel:true ~recurse:true add_path dir acc
  in
  try Ok (List.fold_left add_dir Fpath.Set.empty p.dirs) with
  | Failure e -> Error e

let logical_dir_suggestions ~logical_dirs:dirs dir =
  let dirs = Fpath.Set.fold (fun p acc -> Fpath.to_string p :: acc) dirs [] in
  let dir = Fpath.to_string dir in
  let dir_root = match String.cut_left ~sep:Fpath.dir_sep dir with
  | None -> dir | Some (root, _) -> root
  in
  let some ds = Some (List.map Fpath.v ds) in
  let ds = String.suggest dirs dir in
  if ds <> [] then some ds else
  let ds = match List.mem dir_root dirs with
  | true -> List.filter (fun f -> String.starts_with ~prefix:dir_root f) dirs
  | false -> String.suggest dirs dir
  in
  if ds <> [] then some ds else None

let pp_did_you_mean_logical_dirs ppf dirs =
  Fmt.pf ppf "@,@[%a@]" (Fmt.did_you_mean (Fmt.code' Fpath.pp_unquoted)) dirs

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

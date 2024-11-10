(*---------------------------------------------------------------------------
   Copyright (c) 2019 The b0caml programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std

(* Listing packages *)

let pkg_set_of_lines s =
  let add_pkg _ set pkg = if pkg <> "" then String.Set.add pkg set else set in
  String.fold_ascii_lines ~strip_newlines:true add_pkg String.Set.empty s

let uninstalled ?search ?switch () =
  let switch = match switch with
  | None -> Cmd.empty | Some s -> Cmd.(arg "--switch" % s)
  in
  let opam_list = Cmd.(tool "opam" % "list" %% switch % "--short") in
  match Os.Cmd.find ?search opam_list with
  | None -> Ok String.Set.empty
  | Some opam_list ->
      let list kind = Os.Cmd.run_out ~trim:true Cmd.(opam_list % kind) in
      Result.bind (list "--available") @@ fun available ->
      Result.bind (list "--installed") @@ fun installed ->
      let available = pkg_set_of_lines available in
      let installed = pkg_set_of_lines installed in
      Ok (String.Set.diff available installed)

(* Suggesting packages *)

let pkg_suggestions ~pkgs ~pkg =
  if String.Set.mem pkg pkgs then Some (`Exact pkg) else
  match String.suggest (String.Set.elements pkgs) pkg with
  | [] -> None | ss -> Some (`Fuzzy ss)

let pp_maybe_try_install ~alt ppf opam =
  let pp_cmd ppf l = Fmt.st [`Bold] ppf (String.concat " " l) in
  let maybe = if alt then "Or maybe try" else "Maybe try" in
  let pp_install ppf pkgs = pp_cmd ppf ("opam" :: "install" :: pkgs) in
  match opam with
  | `Exact pkg -> Fmt.pf ppf "@,@[%s %a@]" maybe pp_install [pkg]
  | `Fuzzy pkgs ->
    let alts = Fmt.or_enum (Fmt.st [`Bold]) in
    Fmt.pf ppf "@,@[%s %a with %a@]" maybe pp_install [] alts pkgs

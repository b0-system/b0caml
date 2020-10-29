(*---------------------------------------------------------------------------
   Copyright (c) 2019 The b0caml programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open B00_std

(* Listing packages *)

let uninstalled ?search ?switch () =
  let switch = match switch with
  | None -> Cmd.empty | Some s -> Cmd.(atom "--switch" % s)
  in
  let opam_list = Cmd.(atom "opam" % "list" %% switch % "--short") in
  Result.bind (Os.Cmd.find ?search opam_list) @@ function
  | None -> Ok String.Set.empty
  | Some opam_list ->
      let list kind = Os.Cmd.run_out ~trim:true Cmd.(opam_list % kind) in
      Result.bind (list "--available") @@ fun available ->
      Result.bind (list "--installed") @@ fun installed ->
      let available = B00_lines.of_string (String.trim available) in
      let installed = B00_lines.of_string (String.trim installed) in
      Ok (String.Set.(diff (of_list available) (of_list installed)))

(* Suggesting packages *)

let pkg_suggestions ~pkgs ~pkg =
  if String.Set.mem pkg pkgs then Some (`Exact pkg) else
  match String.suggest (String.Set.elements pkgs) pkg with
  | [] -> None | ss -> Some (`Fuzzy ss)

let pp_maybe_try_install ~alt ppf opam =
  let pp_cmd ppf l = Fmt.tty_string [`Bold] ppf (String.concat " " l) in
  let maybe = if alt then "Or maybe try" else "Maybe try" in
  let pp_install ppf pkgs = pp_cmd ppf ("opam" :: "install" :: pkgs) in
  match opam with
  | `Exact pkg -> Fmt.pf ppf "@,@[%s %a@]" maybe pp_install [pkg]
  | `Fuzzy pkgs ->
    let alts = Fmt.or_enum (Fmt.tty [`Bold] Fmt.string) in
    Fmt.pf ppf "@,@[%s %a with %a@]" maybe pp_install [] alts pkgs

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

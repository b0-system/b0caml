(*---------------------------------------------------------------------------
   Copyright (c) 2019 The b0caml programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** [OCAMLPATH] support move to B0_care. *)

open B0_std

(** {1:logical Logical paths} *)

val classify_path : Fpath.t -> [ `Concrete of Fpath.t | `Logical of Fpath.t ]
(** [classify_path p] is:
    {ul
    {- [`Logical l] if [p] starts with a ['+']. [l] is [p] without
       the ['+'] or [Fpath.v "."] if that results in the empty string.}
    {- [`Concrete d] otherwise.}}
       Logical paths are those that need to be looked up in [OCAMLPATH]. *)

val logical_path_root_name : Fpath.t -> string option
(** [logical_path_root_name d] is:
    {ul
    {- [Some r] if [classify_path d] is [`Logical d'] and [r]
         the first non-dot segment of [d'].}
    {- [None] otherwise.}} *)

(** {1:lookup OCAMLPATH lookup} *)

type t
(** The type for OCAMLPATH. Conceptually it's just a list of directories
    but for legacy reasons we need to carry a bit more information. *)

val get :
  ?search:Cmd.tool_search -> Fpath.t list option -> (t, string) result
(** [get ocamlpath] is [Ok ps] if [ocamlpath] is [Some ps] and otherwise:
    {ul
    {- If the [OCAMLPATH] environment variable is defined, its contents
       parsed according to {!Fpath.list_of_search_path}.}
    {- If the [opam] tool is available [[$(opam var lib); $(ocamlc -where)]]
       or [[$(opam var lib)]] if [$(ocamlc -where)] is included in it.}
    {- If the [opam] tool is not available [$(ocamlc -where)]}}
       [search] is given to {!Os.Cmd.find} to lookup [ocamlc] and [opam]. *)

val dirs : t -> Fpath.t list
(** [dirs] are the directories in the OCAMLPATH. *)

val ocaml_logical_dir : t -> Fpath.t
(** [ocaml_logical] is the path to the directory that should be called
    ["+ocaml"] in the OCAMLPATH. For systems installs where packages
    are installed in [ocamlc -where] (OCAMLPATH is undefined), [ocamlc
    -where] is in [dirs] but ["+ocaml"] cannot be resolved by looking
    up {!dirs}. This is the resolution that should be used for
    ["+ocaml"]. *)

val logical_dirs : t -> (Fpath.Set.t, string) result
(** [logical_dirs] is the domain of logical directories in [ocamlpath] on the
    current file system. That is the set of directories [DIR] that can
    be specified as [+DIR]. The set has them without the [+]. *)

(** {1:suggest Logical suggestions} *)

val logical_dir_suggestions :
  logical_dirs:Fpath.Set.t -> Fpath.t -> Fpath.t list option
  (** [dir_suggestions ~dirs dir] are suggestions to correct
      an unfound logical directory [dir] in [logical_dirs] for
      example obtained via {!logical_dirs}. *)

val pp_did_you_mean_logical_dirs : Fpath.t list Fmt.t
(** [pp_did_you_mean_logical_dirs] suggests a logical directory
    spell check. Formats a starting {!Fmt.cut} followed
    by a boxed sentence of the form ["Did you mean ... ?"]. *)

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

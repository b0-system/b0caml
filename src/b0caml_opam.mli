(*---------------------------------------------------------------------------
   Copyright (c) 2019 The b0caml programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** [opam] support, move to [B00_kit]. *)

open B0_std

(** {1:pkg_list Listing packages} *)

val uninstalled :
  ?search:Cmd.tool_search -> ?switch:string -> unit ->
  (String.Set.t, string) result
(** [uninstalled ?search ~switch ()] is the set of uninstalled
    packages in the opam switch [switch]. [search] is given to
    {!Os.Cmd.find} to lookup [opam]. The empty set is returned
    if [opam] can't be looked up. *)

(** {1:pkg_suggest Package suggestions} *)

val pkg_suggestions : pkgs:String.Set.t -> pkg:string ->
  [ `Exact of string | `Fuzzy of string list ] option
(** [pkg_suggestions ~pkgs ~pkg] is [Some (`Exact pkg)] if [pkg] is in
    [pkgs] or [Some (`Fuzzy pkgs)] if [pkgs] matches according to
    {!String.suggest} or [None] otherwise. *)

val pp_maybe_try_install :
  alt:bool -> [`Exact of string | `Fuzzy of string list ] Fmt.t
(** [pp_maybe_try_install ~alt] entices to install the given
    package. Formats a starting {!Fmt.cut} by a sentence
    of the form ["Maybe try opam install ..."]. If [alt]
    is [true] then the sentence starts with ["Or maybe try"]. *)

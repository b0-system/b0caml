(*---------------------------------------------------------------------------
   Copyright (c) 2019 The b0caml programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** [opam] support, move to [B00_kit]. *)

open B0_std

(** {1:pkg_list Listing packages} *)

val uninstalled :
  ?search:Fpath.t list -> ?switch:string -> unit ->
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

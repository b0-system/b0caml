(*---------------------------------------------------------------------------
   Copyright (c) 2019 The b0caml programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** OCaml scripts *)

open B0_std
open B00_serialk_text

(** {1:meta Syntactic metadata} *)

type smeta
(** Metadata attached to syntactic constructs. *)

val loc : smeta -> Tloc.t
(** [loc i] is the text source location of [i]. *)

val loc_errf : smeta ->
  ('a, Format.formatter, unit, string) format4 -> 'a
(** [loc_errf smeta fmt] formats an error for the location in smeta. *)

(** {1:scripts Scripts} *)

type t
(** The type for scripts. *)

val of_string : file:Fpath.t -> string -> (t, string) result
(** [of_string ?file s] parses a b0caml script from [s]. [file] is
    the file used for locations, it must be an absolute file path. *)

val file : t -> Fpath.t
(** [file s] is the script's file. *)

val shebang : t -> (string * smeta) option
(** [shebang s] is the optional shebang line without the shebang
    and the new line. *)

val directories : t -> (Fpath.t * smeta) list
(** [directories s] are the script's [#directory] directives. The location
    spans the directive's argument. Relative file paths or [+] are not
    resolved. See {!directory_resolution}. *)

val mod_uses : t -> (Fpath.t * smeta) list
(** [mod_uses s] are the script's [#mod_use] directives. The location
    spans the directive's argument. Relative file paths are not
    resolved. See {!mod_use_resolution}. *)

val ocaml_unit : t -> string * smeta
(** [ocaml_unit s] is the script's OCaml implementation unit. *)

val pp_dump : t Fmt.t
(** [pp_dump] dumps the parsed script. *)

val pp_locs : t Fmt.t
(** [pp_locs] dumps the source text locations of [s]. *)

(** {1:directory_resolution [#directory] resolution} *)

type directory_resolution = Fpath.t * smeta
(** The type for [#directory] resolution results. An absolute path to
    a existing directory and the directive where it originates
    from. *)

type directory_resolution_error = Fpath.t * smeta * [`Error of string | `Miss ]
(** The type for [#directory] resolution error. An absolute or logical
    path that failed to resolve, the directive where it originates
    from and either a file system error or a missing error. *)

val directory_resolution_dir : directory_resolution -> Fpath.t
(** [directory_resolution_dir res] is the directory mentioned in
    [res]. The file path is syntactically a
    {{!Fpath.is_dir_path}directory path}. *)

val resolve_directories :
  ocamlpath:B0caml_ocamlpath.t -> t ->
  (directory_resolution list, directory_resolution_error list) result
(** [resolve_directories ~ocamlpath s] resolves the [#directory]
    directives of [s] according to [ocamlpath].  See the types for a
    description of the result. *)

(** {1:mod_use_resolution [#mod_use] resolution} *)

type mod_use_resolution = Fpath.t option * Fpath.t * smeta
(** The type for [#mod_use] resolution results. An absolute path to an
    optional existing interface file, an absolute path to an existing
    implementation file and the directive where it originates from. *)

type mod_use_resolution_error = Fpath.t * smeta * [`Error of string | `Miss ]
(** The type for [#mod_use] resolution errors. An absolute paths to an
    a resolved file and either a file system error or a missing file. *)

val mod_use_resolution_files : mod_use_resolution -> Fpath.t list
(** [mod_use_resolution_files res] are the files mentioned in [res]. *)

val resolve_mod_uses :
  t -> (mod_use_resolution list, mod_use_resolution_error list) result
(** [resolve_mod_uses s] resolves the [#mod_use] directives of [s]. See the
    types for a description of the result. *)

(** {1:src Script source} *)

val src :
  mod_use_resolutions:mod_use_resolution list -> t -> (string, string) result
(** [src ~mod_use_resolutions s] uses [mod_use_resolutions] (see
    {!resolve_mod_uses}) to produce the final script source of [s]. *)

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

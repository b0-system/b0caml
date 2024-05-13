(*---------------------------------------------------------------------------
   Copyright (c) 2019 The b0caml programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** [b0caml] support. *)

open B0_std

(** [b0caml] exit codes. *)
module Exit : sig

  type t =
  | Code of int
  | Exec of Fpath.t * Cmd.t (** *)
  (** The type for exits. Either an exit code or a command to [execv]. *)

  val code : t -> int
  (** [code c] is the code of [c] raises [Invalid_argument] if [c]
      is [Exec]. *)

  val conf_error : t
  (** [conf_error] is for configuration errors. *)

  val comp_error : t
  (** [comp_error] is for compilation errors. More specifically
      this is [127] (what shells use when the command
      is not found in path). *)

  val miss_dep_error : t
  (** [miss_dep_error] is used by the [deps] subcommand to report
      missing [#mod_use] or [#directory] paths. *)

  val miss_log_error : t
  (** [miss_log_error] is used by the [log] subcommand to report a
      missing lot. *)

  val ok : t
  (** [ok] is the zero exit code. *)

  val some_error : t
  (** [some_error] is used to indicate an indiscriminate error
      happened and was reported on stdout. *)
end

(** [b0caml] environment variables. *)
module Env : sig
  val cache_dir : string
  (** [cache_dir] is the variable used to sepcify the cache directory. *)

  val color : string
  (** [color] is the variable used to specify tty styling. *)

  val comp_target : string
  (** [comp_target] is the variable used to specify the compilation target. *)

  val verbosity : string
  (** [verbosity] is the variable used to specify log verbosity. *)
end

(** [b0caml] configuration. *)
module Conf : sig

  type comp_target = [ `Auto | `Byte |  `Native ]
  (** The type for compilation targets. *)

  val comp_target_of_string : string -> (comp_target, string) result
  (** [comp_target_of_string s] parses a compilation target from [s]. *)

  (** {1:conf Configurations} *)

  type t
  (** The type for configurations. *)

  val v :
    cache_dir:Fpath.t -> comp_target:comp_target -> cwd:Fpath.t ->
    log_level:Log.level -> ocamlpath:B0caml_ocamlpath.t ->
    fmt_styler:Fmt.styler -> unit -> t
  (** [v] constructs a configuration with the given attributes.
      See the corresponding accessors for details. *)

  val cache_dir : t -> Fpath.t
  (** [cache_dir c] is the cache directory. *)

  val b0_cache_dir : t -> Fpath.t
  (** [b0_cache_dir c] is the b0 cache directory. *)

  val comp_target : t -> comp_target
  (** [comp_target c] is the target to which scripts are compiled. *)

  val cwd : t -> Fpath.t
  (** [cwd c] is the current working directory w.r.t. relative
      configuration file paths are expressed. *)

  val log_level : t -> Log.level
  (** [log_level c] is the desired log level. *)

  val ocamlpath : t -> B0caml_ocamlpath.t
  (** [ocamlpath] is the [OCAMLPATH] to consider. *)

  val memo : t -> (B0_memo.t, string) result
  (** [memo c] is the memoizer for the configuration. *)

  val fmt_styler : t -> Fmt.styler
  (** [fmt_styler c] is formatting styler assumed for output. *)

  (** {1:setup Setup} *)

  val setup_with_cli :
    cache_dir:Fpath.t option -> comp_target:comp_target option ->
    log_level:Log.level option -> color:Fmt.styler option option -> unit ->
    (t, string) result
  (** [setup_with_cli ~cache_dir ~comp_target ~color ~log_level ()]
      determines and setups a configuration with the given values. These are
      expected to have been determined by environment variables and command
      line arguments. *)

  val setup_without_cli : unit -> (t, string) result
  (** [setup_without_cli] determines and setups a configuration
      without without command line arguments. This looks up
      environment variables and determines defaults. *)
end

(** [b0caml] error messages. *)
module Err : sig
  val directories :
    ocamlpath:B0caml_ocamlpath.t ->
    B0caml_script.directory_resolution_error list -> string
  (** [directories] is an error message for [#directory] resolution errors. *)

  val mod_uses :
    B0caml_script.mod_use_resolution_error list -> string
  (** [mod_uses] is an error message for [#mod_use] resolution errors. *)
end


val get_script_file : Conf.t -> string -> (Fpath.t, string) result
(** [get_script_file c file] is the absolute paths to script [file]. *)

val script_build_dir : Conf.t -> script_file:Fpath.t -> Fpath.t
(** [script_build_dir c ~script_file] is a build directory in the cache
    directory of [c] for script [~script_file].*)

val script_build_log : build_dir:Fpath.t -> Fpath.t
(** [script_build_log ~build_dir] is a build log file in [build_dir]. *)

val get_script : Conf.t -> string -> (B0caml_script.t, string) result
(** [get_script c file] parses a script from file [file] in configuration
    [c]. *)

val get_source : Conf.t -> B0caml_script.t -> (string, string) result
(** [get_source c s] is the final source of script [s] in
    configuration [c], determined via {!B0caml_script.src}. *)

val compile_script :
  Conf.t -> B0caml_script.t -> (Fpath.t, string) result
(** [compile_script c s] compiles script [s] to an executable. *)

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

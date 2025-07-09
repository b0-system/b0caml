(*---------------------------------------------------------------------------
   Copyright (c) 2019 The b0caml programmers. All rights reserved.
   SPDX-License-Identifier: ISC
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

  val comp_target : string
  (** [comp_target] is the variable used to specify the compilation target. *)
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

  val make :
    cache_dir:Fpath.t -> comp_target:comp_target -> cwd:Fpath.t ->
    ocamlpath:B0caml_ocamlpath.t -> unit -> t
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

  val ocamlpath : t -> B0caml_ocamlpath.t
  (** [ocamlpath] is the [OCAMLPATH] to consider. *)

  val memo : t -> Fpath.t -> (B0_memo.t, string) result
  (** [memo c script] is the memoizer for the configuration and script
      [script]. *)

  (** {1:setup Setup} *)

  val of_cli : unit -> (t, string) result Cmdliner.Term.t
  (** [of_cli] is a configuration command line interaface. This
      setups logging by side-effect. *)

  val of_env : unit -> (t, string) result
  (** [of ()] is an configuration read from the environments. This
      setups logging by side-effect. *)
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

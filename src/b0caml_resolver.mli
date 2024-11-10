(*---------------------------------------------------------------------------
   Copyright (c) 2019 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Library resolver *)

open B0_std

type t
(** The type for library resolvers. *)

val create :
  B0_memo.t -> memo_dir:Fpath.t -> ocamlpath:B0caml_ocamlpath.t -> t

val ocamlpath : t ->  B0caml_ocamlpath.t
val find_archives_and_deps :
  ?deps:(B0_ocaml.Cobj.t -> B0_ocaml.Modref.Set.t) -> t ->
  code:B0_ocaml.Code.t -> dirs:B0_std.Fpath.t list ->
  B0_ocaml.Cobj.t list Fut.t

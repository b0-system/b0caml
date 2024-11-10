(*---------------------------------------------------------------------------
   Copyright (c) 2019 The b0caml programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** [b0caml] cmdliner command. *)

val cmd : unit -> B0caml.Exit.t Cmdliner.Cmd.t
(** [cmdliner] command for [b0caml]. *)

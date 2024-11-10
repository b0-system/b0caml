#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let () =
  Pkg.describe "b0caml" @@ fun c ->
  Ok [ Pkg.mllib "src/b0caml.mllib";
       Pkg.bin "src/tool/b0caml_main" ~dst:"b0caml";
       Pkg.doc "doc/index.mld" ~dst:"odoc-pages/index.mld";
       Pkg.doc "doc/manual.mld" ~dst:"odoc-pages/manual.mld"; ]

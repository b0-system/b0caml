open B0_kit.V000
open B00_std

(* OCaml library names *)

let cmdliner = B0_ocaml.libname "cmdliner"
let b0_b00_std = B0_ocaml.libname "b0.b00.std"
let b0_b00 = B0_ocaml.libname "b0.b00"
let b0_b00_kit = B0_ocaml.libname "b0.b00.kit"
let b0caml = B0_ocaml.libname "b0caml"

(* Units *)

let b0caml_lib =
  let requires = [cmdliner; b0_b00_std; b0_b00; b0_b00_kit] in
  let srcs = Fpath.[`Dir (v "src"); `X (v "src/b0caml_main.ml");] in
  let name = "lib" in
  B0_ocaml.lib b0caml ~name ~doc:"B0caml support library" ~requires ~srcs

let b0caml =
  let requires = [cmdliner; b0_b00_std; b0_b00; b0_b00_kit; b0caml] in
  let srcs = Fpath.[`File (v "src/b0caml_main.ml")] in
  B0_ocaml.exe "b0caml" ~doc:"b0caml tool" ~requires ~srcs

(* Packs *)

let default =
  let units = B0_unit.list () in
  let meta =
    let open B0_meta in
    empty
    |> add authors ["The b0caml programmers"]
    |> add maintainers ["Daniel Bünzli <daniel.buenzl i@erratique.ch>"]
    |> add homepage "https://erratique.ch/software/b0caml"
    |> add online_doc "https://erratique.ch/software/b0caml/doc"
    |> add description_tags
      ["org:erratique"; "org:b0-system"; "build"; "dev"; "scripting"]
    |> add licenses ["ISC"; "PT-Sans-fonts"; "DejaVu-fonts"]
    |> add repo "git+https://erratique.ch/repos/b0caml.git"
    |> add issues "https://github.com/b0-system/b0caml/issues"
  in
  B0_pack.v "default" ~doc:"The b0caml project" ~meta ~locked:true units

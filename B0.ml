open B0_kit.V000

(* OCaml library names *)

let unix = B0_ocaml.libname "unix"
let cmdliner = B0_ocaml.libname "cmdliner"
let b0_std = B0_ocaml.libname "b0.std"
let b0_memo = B0_ocaml.libname "b0.memo"
let b0_file = B0_ocaml.libname "b0.file"
let b0_kit = B0_ocaml.libname "b0.kit"
let b0caml = B0_ocaml.libname "b0caml"

(* Units *)

let b0caml_lib =
  let requires = [unix; cmdliner; b0_std; b0_memo; b0_file; b0_kit] in
  let srcs = Fpath.[`Dir (v "src"); `X (v "src/b0caml_main.ml");] in
  let name = "lib" in
  B0_ocaml.lib b0caml ~name ~doc:"B0caml support library" ~requires ~srcs

let b0caml =
  let requires = [cmdliner; b0_std; b0_memo; b0_file; b0_kit; b0caml] in
  let srcs = Fpath.[`File (v "src/b0caml_main.ml")] in
  B0_ocaml.exe "b0caml" ~public:true ~doc:"b0caml tool" ~requires ~srcs

(* Packs *)

let default =
  let meta =
    B0_meta.empty
    |> B0_meta.(add authors) ["The b0caml programmers"]
    |> B0_meta.(add maintainers)
       ["Daniel Bünzli <daniel.buenzl i@erratique.ch>"]
    |> B0_meta.(add homepage) "https://erratique.ch/software/b0caml"
    |> B0_meta.(add online_doc) "https://erratique.ch/software/b0caml/doc"
    |> B0_meta.(add description_tags)
      ["org:erratique"; "org:b0-system"; "build"; "dev"; "scripting"]
    |> B0_meta.(add licenses) ["ISC"; "PT-Sans-fonts"; "DejaVu-fonts"]
    |> B0_meta.(add repo) "git+https://erratique.ch/repos/b0caml.git"
    |> B0_meta.(add issues) "https://github.com/b0-system/b0caml/issues"
  in
  B0_pack.make "default" ~doc:"The b0caml project" ~meta ~locked:true @@
  B0_unit.list ()

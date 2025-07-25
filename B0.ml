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
  let srcs = [`Dir ~/"src"] in
  B0_ocaml.lib b0caml ~name:"b0caml-lib" ~requires ~srcs

let b0caml =
  let requires = [cmdliner; b0_std; b0_memo; b0_file; b0_kit; b0caml] in
  let srcs = [`Dir ~/"src/tool"] in
  B0_ocaml.exe "b0caml" ~public:true ~requires ~srcs

(* Packs *)

let default =
  let meta =
    B0_meta.empty
    |> ~~ B0_meta.authors ["The b0caml programmers"]
    |> ~~ B0_meta.maintainers ["Daniel Bünzli <daniel.buenzl i@erratique.ch>"]
    |> ~~ B0_meta.homepage "https://erratique.ch/software/b0caml"
    |> ~~ B0_meta.online_doc "https://erratique.ch/software/b0caml/doc"
    |> ~~ B0_meta.repo "git+https://erratique.ch/repos/b0caml.git"
    |> ~~ B0_meta.issues "https://github.com/b0-system/b0caml/issues"
    |> ~~ B0_meta.description_tags
      ["org:erratique"; "org:b0-system"; "build"; "dev"; "scripting"]
    |> ~~ B0_meta.licenses ["ISC"]
    |> ~~ B0_opam.build
      {|[["ocaml" "pkg/pkg.ml" "build" "--dev-pkg" "%{dev}%" ]]|}
    |> ~~ B0_opam.depends
      [ "ocaml", {|>= "4.14.0"|};
        "ocamlfind", {|build|};
        "ocamlbuild", {|build|};
        "topkg", {|build & >= "1.1.0"|};
        "cmdliner", {|>= "1.3.0"|};
        "b0", {||}; ]
    |> ~~ B0_opam.pin_depends
      [ "b0.dev", "git+https://erratique.ch/repos/b0.git#master"]
    |> B0_meta.tag B0_opam.tag
    |> B0_meta.tag B0_release.tag
  in
  B0_pack.make "default" ~doc:"b0caml package" ~meta ~locked:true @@
  B0_unit.list ()

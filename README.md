b0caml — Easy OCaml scripts
============================

B0caml runs OCaml scripts like `ocaml`. However it provides direct
support for using third-party modules and compiles scripts
transparently to a cache of native (or bytecode) executables.

B0caml is distributed under the ISC license and depends on [`b0`][b0]
and [`cmdliner`][cmdliner]. It is pronounced /bokamɛl/ with a thick
french accent.

Homepage: https://erratique.ch/software/b0caml  

[b0]: https://erratique.ch/software/b0
[cmdliner]: https://erratique.ch/software/cmdliner

## Installation

b0caml can be installed with `opam`:

    opam install b0caml

If you don't use `opam` consult the [`opam`](opam) file for build
instructions.

## Quick start 

A few invocations to get you started:

```shell
> cat > echo <<EOCAML
#!/usr/bin/env b0caml

let echo oc ss = output_string oc (String.concat " " ss ^ "\n")
let () = echo stdout (List.tl (Array.to_list Sys.argv))
EOCAML
> chmod +x ./echo
> ./echo Hello world
Hello world
```

For more information on how to use third-party modules see the
tutorial introduction of the [manual][doc] (also available via
`odig doc b0caml`).

## Documentation & support

The documentation can be consulted [online][doc] or via `odig doc
b0caml`.

Questions are welcome but better asked on the [OCaml forum][ocaml-forum] 
than on the issue tracker.

[doc]: https://erratique.ch/software/b0caml/doc
[ocaml-forum]: https://discuss.ocaml.org/

## Examples

A few examples can be found in the [`test`](test/) directory.


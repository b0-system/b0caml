# Building

    opam install topkg-care
    topkg build

Alternatively if you have a `brzo` in your path:

    brzo -b
    brzo --doc

# Running

    topkg run b0caml_bin
    brzo


Invoking `source dev-env` declares a `b0caml` function that uses a
`b0caml` built by one of the alternatives above (favouring a brzo
build if it can be found). 

Note however that this won't work for testing shebangs. You'll
have to install for that.





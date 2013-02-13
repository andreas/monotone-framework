This is a library for specifying monotone frameworks in OCaml, as described in [Principles of Program Analysis](http://www2.imm.dtu.dk/~hrni/PPA/ppa.html). An example using the framework is included in the `refract`-library, which uses monotone frameworks for static code analysis when compiling from one input language, REFRACT, to the source language of [PRISM](http://www.prismmodelchecker.org/).

## Running the example

Requirements:
* ocaml
* ocamlbuild
* menhir
* ocamlgraph
* facile

To build the example with ocamlbuild:

    ocamlbuild -use-menhir -use-ocamlfind -package ocamlgraph,facile refract/compiler.native

This will producce a file `compiler.native`. To run on an example program:

    ./compiler.native refract/programs/self_stabilization.refract

## Copyright

Copyright (c) 2012 Andreas Garnæs. See [LICENSE](LICENSE) for details.

#!/bin/sh
set -e


# export OCAMLPARAM='_,bin-annot=1'
# export OCAMLRUNPARAM=b

cd vendor/ocaml
./configure -prefix `pwd`  -no-ocamldoc -no-ocamlbuild -no-curses -no-graph  && make -j9 world.opt && make install  && cd ..

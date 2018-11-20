#!/bin/sh
set -e


# export OCAMLPARAM='_,bin-annot=1'
# export OCAMLRUNPARAM=b

cd vendor/ocaml
./configure -prefix `pwd` && make -j9 world.opt && make install  && cd ..

#!/bin/bash
set -ex

################################################################
# don't call this script directly, call linux-build.sh instead #
################################################################

VERSION=$(sed -nr 's/.*"version": "(.*)",/\1/p' /io/package.json)
echo Building version $VERSION

# install zip in order to create the zip package later
yum install -y zip git

# activate the Holy Build Box environment.
source /hbb/activate

# build ocaml
cd /io/vendor/ocaml
DIRNAME=`pwd`
/io/vendor/ocaml/configure -prefix `pwd` -cc "gcc -fPIC" -aspp "gcc -c -fPIC" -no-ocamldoc -no-ocamlbuild -no-curses -no-graph
make clean
make world.opt
make install
mkdir -p $DIRNAME/lib/ocaml/caml
make -C otherlibs/systhreads
cp otherlibs/systhreads/threads.h $DIRNAME/lib/ocaml/caml/threads.h

# copy ninja binary
cp /io/vendor/ninja/snapshot/ninja.linux /io/lib/ninja.exe

# build bsc/bsb
cd /io
PATH=/io/vendor/ocaml:$PATH make -C jscomp clean
PATH=/io/vendor/ocaml:$PATH make world-native
PATH=/io/vendor/ocaml:$PATH make install

# create zip package
rm -f bsb-native-linux-${VERSION}.zip
zip -r bsb-native-linux-${VERSION}.zip lib vendor/ocaml/bin/ocamlrun vendor/ocaml/ocamlc.opt vendor/ocaml/ocamlopt.opt vendor/ocaml/lib/ocaml -x lib/bsb -x lib/bsc -x lib/bsrefmt

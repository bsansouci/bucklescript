# Stop on any error
set -e

# Prevents warnings when running on older version than my mac
export MACOSX_DEPLOYMENT_TARGET="10.10"

# Make sure the global ocaml is up to date
# opam update
# opam switch reinstall 4.02.3+buckle-master
# eval `opam config env`

# Don't run ./scripts/buildocaml.sh because that turns off some features we actually want.
cd vendor/ocaml
./configure  -prefix `pwd`
make clean
make -j9 world.opt
make install
cd ../..

# Re-generate packed files for convenience.
cd jscomp
make common/bs_version.ml
make core/js_map.ml
make core/js_fold.ml
make force-snapshotml-native
make BS_NATIVE=true -C belt_byte
make BS_NATIVE=true -C belt_native
cd ..

# Get version number from package.json
VERSION=$(cat package.json | sed -n -E 's/.*"version": "(.*)",/\1/p')

# We need to run this because we need to make sure everything in bsb-native gets compiled with the
# vendored ocaml and not the global one. The reason is that the vendored one has some changes to be
# relocable that weren't published to the global one.
node scripts/install.js native

# Zip the whole thing.
rm -rf bsb-native-osx-$VERSION.zip && zip -r bsb-native-osx-$VERSION.zip  lib/ vendor/ocaml/ocamlc.opt vendor/ocaml/ocamlopt.opt vendor/ocaml/lib/ocaml vendor/ocaml/bin/ocamlrun -x lib/bsb -x lib/bsc -x lib/bsrefmt -x vendor/ocaml/lib/ocaml/ocamlbuild/**/* -x vendor/ocaml/lib/ocaml/ocamldoc/**/* -x lib/bsb.cm* -x lib/bsb_helper.cm* -x lib/bsb.o -x lib/bsc.o -x lib/bsb_helper.o

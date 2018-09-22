- make a PR to add "bs-super-errors" to config -> needed for ocamlfind support 
- rename `allowed_build_kinds` to `allowed_backend`
- we are killing matchenv, sorry boiz search for `@SorryMatchenv`
- change `build-script` name to something more like `prebuild` or something
- maybe change `build-script` to generate a ninja file
- remove `allowed-build-kinds` to `native-dependencies` / `bytecode-dependencies`
- make super errors tell people about Bigarray and all of others. Then we don't need to link them by default, making them smaller and hopefully people won't be annoyed. Add hint for `num` vs `nums`
- output name maybe not super useful? 
- `build_artifacts_dir` should overwrite `in-source`
- turn on -bin-annot by default in ocaml_flags
- ok the hierarchy is confusing as fuck
- Bsb_default_paths was an idea I had to figure out where the executables are. Because right now `Bsb_build_util.get_ocaml_dir` uses the location of `bsc.exe` as a reference point for where the stdlib is, but that won't be the right place in the esy world. Supporting multiple versions of ocaml means
    + not having super errors
    + not compiling / setting up bsc -> different installation process
- turn `-make-world` on by default
- fix `main_bs_super_errors` in `bsb_ninja_regen`
- BREAKING CHANGE: if you do `-clean` or `-clean-self`, we're deleting all artifacts now
- Check `@Esy` tasks
- Merge the windows changes (see https://github.com/bsansouci/bsb-native/commit/754c14a781604334d981f1245f071e42fce3ac29#diff-574597a1bb956e991501c0e9fe423e18)
- Generate uniform name https://github.com/bsansouci/bsb-native/issues/68
- Why are the line numbers wrong? Fix line number for belt ppx
- Bspp has wrong line numbers
- Make a PR to bucklescript's vendored ocaml to be slightly more relocatable (dll hack + bytecode interperter path)
- Have someone double check the hashing stuff inside hashset/hashmap because I'm no expert.


Double check we are up to date with Bob's bs.deriving stuff. Make sure have the line numbers all correct!








let backend = ref Bsb_config_types.Bytecode

let lib_artifacts_dir = ref Bsb_config.lib_bs

let lib_ocaml_dir = ref Bsb_config.lib_ocaml

let backend_string = ref Literals.bytecode

let (//) = Ext_path.combine

let set_backend b =
  backend := b;
  match b with
  | Bsb_config_types.Native   -> 
    lib_artifacts_dir := Bsb_config.lib_lit // "bs-native";
    lib_ocaml_dir := Bsb_config.lib_lit // "ocaml-native";
    backend_string := Literals.native;
  | Bsb_config_types.Bytecode -> 
    lib_artifacts_dir := Bsb_config.lib_lit // "bs-bytecode";
    lib_ocaml_dir := Bsb_config.lib_lit // "ocaml-bytecode";
    backend_string := Literals.bytecode;


let bsb_native_dir  = 
  Filename.dirname (Ext_path.normalize_absolute_path (Ext_path.combine (Sys.getcwd ())  Sys.executable_name))

let vendor_bsdep =     
  Filename.concat bsb_native_dir "bsb_helper.exe"

let ocaml_version = "4.06.1"

let ocaml_dir =
  Filename.(concat (concat (dirname bsb_native_dir) "native") ocaml_version)

let ocaml_lib_dir =
  Filename.(concat (concat ocaml_dir "lib") "ocaml")

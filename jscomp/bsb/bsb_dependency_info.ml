type path = string

type t = {
  mutable static_libraries: path list;
  mutable all_external_deps: path list;
  mutable all_c_linker_flags: string list;
  mutable all_otherlibs: string list;
}

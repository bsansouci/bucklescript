type path = string
(**
  Data structure used to track information about the project's dependencies.
  Used by the packing / linking step.
 *)
type t = {
  mutable static_libraries: path list;
  mutable all_external_deps: path list;
  mutable all_c_linker_flags: string list;
  mutable all_otherlibs: string list;
}


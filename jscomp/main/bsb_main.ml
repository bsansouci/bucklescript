let ( // ) = Ext_path.combine

let collect_dependency_info ~root_project_dir = 
  let dependency_info : Bsb_dependency_info.t = {
    static_libraries = [];
    all_external_deps = [];
    all_c_linker_flags = [];
    all_otherlibs = [];
  } in

  Bsb_build_util.walk_all_deps root_project_dir
    (fun {top; proj_dir} ->
      if not top then begin
        (* TODO: only read the one field we want to read *)
        let config = Bsb_config_parse.interpret_json 
          ~toplevel_package_specs:None
          ~per_proj_dir:proj_dir in
        (* TODO: double check this order *)
        let lib_artifacts_dir = proj_dir // !Bsb_global_backend.lib_artifacts_dir in
        dependency_info.static_libraries <- (List.map (fun lib -> lib_artifacts_dir // lib) config.static_libraries) @ dependency_info.static_libraries;
        dependency_info.all_c_linker_flags <- (Bsb_config_types.(config.c_linker_flags)) @ dependency_info.all_c_linker_flags;
        dependency_info.all_external_deps <- proj_dir // !Bsb_global_backend.lib_ocaml_dir :: dependency_info.all_external_deps;
        (* Dedup otherlibs *)
        (* TODO: check this order *)
        dependency_info.all_otherlibs <- (List.filter (fun lib1 -> (List.find_opt (fun lib2 -> lib2 = lib1) dependency_info.all_otherlibs) = None ) config.otherlibs) @ dependency_info.all_otherlibs;
      end;
  );
  dependency_info.all_external_deps <- List.rev dependency_info.all_external_deps;
  (* dependency_info.all_otherlibs <- List.filter (fun lib1 -> (List.find_opt (fun lib2 -> lib2 = lib1) dependency_info.all_otherlibs) = None ) dependency_info.all_otherlibs; *)
  dependency_info

let usage = "Usage : bsb.exe <bsb-options> -- <ninja_options>\n\
             For ninja options, try ninja -h \n\
             ninja will be loaded either by just running `bsb.exe' or `bsb.exe .. -- ..`\n\
             It is always recommended to run ninja via bsb.exe \n\
             Bsb options are:"

let handle_anonymous_arg arg =
  raise (Arg.Bad ("Unknown arg \"" ^ arg ^ "\""))

let per_proj_dir = ref None
let lib_artifacts_dir = ref None
let root_project_dir = ref None
let bsc_dir = ref None
let build_library = ref None

let bsb_main_flags : (string * Arg.spec * string) list = [
  "-bsc-dir", Arg.String (fun s ->
    bsc_dir := Some s
  ),
  "Internal";
  "-root-project-dir", Arg.String (fun s ->
    root_project_dir := Some s
  ),
  "Internal";
  "-lib-artifacts-dir", Arg.String (fun s ->
    lib_artifacts_dir := Some s
  ),
  "Internal";
  "-project-dir", Arg.String (fun s ->
    per_proj_dir := Some s
  ),
  "Internal";

  "-verbose", Arg.Unit Bsb_log.verbose,
  " Set the output(from bsb-native) to be verbose";

  "-backend", Arg.String (fun s -> 
      match s with
      | "native"   -> Bsb_global_backend.set_backend Bsb_config_types.Native
      | "bytecode" -> Bsb_global_backend.set_backend Bsb_config_types.Bytecode
      | _ -> failwith "-backend should be one of: 'js', 'bytecode' or 'native'."
    ),
  " Builds the entries in the bsconfig which match the given backend.";

  "-build-library", Arg.String (fun main_file -> build_library := Some(main_file)),
  " Builds a static library given a main module name. Outputs a cmxa/cma file depending on -backend.";

  "-w", Arg.Unit (fun () -> exit 0 ),
  " Watch mode";
]


let () =
  Arg.parse bsb_main_flags handle_anonymous_arg usage;

  let per_proj_dir = match !per_proj_dir with
    | None -> failwith "-project-dir was not set"
    | Some per_proj_dir -> per_proj_dir
  in
  let root_project_dir = match !root_project_dir with
    | None -> failwith "-root-project-dir was not set"
    | Some root_project_dir -> root_project_dir
  in
  let bsc_dir = match !bsc_dir with
    | None -> failwith "-bsc-dir was not set"
    | Some bsc_dir -> bsc_dir
  in
  let build_library = !build_library in

  let config = 
    Bsb_config_parse.interpret_json 
      ~toplevel_package_specs:None
      ~per_proj_dir in

  let toplevel = per_proj_dir = root_project_dir in       
  let dependency_info = if toplevel then collect_dependency_info ~root_project_dir
    else { Bsb_dependency_info.all_external_deps = []; static_libraries=[]; all_c_linker_flags=[]; all_otherlibs=[]; } in

  Bsb_merlin_gen.merlin_file_gen ~per_proj_dir ~bsc_dir config;       
  let ocaml_dir = Bsb_global_paths_native.ocaml_dir in
  Bsb_ninja_gen.output_ninja_and_namespace_map 
      ~per_proj_dir ~build_library  ~toplevel ~dependency_info ~ocaml_dir ~root_project_dir ~bsc_dir config ; 

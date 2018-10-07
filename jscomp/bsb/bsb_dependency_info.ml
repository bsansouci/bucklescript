#if BS_NATIVE then
type path = string

type t = {
  mutable all_external_deps: path list;
  mutable all_ocamlfind_dependencies: string list;
  mutable all_ocaml_dependencies: Depend.StringSet.t;
  mutable all_clibs: path list;
  mutable all_c_linker_flags: string list;
  mutable all_toplevel_ppxes: (Bsb_config_types.entries_t list) String_map.t;
}

let (//) = Ext_path.combine

let get_exec_name entry =
  match entry with
  | {Bsb_config_types. backend = [NativeTarget]; output_name = None; main_module_name} -> 
    if Ext_sys.is_windows_or_cygwin then (String.lowercase main_module_name) ^ ".native.exe" 
                                    else (String.lowercase main_module_name) ^ ".native"
  | {backend = [NativeTarget]; output_name = Some(name); } -> name
  | {backend = [BytecodeTarget]; output_name = None; main_module_name} ->
    if Ext_sys.is_windows_or_cygwin then (String.lowercase main_module_name) ^ ".byte.exe" 
                                    else (String.lowercase main_module_name) ^ ".byte"
  | {backend = [BytecodeTarget]; output_name = Some(name); } -> name
  | _ -> failwith "Internal error: get_exec_name shouldn't be called for anything that isn't a native or bytecode binary"


let check_if_dep ~root_project_dir ~backend (dependency_info : t) flag_exec =
  let components = Ext_string.split_by (fun c -> c = Ext_path.sep_char) flag_exec in
  match components with 
  | [dep_name; entry_name] -> begin 
    match String_map.find_opt dep_name dependency_info.all_toplevel_ppxes with
    | None -> 
      Bsb_exception.no_package_found_for_ppx dep_name entry_name
    | Some l -> begin 
      match List.filter Bsb_config_types.(fun {main_module_name} -> main_module_name = entry_name) l with 
      | [] -> Bsb_exception.ppx_not_found_for_package entry_name dep_name
      | head :: _ -> 
        let nested = begin match backend with
          | Bsb_config_types.Js       -> "js"
          | Bsb_config_types.Native   -> "native"
          | Bsb_config_types.Bytecode -> "bytecode"
          | Bsb_config_types.NativeIos -> "ios"
        end in 
        let exec_name = get_exec_name head in
        root_project_dir // Literals.node_modules // dep_name // Bsb_config.lib_bs // nested // exec_name
      end
    end
  | _ -> flag_exec
#end

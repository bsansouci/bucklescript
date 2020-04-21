(* Copyright (C) 2017 Authors of BuckleScript
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)

let bsdeps = ".bsdeps"

let (//) = Ext_path.combine

(** Regenerate ninja file by need based on [.bsdeps]
    return None if we dont need regenerate
    otherwise return Some info
*)
let regenerate_ninja 
    ~(toplevel_package_specs : Bsb_package_specs.t option)
    ~forced ~per_proj_dir
  : Bsb_config_types.t option =  
  let toplevel = toplevel_package_specs = None in 
  let lib_artifacts_dir = !Bsb_global_backend.lib_artifacts_dir in
  let lib_bs_dir =  per_proj_dir // lib_artifacts_dir  in 
  let output_deps = lib_bs_dir // bsdeps in
  let check_result  =
    Bsb_ninja_check.check 
      ~per_proj_dir:per_proj_dir  
      ~forced ~file:output_deps in
  Bsb_log.info
    "@{<info>BSB check@} build spec : %a @." Bsb_ninja_check.pp_check_result check_result ;
  match check_result  with 
  | Good ->
    None  (* Fast path, no need regenerate ninja *)
  | Bsb_forced 
  | Bsb_bsc_version_mismatch 
  | Bsb_file_not_exist 
  | Bsb_source_directory_changed  
  | Other _ -> 
    if check_result = Bsb_bsc_version_mismatch then begin 
      Bsb_log.warn "@{<info>Different compiler version@}: clean current repo@.";
      Bsb_clean.clean_self  per_proj_dir; 
    end ; 
    
    let config = 
      Bsb_config_parse.interpret_json 
        ~toplevel_package_specs
        ~per_proj_dir in 
    (* create directory, lib/bs, lib/js, lib/es6 etc *)    
    Bsb_build_util.mkp lib_bs_dir;         
    Bsb_package_specs.list_dirs_by config.package_specs
      (fun x -> 
        let dir = per_proj_dir // x in (*Unix.EEXIST error*)
        if not (Sys.file_exists dir) then  Unix.mkdir dir 0o777);
    if toplevel then       
      Bsb_watcher_gen.generate_sourcedirs_meta
        ~name:(lib_bs_dir // Literals.sourcedirs_meta)
        config.file_groups
    ;
#if BS_NATIVE then
    if !Bsb_global_backend.backend = Bsb_config_types.Js then begin
      Bsb_merlin_gen.merlin_file_gen ~per_proj_dir
        config;       
      Bsb_ninja_gen.output_ninja_and_namespace_map 
        ~per_proj_dir  ~toplevel config ;             
    end else begin
      let os = Filename.basename Bsb_global_paths.bsc_dir in
      let plugin_path = Bsb_global_paths.cwd // "node_modules" // "bs-platform-native" // os // "bsb.exe" in
      let status = Sys.command (Printf.sprintf "%s -project-dir %s -lib-artifacts-dir %s -root-project-dir %s -bsc-dir %s -backend %s" plugin_path per_proj_dir lib_bs_dir Bsb_global_paths.cwd Bsb_global_paths.bsc_dir !Bsb_global_backend.backend_string) in
      if status <> 0 then
        exit status;
    end;
#else
    Bsb_merlin_gen.merlin_file_gen ~per_proj_dir
       config;       
    Bsb_ninja_gen.output_ninja_and_namespace_map 
      ~per_proj_dir  ~toplevel config ;             
#end
    
    (* PR2184: we still need record empty dir 
        since it may add files in the future *)  
    Bsb_ninja_check.record ~per_proj_dir ~file:output_deps 
      (Literals.bsconfig_json::config.file_groups.globbed_dirs) ;
    Some config 



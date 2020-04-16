(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
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



(** The complexity comes from the fact that we allow custom rules which could
  conflict with our custom built-in rules
*)
type t  

val print_rule :  out_channel -> 
  description:string  ->
  ?restat : unit  ->
  ?dyndep: string ->
  command:string ->   
  string -> unit

val get_name : t  -> out_channel -> string

(***********************************************************)
(** A list of existing rules *)
type builtin = {
  
  build_ast : t;
  build_ast_from_re : t ;

  (** platform dependent, on Win32,
      invoking cmd.exe
  *)
  copy_resources : t;
  (** Rules below all need restat *)
  build_bin_deps : t ;

#if BS_NATIVE then
  build_package_gen_mlast_simple: t;
  build_package_build_cmi_bytecode: t;
  build_package_build_cmi_native: t;
  
  build_cmo_cmi_bytecode: t;
  build_cmi_bytecode: t;
  build_cmx_cmi_native: t;
  build_cmi_native: t;

  linking_bytecode: t;
  linking_native: t;

  build_cma_library: t;
  build_cmxa_library: t;
#end

  ml_cmj_js : t;
  ml_cmj_js_dev : t;
  ml_cmj_cmi_js : t ;
  ml_cmj_cmi_js_dev : t ;
  ml_cmi : t;
  ml_cmi_dev : t ;

  build_package : t ;
  customs : t Map_string.t
}
(***********************************************************)

(** rules are generally composed of built-in rules and customized rules, there are two design choices:
    1. respect custom rules with the same name, then we need adjust our built-in 
    rules dynamically in case the conflict.
    2. respect our built-in rules, then we only need re-load custom rules for each bsconfig.json
*)

type command = string
(** Since now we generate ninja files per bsconfig.json in a single process, 
    we must make sure it is re-entrant
*)
val make_custom_rules : 
  has_gentype:bool ->
  has_postbuild:bool ->
  has_ppx:bool ->
  has_pp:bool ->
  has_builtin:bool -> 
  bs_suffix:bool ->
  reason_react_jsx : Bsb_config_types.reason_react_jsx option ->
  digest:string ->
  refmt:string option ->
  command Map_string.t ->
  builtin


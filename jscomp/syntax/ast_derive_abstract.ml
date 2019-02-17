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


(* let derivingName = "abstract" *)
module U = Ast_derive_util
open Ast_helper
type tdcls = Parsetree.type_declaration list

type abstractKind = 
  | Not_abstract
  | Light_abstract
  | Complex_abstract 

let  isAbstract (xs :Ast_payload.action list) = 
  match xs with 
  | [{loc; txt = "abstract"}, 
    (None 
    )]  -> 
    Complex_abstract
  | [{loc; txt = "abstract"}, 
    Some {pexp_desc = Pexp_ident {txt = Lident "light"}}  
    ] -> Light_abstract
  | [{loc; txt = "abstract"}, Some _ ]
    -> 
      Location.raise_errorf ~loc "invalid config for abstract"
  | xs -> 
    Ext_list.iter xs (function (({loc; txt}),_) ->  
      match txt with 
      | "abstract" -> 
        Location.raise_errorf ~loc 
          "bs.deriving abstract does not work with any other deriving"
      | _ -> ()
    ) ;
    Not_abstract
(* let handle_config (config : Parsetree.expression option) =
  match config with
  | Some config ->
    U.invalid_config config
  | None -> () *)



let get_optional_attrs =
  [ 
    Ast_attributes.bs_get; 
    Ast_attributes.bs_return_undefined
    ]
(** For this attributes, its type was wrapped as an option,
   so we can still reuse existing frame work
*)  

let get_attrs = [ Ast_attributes.bs_get_arity]
let set_attrs = [Ast_attributes.bs_set]

let deprecated name = 
  Ast_attributes.deprecated  
    ("use " ^ name ^ "Get instead or use {abstract = light} explicitly")


let handleTdcl 
  light
  (tdcl : Parsetree.type_declaration) 
  : Parsetree.type_declaration * Parsetree.value_description list 
  =
  let core_type = U.core_type_of_type_declaration tdcl in
  let loc = tdcl.ptype_loc in
  let type_name = tdcl.ptype_name.txt in
  let newTdcl = {
    tdcl with
    ptype_kind = Ptype_abstract;
    ptype_attributes = [];
    (* avoid non-terminating*)
  } in
  match tdcl.ptype_kind with
  | Ptype_record label_declarations ->
    let is_private = tdcl.ptype_private = Private in
    let has_optional_field =
      Ext_list.exists label_declarations (fun {pld_type; pld_attributes} ->
          Ast_attributes.has_bs_optional pld_attributes
        )  in
    let setter_accessor, makeType, labels =
      Ext_list.fold_right
        label_declarations
        ([],
         (if has_optional_field then
            Ast_compatible.arrow ~loc  (Ast_literal.type_unit ()) core_type
          else  core_type),
         [])
        (fun
          ({pld_name =
              {txt = label_name; loc = label_loc} as pld_name;
            pld_type;
            pld_mutable;
            pld_attributes;
            pld_loc
           }:
             Parsetree.label_declaration) (acc, maker, labels) ->
          let prim_as_name, newLabel =
            match Ast_attributes.iter_process_bs_string_as pld_attributes with
            | None ->
              label_name, pld_name
            | Some new_name ->
              new_name, {pld_name with txt = new_name}
          in
          let prim = [prim_as_name] in 
          let is_optional = Ast_attributes.has_bs_optional pld_attributes in
          
          let maker, acc =
            if is_optional then
              let optional_type = Ast_core_type.lift_option_type pld_type in
              (Ast_compatible.opt_arrow ~loc:pld_loc label_name 
#if OCAML_VERSION =~ "<4.03.0" then
                  optional_type
#else             pld_type  
#end              

                maker,
              let aux light deprec pld_name : Parsetree.value_description = 
                (Val.mk ~loc:pld_loc
                 (if light then pld_name else 
                  {pld_name with txt = pld_name.txt ^ "Get"})
                ~attrs:(if deprec then deprecated (pld_name.Asttypes.txt) :: get_optional_attrs  
                        else get_optional_attrs) ~prim
                (Ast_compatible.arrow ~loc  core_type optional_type)
                ) in 
                if not light then 
                  aux true true pld_name :: aux false false pld_name  :: acc
                else  aux true false pld_name :: acc                   
              )
            else
              Ast_compatible.label_arrow ~loc:pld_loc label_name pld_type maker,
              (
                let aux light deprec pld_name = 
                  Val.mk ~loc:pld_loc 
                    (if light then pld_name else 
                       {pld_name with txt = pld_name.txt ^ "Get"}
                    ) ~attrs:(if deprec then deprecated pld_name.Asttypes.txt :: get_attrs else get_attrs)
                    ~prim:(
                ["" ; (* Not needed actually*)
                External_ffi_types.to_string 
                (Ffi_bs (
                  [{arg_type = Nothing; arg_label = External_arg_spec.empty_label}],
                  Return_identity,
                  Js_get {js_get_name = prim_as_name; js_get_scopes = []}
                  ))] )
               (Ast_compatible.arrow ~loc  core_type pld_type)
               in 
               if not light then 
                aux true true pld_name ::aux false false pld_name :: acc 
               else aux true false pld_name :: acc 
              )
          in
          let is_current_field_mutable = pld_mutable = Mutable in
          let acc =
            if is_current_field_mutable then
              let setter_type =
                (Ast_compatible.arrow core_type
                   (Ast_compatible.arrow
                      pld_type (* setter *)
                      (Ast_literal.type_unit ()))) in
              Val.mk ~loc:pld_loc
                {loc = label_loc; txt = label_name ^ "Set"}
                (* setter *)
                ~attrs:set_attrs
                ~prim setter_type
              :: acc
            else acc in
          acc,
          maker,
          (is_optional, newLabel)::labels
        ) 
    in
    newTdcl,
    (if is_private then
       setter_accessor
     else
       let myPrims =
        External_process.pval_prim_of_option_labels
          labels
          has_optional_field
        in
       let myMaker =
         Val.mk  ~loc
           {loc; txt = type_name}
           ~prim:myPrims makeType in
       (myMaker :: setter_accessor))

  | Ptype_abstract
  | Ptype_variant _
  | Ptype_open ->
    (* Looks obvious that it does not make sense to warn *)
    (* U.notApplicable tdcl.ptype_loc derivingName;  *)
    tdcl, []

let handleTdclsInStr ~light tdcls =
  let tdcls, code =
    Ext_list.fold_right tdcls ([],[]) (fun tdcl (tdcls, sts)  ->
        match handleTdcl light tdcl with
          ntdcl, value_descriptions ->
          ntdcl::tdcls,
          Ext_list.map_append value_descriptions sts (fun x -> Str.primitive x) 
      ) in
Ast_compatible.rec_type_str tdcls :: code
(* still need perform transformation for non-abstract type*)

let handleTdclsInSig ~light tdcls =
  let tdcls, code =
    Ext_list.fold_right tdcls ([],[]) (fun tdcl (tdcls, sts)  ->
        match handleTdcl light tdcl with
          ntdcl, value_descriptions ->
          ntdcl::tdcls,
          Ext_list.map_append value_descriptions sts (fun x -> Sig.value x) 
      ) in
  Ast_compatible.rec_type_sig tdcls :: code

#if BS_NATIVE then

let strip_option arg_name =
  if arg_name.[0] = '?' then
    String.sub arg_name 1 (String.length arg_name - 1)
  else arg_name

(* @Todo refactor me please, this is a bit of a mess due to Ben wanting to ship too quickly.

         Ben - June 8th 2018
*)
let handleTdcl light (tdcl : Parsetree.type_declaration) =
  let core_type = U.core_type_of_type_declaration tdcl in
  let loc = tdcl.ptype_loc in
  let type_name = tdcl.ptype_name.txt in
  match tdcl.ptype_kind with
  | Ptype_record label_declarations ->
    let is_private = tdcl.ptype_private = Private in
    let (has_optional_field, new_label_declarations) =
      Ext_list.fold_right label_declarations (false, []) (fun ({pld_type; pld_loc; pld_attributes} as dcl : Parsetree.label_declaration) (has_optional_field, acc) ->
          let has_optional_field_local = Ast_attributes.has_bs_optional pld_attributes in
          let acc = if has_optional_field_local then
          (* @Incomplete remove ALL attributes when we might want to only remove the bs.optional.

                     Ben - June 8th 2018
           *)
            { dcl with
              pld_type = {ptyp_desc =
               Ptyp_constr(
                 {txt = Lident "option";
                  loc = pld_loc}
                  , [pld_type]);
                  ptyp_loc = pld_loc;
                ptyp_attributes = []
              };
            }
              :: acc
          else dcl :: acc in
            (has_optional_field || has_optional_field_local, acc)
        ) in
    let newTdcl = {
      tdcl with
      ptype_kind = Ptype_record new_label_declarations;
      ptype_attributes = [];
    } in
    let setter_accessor, makeType, labels =
      Ext_list.fold_right
        label_declarations
        ([],
         (if has_optional_field then
            Typ.arrow ~loc "" (Ast_literal.type_unit ()) core_type
          else  core_type),
         [])
        (fun
          ({pld_name =
              {txt = label_name; loc = label_loc} as pld_name;
            pld_type;
            pld_mutable;
            pld_attributes;
            pld_loc
           }:
             Parsetree.label_declaration) (acc, maker, labels) ->
          (* TODO: explain why *)
          let prim, newLabel =
            match Ast_attributes.iter_process_bs_string_as pld_attributes with
            | None ->
              [label_name], pld_name
            | Some new_name ->
              [new_name], {pld_name with txt = new_name}
          in
          let is_option = Ast_attributes.has_bs_optional pld_attributes in
          (* Add the question mark back because that's how ocaml 4.02.3 determines if the argument
            label is for a optional argument vs a named argument. *)
          let newLabel = if is_option then {newLabel with txt = "?" ^ newLabel.Asttypes.txt} else newLabel in

          let maker, getter_type =
             if is_option then
              let maker_optional_type = Ast_core_type.lift_option_type pld_type in
              let getter_optional_type = {
                Parsetree.ptyp_desc =
                 Ptyp_constr(
                   {txt = Lident "option";
                    loc = pld_loc}
                    , [pld_type]);
                ptyp_loc = pld_loc;
                ptyp_attributes = [];
              } in
              Typ.arrow ~loc:pld_loc ("?" ^ label_name) maker_optional_type maker,
              Typ.arrow ~loc "" core_type getter_optional_type
            else
              Typ.arrow ~loc:pld_loc label_name pld_type maker,
               Typ.arrow ~loc "" core_type pld_type
          in
          let makeGetter light deprec pld_name =
            Str.value Nonrecursive [
                Vb.mk
                  ~attrs:(if deprec then deprecated (pld_name.Asttypes.txt) :: []
                    else [])
                  (Pat.var {pld_name with txt = if light then label_name else label_name ^ "Get"})
                  (Exp.constraint_ (Exp.fun_ "" None
                      (Pat.var {Location.txt = "o"; loc = !default_loc})
                      (Exp.field (Exp.ident {Location.txt = Longident.Lident "o"; loc = !default_loc}) {txt = Longident.Lident pld_name.Location.txt; loc = !default_loc})) getter_type)]
            in
            let acc = if not light then
              makeGetter true true pld_name :: makeGetter false false pld_name  :: acc
            else  makeGetter true false pld_name :: acc in
          let is_current_field_mutable = pld_mutable = Mutable in
          let acc =
            if is_current_field_mutable then
              let setter_type =
                (Typ.arrow "" core_type
                   (Typ.arrow ""
                      pld_type (* setter *)
                      (Ast_literal.type_unit ()))) in
              let variable = (Exp.ident {Location.txt = Longident.Lident "v"; loc = !default_loc}) in
              let setter = Str.value Nonrecursive [
                Vb.mk
                  (Pat.var {loc = label_loc; txt = label_name ^ "Set"})
                  (Exp.constraint_ (Exp.fun_ "" None
                      (Pat.var {Location.txt = "o"; loc = !default_loc})
                      (Exp.fun_ "" None
                        (Pat.var {Location.txt = "v"; loc = !default_loc})
                        (Exp.setfield
                          (Exp.ident {Location.txt = Longident.Lident "o"; loc = !default_loc})
                          {txt = Longident.Lident pld_name.Location.txt; loc = !default_loc}
                          (if is_option then Exp.construct {txt=Lident "Some"; loc = !default_loc} (Some variable) else variable))))
                      setter_type)
                  ]
                in
              setter :: acc
            else acc in
          acc,
          maker,
          newLabel::labels
        )
    in
    newTdcl,
    (if is_private then
       setter_accessor
     else
       let maker_body = Exp.record (Ext_list.fold_right labels [] (fun ({ Asttypes.txt }) rest ->
          let field_name = {Asttypes.txt = Longident.Lident (strip_option txt); loc = !default_loc} in
          (field_name, Exp.ident field_name) :: rest
        )) None in
       (* This is to support bs.optional, which makes certain args of the function optional so we
          add a unit at the end to prevent auto-currying issues. *)
       let body_with_extra_unit_fun = (if has_optional_field then
          (Exp.fun_ "" None
            (Pat.var ({txt = "()"; loc = !default_loc})) maker_body)
        else maker_body) in
       let myMaker =
        Str.value Nonrecursive [
          Vb.mk
            (Pat.var {loc; txt = type_name})
            (Exp.constraint_ (
              Ext_list.fold_right
                labels
                body_with_extra_unit_fun
                (fun arg_name rest ->
                  (Exp.fun_ arg_name.Asttypes.txt None
                    (Pat.var ({arg_name with txt = strip_option arg_name.Asttypes.txt})) rest))
                ) makeType)
        ]
        in
       (myMaker :: setter_accessor))

  | Ptype_abstract
  | Ptype_variant _
  | Ptype_open ->
    (* Looks obvious that it does not make sense to warn *)
    (* U.notApplicable tdcl.ptype_loc derivingName;  *)
    tdcl, []

let handleTdclSig light (tdcl : Parsetree.type_declaration) =
  let core_type = U.core_type_of_type_declaration tdcl in
  let loc = tdcl.ptype_loc in
  let type_name = tdcl.ptype_name.txt in
  let newTdcl = {
    tdcl with
    ptype_kind = Ptype_abstract;
    ptype_attributes = [];
    (* avoid non-terminating*)
  } in
  match tdcl.ptype_kind with
  | Ptype_record label_declarations ->
    let is_private = tdcl.ptype_private = Private in
    let has_optional_field =
      Ext_list.exists label_declarations (fun ({pld_type; pld_attributes} : Parsetree.label_declaration) ->
          Ast_attributes.has_bs_optional pld_attributes
        ) in
    let setter_accessor, makeType, labels =
      Ext_list.fold_right
        label_declarations
        ([],
         (if has_optional_field then
            Typ.arrow ~loc "" (Ast_literal.type_unit ()) core_type
          else  core_type),
         [])
        (fun
          ({pld_name =
              {txt = label_name; loc = label_loc} as pld_name;
            pld_type;
            pld_mutable;
            pld_attributes;
            pld_loc
           }:
             Parsetree.label_declaration) (acc, maker, labels) ->
          (* TODO: explain why *)
          let prim, newLabel =
            match Ast_attributes.iter_process_bs_string_as pld_attributes with
            | None ->
              [label_name], pld_name
            | Some new_name ->
              [new_name], {pld_name with txt = new_name}
          in
          let is_option = Ast_attributes.has_bs_optional pld_attributes in

          let maker, getter_type =
             if is_option then
              let maker_optional_type = Ast_core_type.lift_option_type pld_type in
              let getter_optional_type = { Parsetree.ptyp_desc =
                 Ptyp_constr(
                   {txt = Lident "option";
                    loc = pld_loc}
                    , [pld_type]);
                    ptyp_loc = pld_loc;
                  ptyp_attributes = []
                } in
              Typ.arrow ~loc:pld_loc ("?" ^ label_name) maker_optional_type maker,
              Typ.arrow ~loc "" core_type getter_optional_type
            else
              Typ.arrow ~loc:pld_loc label_name pld_type maker,
               Typ.arrow ~loc "" core_type pld_type
          in
          let makeGetter light deprec pld_name =
            Val.mk {pld_name with txt = if light then label_name else label_name ^ "Get"}
              ~attrs:(if deprec then deprecated (pld_name.Asttypes.txt) :: []
                      else [])
              ~prim:[] getter_type
          in
          let acc = if not light then
            makeGetter true true pld_name :: makeGetter false false pld_name  :: acc
          else  makeGetter true false pld_name :: acc in
          let is_current_field_mutable = pld_mutable = Mutable in
          let acc =
            if is_current_field_mutable then
              let setter_type =
                (Typ.arrow "" core_type
                   (Typ.arrow ""
                      pld_type (* setter *)
                      (Ast_literal.type_unit ()))) in
              Val.mk
                {loc = label_loc; txt = label_name ^ "Set"}
                (* setter *)
                ~attrs:[]
                ~prim:[] setter_type
              :: acc
            else acc in
          acc,
          maker,
          (is_option, newLabel)::labels
        )
    in
    newTdcl,
    (if is_private then
       setter_accessor
     else
       let myMaker =
         Val.mk  ~loc
           {loc; txt = type_name}
           ~prim:[] makeType in
       (myMaker :: setter_accessor))

  | Ptype_abstract
  | Ptype_variant _
  | Ptype_open ->
    (* Looks obvious that it does not make sense to warn *)
    (* U.notApplicable tdcl.ptype_loc derivingName;  *)
    tdcl, []

let handleTdclsInStr ~light tdcls =
  let tdcls, tdcls_sig, code, code_sig =
    Ext_list.fold_right tdcls ([],[], [], []) (fun tdcl (tdcls, tdcls_sig, sts, code_sig)  ->
        match handleTdcl light tdcl with
          ntdcl, value_descriptions ->
          let open Parsetree in
          (
            ntdcl::tdcls,
            {ntdcl with ptype_kind = Ptype_abstract }::tdcls_sig,
            Ext_list.map_append value_descriptions sts (fun x -> x),
            Ext_list.map_append value_descriptions code_sig (function
              | {pstr_loc; pstr_desc =
                  Pstr_value (_, (({
                    pvb_pat = {ppat_desc = Ppat_var name};
                    pvb_expr = {pexp_desc = Pexp_constraint (_, typ)}
                  } as _makerVb) :: []))
                } ->
                Sig.value (Val.mk ~loc:pstr_loc name typ)
              | _ -> Sig.type_ []
              )
          )
      )  in
  (Ast_compatible.rec_type_str tdcls :: code, Ast_compatible.rec_type_sig tdcls_sig :: code_sig)
(* still need perform transformation for non-abstract type*)

let handleTdclsInSig ~light tdcls =
  let tdcls, code =
    Ext_list.fold_right tdcls ([],[]) (fun tdcl (tdcls, sts)  ->
        match handleTdclSig light tdcl with
          ntdcl, value_descriptions ->
          ntdcl::tdcls,
          Ext_list.map_append value_descriptions sts (fun x -> Sig.value x)

      )  in
  Ast_compatible.rec_type_sig tdcls :: code

#end

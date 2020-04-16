module Bsb_build_schemas
= struct
#1 "bsb_build_schemas.ml"
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


(* let files = "files" *)
let version = "version"
let name = "name"
(* let ocaml_config = "ocaml-config" *)
let bsdep = "bsdep"
let ppx_flags = "ppx-flags"
let pp_flags = "pp-flags"
let bsc = "bsc"
let refmt = "refmt"

let bs_external_includes = "bs-external-includes"
let bs_lib_dir = "bs-lib-dir"
let bs_dependencies = "bs-dependencies"
let bs_dev_dependencies = "bs-dev-dependencies"


let sources = "sources"
let dir = "dir"
let files = "files"
let subdirs = "subdirs"
let bsc_flags = "bsc-flags"
let excludes = "excludes"
let slow_re = "slow-re"
let resources = "resources"
let public = "public"
let js_post_build = "js-post-build"
let cmd = "cmd"
let ninja = "ninja"
let package_specs = "package-specs"

let generate_merlin = "generate-merlin"

let type_ = "type"
let dev = "dev"

let export_all = "all"
let export_none = "none"

let bsb_dir_group = "bsb_dir_group"
let g_lib_incls = "g_lib_incls"
let use_stdlib = "use-stdlib"
let reason = "reason"
let react_jsx = "react-jsx"

let entries = "entries"
let backend = "backend"
let main_module = "main-module"
let cut_generators = "cut-generators"
let generators = "generators"
let command = "command"
let edge = "edge"
let namespace = "namespace"
let in_source = "in-source"
let warnings = "warnings"
let number = "number"
let error = "error"
let suffix = "suffix"
let gentypeconfig = "gentypeconfig"
let path = "path"
let ignored_dirs = "ignored-dirs"
let static_libraries = "static-libraries"
let c_linker_flags = "c-linker-flags"

end
module Ext_array : sig 
#1 "ext_array.mli"
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






(** Some utilities for {!Array} operations *)
val reverse_range : 'a array -> int -> int -> unit
val reverse_in_place : 'a array -> unit
val reverse : 'a array -> 'a array 
val reverse_of_list : 'a list -> 'a array

val filter : ('a -> bool) -> 'a array -> 'a array

val filter_map : 
'a array -> 
('a -> 'b option) -> 
'b array

val range : int -> int -> int array

val map2i : (int -> 'a -> 'b -> 'c ) -> 'a array -> 'b array -> 'c array

val to_list_f : 
  'a array -> 
  ('a -> 'b) -> 
  'b list 

val to_list_map : ('a -> 'b option) -> 'a array -> 'b list 

val to_list_map_acc : 
  'a array -> 
  'b list -> 
  ('a -> 'b option) -> 
  'b list 

val of_list_map : 
  'a list -> 
  ('a -> 'b) -> 
  'b array 

val rfind_with_index : 'a array -> ('a -> 'b -> bool) -> 'b -> int


type 'a split = [ `No_split | `Split of 'a array * 'a array ]

val rfind_and_split : 
  'a array ->
  ('a -> 'b -> bool) ->
  'b -> 'a split

val find_and_split : 
  'a array ->
  ('a -> 'b -> bool) ->
  'b -> 'a split

val exists : ('a -> bool) -> 'a array -> bool 

val is_empty : 'a array -> bool 

val for_all2_no_exn : 
  'a array ->
  'b array -> 
  ('a -> 'b -> bool) -> 
  bool

val map :   
  'a array -> 
  ('a -> 'b) -> 
  'b array

val iter :
  'a array -> 
  ('a -> unit) -> 
  unit

val fold_left :   
  'b array -> 
  'a -> 
  ('a -> 'b -> 'a) ->   
  'a

val get_or :   
  'a array -> 
  int -> 
  (unit -> 'a) -> 
  'a
end = struct
#1 "ext_array.ml"
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





let reverse_range a i len =
  if len = 0 then ()
  else
    for k = 0 to (len-1)/2 do
      let t = Array.unsafe_get a (i+k) in
      Array.unsafe_set a (i+k) ( Array.unsafe_get a (i+len-1-k));
      Array.unsafe_set a (i+len-1-k) t;
    done


let reverse_in_place a =
  reverse_range a 0 (Array.length a)

let reverse a =
  let b_len = Array.length a in
  if b_len = 0 then [||] else  
    let b = Array.copy a in  
    for i = 0 to  b_len - 1 do
      Array.unsafe_set b i (Array.unsafe_get a (b_len - 1 -i )) 
    done;
    b  

let reverse_of_list =  function
  | [] -> [||]
  | hd::tl as l ->
    let len = List.length l in
    let a = Array.make len hd in
    let rec fill i = function
      | [] -> a
      | hd::tl -> Array.unsafe_set a (len - i - 2) hd; fill (i+1) tl in
    fill 0 tl

let filter f a =
  let arr_len = Array.length a in
  let rec aux acc i =
    if i = arr_len 
    then reverse_of_list acc 
    else
      let v = Array.unsafe_get a i in
      if f  v then 
        aux (v::acc) (i+1)
      else aux acc (i + 1) 
  in aux [] 0


let filter_map a (f : _ -> _ option)  =
  let arr_len = Array.length a in
  let rec aux acc i =
    if i = arr_len 
    then reverse_of_list acc 
    else
      let v = Array.unsafe_get a i in
      match f  v with 
      | Some v -> 
        aux (v::acc) (i+1)
      | None -> 
        aux acc (i + 1) 
  in aux [] 0

let range from to_ =
  if from > to_ then invalid_arg "Ext_array.range"  
  else Array.init (to_ - from + 1) (fun i -> i + from)

let map2i f a b = 
  let len = Array.length a in 
  if len <> Array.length b then 
    invalid_arg "Ext_array.map2i"  
  else
    Array.mapi (fun i a -> f i  a ( Array.unsafe_get b i )) a 

let rec tolist_f_aux a f  i res =
  if i < 0 then res else
    let v = Array.unsafe_get a i in
    tolist_f_aux a f  (i - 1)
      (f v :: res)
       
let to_list_f a f = tolist_f_aux a f (Array.length a  - 1) []

let rec tolist_aux a f  i res =
  if i < 0 then res else
    let v = Array.unsafe_get a i in
    tolist_aux a f  (i - 1)
      (match f v with
       | Some v -> v :: res
       | None -> res) 

let to_list_map f a = 
  tolist_aux a f (Array.length a - 1) []

let to_list_map_acc a acc f = 
  tolist_aux a f (Array.length a - 1) acc


let of_list_map a f = 
  match a with 
  | [] -> [||]
  | [a0] -> 
    let b0 = f a0 in
    [|b0|]
  | [a0;a1] -> 
    let b0 = f a0 in  
    let b1 = f a1 in 
    [|b0;b1|]
  | [a0;a1;a2] -> 
    let b0 = f a0 in  
    let b1 = f a1 in 
    let b2 = f a2 in  
    [|b0;b1;b2|]
  | [a0;a1;a2;a3] -> 
    let b0 = f a0 in  
    let b1 = f a1 in 
    let b2 = f a2 in  
    let b3 = f a3 in 
    [|b0;b1;b2;b3|]
  | [a0;a1;a2;a3;a4] -> 
    let b0 = f a0 in  
    let b1 = f a1 in 
    let b2 = f a2 in  
    let b3 = f a3 in 
    let b4 = f a4 in 
    [|b0;b1;b2;b3;b4|]

  | a0::a1::a2::a3::a4::tl -> 
    let b0 = f a0 in  
    let b1 = f a1 in 
    let b2 = f a2 in  
    let b3 = f a3 in 
    let b4 = f a4 in 
    let len = List.length tl + 5 in 
    let arr = Array.make len b0  in
    Array.unsafe_set arr 1 b1 ;  
    Array.unsafe_set arr 2 b2 ;
    Array.unsafe_set arr 3 b3 ; 
    Array.unsafe_set arr 4 b4 ; 
    let rec fill i = function
      | [] -> arr 
      | hd :: tl -> 
        Array.unsafe_set arr i (f hd); 
        fill (i + 1) tl in 
    fill 5 tl

(**
   {[
     # rfind_with_index [|1;2;3|] (=) 2;;
     - : int = 1
               # rfind_with_index [|1;2;3|] (=) 1;;
     - : int = 0
               # rfind_with_index [|1;2;3|] (=) 3;;
     - : int = 2
               # rfind_with_index [|1;2;3|] (=) 4;;
     - : int = -1
   ]}
*)
let rfind_with_index arr cmp v = 
  let len = Array.length arr in 
  let rec aux i = 
    if i < 0 then i
    else if  cmp (Array.unsafe_get arr i) v then i
    else aux (i - 1) in 
  aux (len - 1)

type 'a split = [ `No_split | `Split of 'a array * 'a array ]
let rfind_and_split arr cmp v : _ split = 
  let i = rfind_with_index arr cmp v in 
  if  i < 0 then 
    `No_split 
  else 
    `Split (Array.sub arr 0 i , Array.sub arr  (i + 1 ) (Array.length arr - i - 1 ))


let find_with_index arr cmp v = 
  let len  = Array.length arr in 
  let rec aux i len = 
    if i >= len then -1 
    else if cmp (Array.unsafe_get arr i ) v then i 
    else aux (i + 1) len in 
  aux 0 len

let find_and_split arr cmp v : _ split = 
  let i = find_with_index arr cmp v in 
  if i < 0 then 
    `No_split
  else
    `Split (Array.sub arr 0 i, Array.sub arr (i + 1 ) (Array.length arr - i - 1))        

(** TODO: available since 4.03, use {!Array.exists} *)

let exists p a =
  let n = Array.length a in
  let rec loop i =
    if i = n then false
    else if p (Array.unsafe_get a i) then true
    else loop (succ i) in
  loop 0


let is_empty arr =
  Array.length arr = 0


let rec unsafe_loop index len p xs ys  = 
  if index >= len then true
  else 
    p 
      (Array.unsafe_get xs index)
      (Array.unsafe_get ys index) &&
    unsafe_loop (succ index) len p xs ys 

let for_all2_no_exn xs ys p = 
  let len_xs = Array.length xs in 
  let len_ys = Array.length ys in 
  len_xs = len_ys &&    
  unsafe_loop 0 len_xs p xs ys


let map a f =
  let open Array in 
  let l = length a in
  if l = 0 then [||] else begin
    let r = make l (f(unsafe_get a 0)) in
    for i = 1 to l - 1 do
      unsafe_set r i (f(unsafe_get a i))
    done;
    r
  end

let iter a f =
  let open Array in 
  for i = 0 to length a - 1 do f(unsafe_get a i) done


  let fold_left a x f =
    let open Array in 
    let r = ref x in    
    for i = 0 to length a - 1 do
      r := f !r (unsafe_get a i)
    done;
    !r
  
let get_or arr i cb =     
  if i >=0 && i < Array.length arr then 
    Array.unsafe_get arr i 
  else cb ()  
end
module Ext_list : sig 
#1 "ext_list.mli"
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


val map : 
  'a list -> 
  ('a -> 'b) -> 
  'b list 

val map_combine :  
  'a list -> 
  'b list -> 
  ('a -> 'c) -> 
  ('c * 'b) list 
  
val has_string :   
  string list ->
  string -> 
  bool


val map_split_opt :  
  'a list ->
  ('a -> 'b option * 'c option) ->
  'b list * 'c list 

val mapi :
  'a list -> 
  (int -> 'a -> 'b) -> 
  'b list 
  
val map_snd : ('a * 'b) list -> ('b -> 'c) -> ('a * 'c) list 

(** [map_last f xs ]
    will pass [true] to [f] for the last element, 
    [false] otherwise. 
    For empty list, it returns empty
*)
val map_last : 
    'a list -> 
    (bool -> 'a -> 'b) -> 'b list

(** [last l]
    return the last element
    raise if the list is empty
*)
val last : 'a list -> 'a

val append : 
  'a list -> 
  'a list -> 
  'a list 

val append_one :  
  'a list -> 
  'a -> 
  'a list

val map_append :  
  'b list -> 
  'a list -> 
  ('b -> 'a) -> 
  'a list

val fold_right : 
  'a list -> 
  'b -> 
  ('a -> 'b -> 'b) -> 
  'b

val fold_right2 : 
  'a list -> 
  'b list -> 
  'c -> 
  ('a -> 'b -> 'c -> 'c) ->  'c

val map2 : 
  'a list ->
  'b list ->
  ('a -> 'b -> 'c) ->
  'c list

val fold_left_with_offset : 
  'a list -> 
  'acc -> 
  int -> 
  ('a -> 'acc ->  int ->  'acc) ->   
  'acc 


(** @unused *)
val filter_map : 
  'a list -> 
  ('a -> 'b option) -> 
  'b list  

(** [exclude p l] is the opposite of [filter p l] *)
val exclude : 
  'a list -> 
  ('a -> bool) -> 
  'a list 

(** [excludes p l]
    return a tuple [excluded,newl]
    where [exluded] is true indicates that at least one  
    element is removed,[newl] is the new list where all [p x] for [x] is false

*)
val exclude_with_val : 
  'a list -> 
  ('a -> bool) -> 
  'a list option


val same_length : 'a list -> 'b list -> bool

val init : int -> (int -> 'a) -> 'a list

(** [split_at n l]
    will split [l] into two lists [a,b], [a] will be of length [n], 
    otherwise, it will raise
*)
val split_at : 
  'a list -> 
  int -> 
  'a list * 'a list


(** [split_at_last l]
    It is equivalent to [split_at (List.length l - 1) l ]
*)
val split_at_last : 'a list -> 'a list * 'a

val filter_mapi : 
  'a list -> 
  ('a -> int ->  'b option) -> 
  'b list

val filter_map2 : 
  'a list -> 
  'b list -> 
  ('a -> 'b -> 'c option) -> 
  'c list


val length_compare : 'a list -> int -> [`Gt | `Eq | `Lt ]

val length_ge : 'a list -> int -> bool

(**

   {[length xs = length ys + n ]}
   input n should be positive 
   TODO: input checking
*)

val length_larger_than_n : 
  'a list -> 
  'a list -> 
   int -> 
   bool


(**
   [rev_map_append f l1 l2]
   [map f l1] and reverse it to append [l2]
   This weird semantics is due to it is the most efficient operation
   we can do
*)
val rev_map_append : 
  'a list -> 
  'b list -> 
  ('a -> 'b) -> 
  'b list


val flat_map : 
  'a list -> 
  ('a -> 'b list) -> 
  'b list

val flat_map_append : 
  'a list -> 
  'b list  ->
  ('a -> 'b list) -> 
  'b list


(**
    [stable_group eq lst]
    Example:
    Input:
   {[
     stable_group (=) [1;2;3;4;3]
   ]}
    Output:
   {[
     [[1];[2];[4];[3;3]]
   ]}
    TODO: this is O(n^2) behavior 
    which could be improved later
*)
val stable_group : 
  'a list -> 
  ('a -> 'a -> bool) -> 
  'a list list 

(** [drop n list]
    raise when [n] is negative
    raise when list's length is less than [n]
*)
val drop : 
  'a list -> 
  int -> 
  'a list 

val find_first :   
    'a list ->
    ('a -> bool) ->
    'a option 
    
(** [find_first_not p lst ]
    if all elements in [lst] pass, return [None] 
    otherwise return the first element [e] as [Some e] which
    fails the predicate
*)
val find_first_not : 
  'a list -> 
  ('a -> bool) -> 
  'a option 

(** [find_opt f l] returns [None] if all return [None],  
    otherwise returns the first one. 
*)

val find_opt : 
  'a list -> 
  ('a -> 'b option) -> 
  'b option 

val find_def : 
    'a list -> 
    ('a -> 'b option) ->
    'b ->
    'b 

    
val rev_iter : 
  'a list -> 
  ('a -> unit) -> 
  unit 

val iter:   
   'a list ->  
   ('a -> unit) -> 
   unit
   
val for_all:  
    'a list -> 
    ('a -> bool) -> 
    bool
val for_all_snd:    
    ('a * 'b) list -> 
    ('b -> bool) -> 
    bool

(** [for_all2_no_exn p xs ys]
    return [true] if all satisfied,
    [false] otherwise or length not equal
*)
val for_all2_no_exn : 
  'a list -> 
  'b list -> 
  ('a -> 'b -> bool) -> 
  bool



(** [f] is applied follow the list order *)
val split_map : 
  'a list -> 
  ('a -> 'b * 'c) -> 
  'b list * 'c list       

(** [fn] is applied from left to right *)
val reduce_from_left : 
  'a list -> 
  ('a -> 'a -> 'a) ->
  'a

val sort_via_array :
  'a list -> 
  ('a -> 'a -> int) -> 
  'a list  




(** [assoc_by_string default key lst]
    if  [key] is found in the list  return that val,
    other unbox the [default], 
    otherwise [assert false ]
*)
val assoc_by_string : 
  (string * 'a) list -> 
  string -> 
  'a  option ->   
  'a  

val assoc_by_int : 
  (int * 'a) list -> 
  int -> 
  'a  option ->   
  'a   


val nth_opt : 'a list -> int -> 'a option  

val iter_snd : ('a * 'b) list -> ('b -> unit) -> unit 

val iter_fst : ('a * 'b) list -> ('a -> unit) -> unit 

val exists : 'a list -> ('a -> bool) -> bool 

val exists_fst : 
  ('a * 'b) list ->
  ('a -> bool) ->
  bool

val exists_snd : 
  ('a * 'b) list -> 
  ('b -> bool) -> 
  bool

val concat_append:
    'a list list -> 
    'a list -> 
    'a list

val fold_left2:
    'a list -> 
    'b list -> 
    'c -> 
    ('a -> 'b -> 'c -> 'c)
    -> 'c 

val fold_left:    
    'a list -> 
    'b -> 
    ('b -> 'a -> 'b) -> 
    'b

val singleton_exn:     
    'a list -> 'a

val mem_string :     
    string list -> 
    string -> 
    bool
end = struct
#1 "ext_list.ml"
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




let rec map l f =
  match l with
  | [] ->
    []
  | [x1] ->
    let y1 = f x1 in
    [y1]
  | [x1; x2] ->
    let y1 = f x1 in
    let y2 = f x2 in
    [y1; y2]
  | [x1; x2; x3] ->
    let y1 = f x1 in
    let y2 = f x2 in
    let y3 = f x3 in
    [y1; y2; y3]
  | [x1; x2; x3; x4] ->
    let y1 = f x1 in
    let y2 = f x2 in
    let y3 = f x3 in
    let y4 = f x4 in
    [y1; y2; y3; y4]
  | x1::x2::x3::x4::x5::tail ->
    let y1 = f x1 in
    let y2 = f x2 in
    let y3 = f x3 in
    let y4 = f x4 in
    let y5 = f x5 in
    y1::y2::y3::y4::y5::(map tail f)

let rec has_string l f =
  match l with
  | [] ->
    false
  | [x1] ->
    x1 = f
  | [x1; x2] ->
    x1 = f || x2 = f
  | [x1; x2; x3] ->
    x1 = f || x2 = f || x3 = f
  | x1 :: x2 :: x3 :: x4 ->
    x1 = f || x2 = f || x3 = f || has_string x4 f 
  
let rec map_combine l1 l2 f =
  match (l1, l2) with
    ([], []) -> []
  | (a1::l1, a2::l2) -> 
    (f a1, a2) :: map_combine l1 l2 f 
  | (_, _) -> 
    invalid_arg "Ext_list.map_combine"

let rec map_split_opt 
  (xs : 'a list)  (f : 'a -> 'b option * 'c option) 
  : 'b list * 'c list = 
  match xs with 
  | [] -> [], []
  | x::xs ->
    let c,d = f x in 
    let cs,ds = map_split_opt xs f in 
    (match c with Some c -> c::cs | None -> cs),
    (match d with Some d -> d::ds | None -> ds)

let rec map_snd l f =
  match l with
  | [] ->
    []
  | [ v1,x1 ] ->
    let y1 = f x1 in
    [v1,y1]
  | [v1, x1; v2, x2] ->
    let y1 = f x1 in
    let y2 = f x2 in
    [v1, y1; v2, y2]
  | [ v1, x1; v2, x2; v3, x3] ->
    let y1 = f x1 in
    let y2 = f x2 in
    let y3 = f x3 in
    [v1, y1; v2, y2; v3, y3]
  | [ v1, x1; v2, x2; v3, x3; v4, x4] ->
    let y1 = f x1 in
    let y2 = f x2 in
    let y3 = f x3 in
    let y4 = f x4 in
    [v1, y1; v2, y2; v3, y3; v4, y4]
  | (v1, x1) ::(v2, x2) :: (v3, x3)::(v4, x4) :: (v5, x5) ::tail ->
    let y1 = f x1 in
    let y2 = f x2 in
    let y3 = f x3 in
    let y4 = f x4 in
    let y5 = f x5 in
    (v1, y1)::(v2, y2) :: (v3, y3) :: (v4, y4) :: (v5, y5) :: (map_snd tail f)


let rec map_last l f=
  match l with
  | [] ->
    []
  | [x1] ->
    let y1 = f true x1 in
    [y1]
  | [x1; x2] ->
    let y1 = f false x1 in
    let y2 = f true x2 in
    [y1; y2]
  | [x1; x2; x3] ->
    let y1 = f false x1 in
    let y2 = f false x2 in
    let y3 = f true x3 in
    [y1; y2; y3]
  | [x1; x2; x3; x4] ->
    let y1 = f false x1 in
    let y2 = f false x2 in
    let y3 = f false x3 in
    let y4 = f true x4 in
    [y1; y2; y3; y4]
  | x1::x2::x3::x4::tail ->
    (* make sure that tail is not empty *)    
    let y1 = f false x1 in
    let y2 = f false x2 in
    let y3 = f false x3 in
    let y4 = f false x4 in
    y1::y2::y3::y4::(map_last tail f)

let rec mapi_aux lst i f = 
  match lst with
    [] -> []
  | a::l -> 
    let r = f i a in r :: mapi_aux l (i + 1) f 

let mapi lst f = mapi_aux lst 0 f

let rec last xs =
  match xs with 
  | [x] -> x 
  | _ :: tl -> last tl 
  | [] -> invalid_arg "Ext_list.last"    



let rec append_aux l1 l2 = 
  match l1 with
  | [] -> l2
  | [a0] -> a0::l2
  | [a0;a1] -> a0::a1::l2
  | [a0;a1;a2] -> a0::a1::a2::l2
  | [a0;a1;a2;a3] -> a0::a1::a2::a3::l2
  | [a0;a1;a2;a3;a4] -> a0::a1::a2::a3::a4::l2
  | a0::a1::a2::a3::a4::rest -> a0::a1::a2::a3::a4::append_aux rest l2

let append l1 l2 =   
  match l2 with 
  | [] -> l1 
  | _ -> append_aux l1 l2  

let append_one l1 x = append_aux l1 [x]  

let rec map_append l1 l2 f =   
  match l1 with
  | [] -> l2
  | [a0] -> f a0::l2
  | [a0;a1] -> 
    let b0 = f a0 in 
    let b1 = f a1 in 
    b0::b1::l2
  | [a0;a1;a2] -> 
    let b0 = f a0 in 
    let b1 = f a1 in  
    let b2 = f a2 in 
    b0::b1::b2::l2
  | [a0;a1;a2;a3] -> 
    let b0 = f a0 in 
    let b1 = f a1 in 
    let b2 = f a2 in 
    let b3 = f a3 in 
    b0::b1::b2::b3::l2
  | [a0;a1;a2;a3;a4] -> 
    let b0 = f a0 in 
    let b1 = f a1 in 
    let b2 = f a2 in 
    let b3 = f a3 in 
    let b4 = f a4 in 
    b0::b1::b2::b3::b4::l2

  | a0::a1::a2::a3::a4::rest ->
    let b0 = f a0 in 
    let b1 = f a1 in 
    let b2 = f a2 in 
    let b3 = f a3 in 
    let b4 = f a4 in 
    b0::b1::b2::b3::b4::map_append rest l2 f



let rec fold_right l acc f  = 
  match l with  
  | [] -> acc 
  | [a0] -> f a0 acc 
  | [a0;a1] -> f a0 (f a1 acc)
  | [a0;a1;a2] -> f a0 (f a1 (f a2 acc))
  | [a0;a1;a2;a3] -> f a0 (f a1 (f a2 (f a3 acc))) 
  | [a0;a1;a2;a3;a4] -> 
    f a0 (f a1 (f a2 (f a3 (f a4 acc))))
  | a0::a1::a2::a3::a4::rest -> 
    f a0 (f a1 (f a2 (f a3 (f a4 (fold_right rest acc f )))))  

let rec fold_right2 l r acc f = 
  match l,r  with  
  | [],[] -> acc 
  | [a0],[b0] -> f a0 b0 acc 
  | [a0;a1],[b0;b1] -> f a0 b0 (f a1 b1 acc)
  | [a0;a1;a2],[b0;b1;b2] -> f a0 b0 (f a1 b1 (f a2 b2 acc))
  | [a0;a1;a2;a3],[b0;b1;b2;b3] ->
    f a0 b0 (f a1 b1 (f a2 b2 (f a3 b3 acc))) 
  | [a0;a1;a2;a3;a4], [b0;b1;b2;b3;b4] -> 
    f a0 b0 (f a1 b1 (f a2 b2 (f a3 b3 (f a4 b4 acc))))
  | a0::a1::a2::a3::a4::arest, b0::b1::b2::b3::b4::brest -> 
    f a0 b0 (f a1 b1 (f a2 b2 (f a3 b3 (f a4 b4 (fold_right2 arest brest acc f )))))  
  | _, _ -> invalid_arg "Ext_list.fold_right2"

let rec map2  l r f = 
  match l,r  with  
  | [],[] -> []
  | [a0],[b0] -> [f a0 b0]
  | [a0;a1],[b0;b1] -> 
    let c0 = f a0 b0 in 
    let c1 = f a1 b1 in 
    [c0; c1]
  | [a0;a1;a2],[b0;b1;b2] -> 
    let c0 = f a0 b0 in 
    let c1 = f a1 b1 in 
    let c2 = f a2 b2 in 
    [c0;c1;c2]
  | [a0;a1;a2;a3],[b0;b1;b2;b3] ->
    let c0 = f a0 b0 in 
    let c1 = f a1 b1 in 
    let c2 = f a2 b2 in 
    let c3 = f a3 b3 in 
    [c0;c1;c2;c3]
  | [a0;a1;a2;a3;a4], [b0;b1;b2;b3;b4] -> 
    let c0 = f a0 b0 in 
    let c1 = f a1 b1 in 
    let c2 = f a2 b2 in 
    let c3 = f a3 b3 in 
    let c4 = f a4 b4 in 
    [c0;c1;c2;c3;c4]
  | a0::a1::a2::a3::a4::arest, b0::b1::b2::b3::b4::brest -> 
    let c0 = f a0 b0 in 
    let c1 = f a1 b1 in 
    let c2 = f a2 b2 in 
    let c3 = f a3 b3 in 
    let c4 = f a4 b4 in 
    c0::c1::c2::c3::c4::map2 arest brest f
  | _, _ -> invalid_arg "Ext_list.map2"

let rec fold_left_with_offset l accu i f =
  match l with
  | [] -> accu
  | a::l -> 
    fold_left_with_offset 
    l     
    (f  a accu  i)  
    (i + 1)
    f  


let rec filter_map xs (f: 'a -> 'b option)= 
  match xs with 
  | [] -> []
  | y :: ys -> 
    begin match f y with 
      | None -> filter_map ys f 
      | Some z -> z :: filter_map ys f 
    end

let rec exclude (xs : 'a list) (p : 'a -> bool) : 'a list =   
  match xs with 
  | [] ->  []
  | x::xs -> 
    if p x then exclude xs p
    else x:: exclude xs p

let rec exclude_with_val l p =
  match l with 
  | [] ->  None
  | a0::xs -> 
    if p a0 then Some (exclude xs p)
    else 
      match xs with 
      | [] -> None
      | a1::rest -> 
        if p a1 then 
          Some (a0:: exclude rest p)
        else 
          match exclude_with_val rest p with 
          | None -> None 
          | Some  rest -> Some (a0::a1::rest)



let rec same_length xs ys = 
  match xs, ys with 
  | [], [] -> true
  | _::xs, _::ys -> same_length xs ys 
  | _, _ -> false 


let init n f = 
  match n with 
  | 0 -> []
  | 1 -> 
    let a0 = f 0 in  
    [a0]
  | 2 -> 
    let a0 = f 0 in 
    let a1 = f 1 in 
    [a0; a1]
  | 3 -> 
    let a0 = f 0 in 
    let a1 = f 1 in 
    let a2 = f 2 in 
    [a0; a1; a2]
  | 4 -> 
    let a0 = f 0 in 
    let a1 = f 1 in 
    let a2 = f 2 in 
    let a3 = f 3 in 
    [a0; a1; a2; a3]
  | 5 -> 
    let a0 = f 0 in 
    let a1 = f 1 in 
    let a2 = f 2 in 
    let a3 = f 3 in 
    let a4 = f 4 in  
    [a0; a1; a2; a3; a4]
  | _ ->
    Array.to_list (Array.init n f)

let rec rev_append l1 l2 =
  match l1 with
    [] -> l2
  | a :: l -> rev_append l   (a :: l2)

let rev l = rev_append l []      

let rec small_split_at n acc l = 
  if n <= 0 then rev acc , l 
  else 
    match l with 
    | x::xs -> small_split_at (n - 1) (x ::acc) xs 
    | _ -> invalid_arg "Ext_list.split_at"

let split_at l n = 
  small_split_at n [] l 

let rec split_at_last_aux acc x = 
  match x with 
  | [] -> invalid_arg "Ext_list.split_at_last"
  | [ x] -> rev acc, x
  | y0::ys -> split_at_last_aux (y0::acc) ys   

let split_at_last (x : 'a list) = 
  match x with 
  | [] -> invalid_arg "Ext_list.split_at_last"
  | [a0] -> 
    [], a0
  | [a0;a1] -> 
    [a0], a1  
  | [a0;a1;a2] -> 
    [a0;a1], a2 
  | [a0;a1;a2;a3] -> 
    [a0;a1;a2], a3 
  | [a0;a1;a2;a3;a4] ->
    [a0;a1;a2;a3], a4 
  | a0::a1::a2::a3::a4::rest  ->  
    let rev, last = split_at_last_aux [] rest
    in 
    a0::a1::a2::a3::a4::  rev , last

(**
   can not do loop unroll due to state combination
*)  
let  filter_mapi xs f  = 
  let rec aux i xs = 
    match xs with 
    | [] -> []
    | y :: ys -> 
      begin match f y i with 
        | None -> aux (i + 1) ys
        | Some z -> z :: aux (i + 1) ys
      end in
  aux 0 xs 

let rec filter_map2  xs ys (f: 'a -> 'b -> 'c option) = 
  match xs,ys with 
  | [],[] -> []
  | u::us, v :: vs -> 
    begin match f u v with 
      | None -> filter_map2 us vs f (* idea: rec f us vs instead? *)
      | Some z -> z :: filter_map2  us vs f
    end
  | _ -> invalid_arg "Ext_list.filter_map2"


let rec rev_map_append l1 l2 f =
  match l1 with
  | [] -> l2
  | a :: l -> rev_map_append l (f a :: l2) f



(** It is not worth loop unrolling, 
    it is already tail-call, and we need to be careful 
    about evaluation order when unroll
*)
let rec flat_map_aux f acc append lx =
  match lx with
  | [] -> rev_append acc  append
  | a0::rest -> flat_map_aux f (rev_append (f a0)  acc ) append rest 

let flat_map lx f  =
  flat_map_aux f [] [] lx

let flat_map_append lx append f =
  flat_map_aux f [] append lx  


let rec length_compare l n = 
  if n < 0 then `Gt 
  else 
    begin match l with 
      | _ ::xs -> length_compare xs (n - 1)
      | [] ->  
        if n = 0 then `Eq 
        else `Lt 
    end

let rec length_ge l n =   
  if n > 0 then
    match l with 
    | _ :: tl -> length_ge tl (n - 1)
    | [] -> false
  else true
(**

   {[length xs = length ys + n ]}
*)
let rec length_larger_than_n xs ys n =
  match xs, ys with 
  | _, [] -> length_compare xs n = `Eq   
  | _::xs, _::ys -> 
    length_larger_than_n xs ys n
  | [], _ -> false 




let rec group (eq : 'a -> 'a -> bool) lst =
  match lst with 
  | [] -> []
  | x::xs -> 
    aux eq x (group eq xs )

and aux eq (x : 'a)  (xss : 'a list list) : 'a list list = 
  match xss with 
  | [] -> [[x]]
  | (y0::_ as y)::ys -> (* cannot be empty *) 
    if eq x y0 then
      (x::y) :: ys 
    else
      y :: aux eq x ys                                 
  | _ :: _ -> assert false    

let stable_group lst eq =  group eq lst |> rev  

let rec drop h n = 
  if n < 0 then invalid_arg "Ext_list.drop"
  else
  if n = 0 then h 
  else 
    match h with 
    | [] ->
      invalid_arg "Ext_list.drop"
    | _ :: tl ->   
      drop tl (n - 1)

let rec find_first x p = 
  match x with 
  | [] -> None
  | x :: l -> 
    if p x then Some x 
    else find_first l p

let rec find_first_not  xs p = 
  match xs with 
  | [] -> None
  | a::l -> 
    if p a 
    then find_first_not l p 
    else Some a 


let rec rev_iter l f = 
  match l with
  | [] -> ()    
  | [x1] ->
    f x1 
  | [x1; x2] ->
    f x2 ; f x1 
  | [x1; x2; x3] ->
    f x3 ; f x2 ; f x1 
  | [x1; x2; x3; x4] ->
    f x4; f x3; f x2; f x1 
  | x1::x2::x3::x4::x5::tail ->
    rev_iter tail f;
    f x5; f x4 ; f x3; f x2 ; f x1

let rec iter l f = 
  match l with
  | [] -> ()    
  | [x1] ->
    f x1 
  | [x1; x2] ->
    f x1 ; f x2
  | [x1; x2; x3] ->
    f x1 ; f x2 ; f x3
  | [x1; x2; x3; x4] ->
    f x1; f x2; f x3; f x4
  | x1::x2::x3::x4::x5::tail ->
    f x1; f x2 ; f x3; f x4 ; f x5;
    iter tail f 


let rec for_all lst p = 
  match lst with 
    [] -> true
  | a::l -> p a && for_all l p

let rec for_all_snd lst p = 
  match lst with 
    [] -> true
  | (_,a)::l -> p a && for_all_snd l p


let rec for_all2_no_exn  l1 l2 p = 
  match (l1, l2) with
  | ([], []) -> true
  | (a1::l1, a2::l2) -> p a1 a2 && for_all2_no_exn l1 l2 p
  | (_, _) -> false


let rec find_opt xs p = 
  match xs with 
  | [] -> None
  | x :: l -> 
    match  p x with 
    | Some _ as v  ->  v
    | None -> find_opt l p

let rec find_def xs p def =
  match xs with 
  | [] -> def
  | x::l -> 
    match p x with 
    | Some v -> v 
    | None -> find_def l p def   

let rec split_map l f = 
  match l with
  | [] ->
    [],[]
  | [x1] ->
    let a0,b0 = f x1 in
    [a0],[b0]
  | [x1; x2] ->
    let a1,b1 = f x1 in
    let a2,b2 = f x2 in
    [a1;a2],[b1;b2]
  | [x1; x2; x3] ->
    let a1,b1 = f x1 in
    let a2,b2 = f x2 in
    let a3,b3 = f x3 in
    [a1;a2;a3], [b1;b2;b3]
  | [x1; x2; x3; x4] ->
    let a1,b1 = f x1 in
    let a2,b2 = f x2 in
    let a3,b3 = f x3 in
    let a4,b4 = f x4 in
    [a1;a2;a3;a4], [b1;b2;b3;b4] 
  | x1::x2::x3::x4::x5::tail ->
    let a1,b1 = f x1 in
    let a2,b2 = f x2 in
    let a3,b3 = f x3 in
    let a4,b4 = f x4 in
    let a5,b5 = f x5 in
    let ass,bss = split_map tail f in 
    a1::a2::a3::a4::a5::ass,
    b1::b2::b3::b4::b5::bss




let sort_via_array lst cmp =
  let arr = Array.of_list lst  in
  Array.sort cmp arr;
  Array.to_list arr




let rec assoc_by_string lst (k : string) def  = 
  match lst with 
  | [] -> 
    begin match def with 
      | None -> assert false 
      | Some x -> x end
  | (k1,v1)::rest -> 
    if  k1 = k then v1 else 
      assoc_by_string  rest k def 

let rec assoc_by_int lst (k : int) def = 
  match lst with 
  | [] -> 
    begin match def with
      | None -> assert false 
      | Some x -> x end
  | (k1,v1)::rest -> 
    if k1 = k then v1 else 
      assoc_by_int rest k def 


let rec nth_aux l n =
  match l with
  | [] -> None
  | a::l -> if n = 0 then Some a else nth_aux l (n-1)

let nth_opt l n =
  if n < 0 then None 
  else
    nth_aux l n

let rec iter_snd lst f =     
  match lst with
  | [] -> ()
  | (_,x)::xs -> 
    f x ; 
    iter_snd xs f 
    
let rec iter_fst lst f =     
  match lst with
  | [] -> ()
  | (x,_)::xs -> 
    f x ; 
    iter_fst xs f 

let rec exists l p =     
  match l with 
    [] -> false  
  | x :: xs -> p x || exists xs p

let rec exists_fst l p = 
  match l with 
    [] -> false
  | (a,_)::l -> p a || exists_fst l p 

let rec exists_snd l p = 
  match l with 
    [] -> false
  | (_, a)::l -> p a || exists_snd l p 

let rec concat_append 
  (xss : 'a list list)  
  (xs : 'a list) : 'a list = 
  match xss with 
  | [] -> xs 
  | l::r -> append l (concat_append r xs)

let rec fold_left l accu f =
  match l with
    [] -> accu
  | a::l -> fold_left l (f accu a) f 
  
let reduce_from_left lst fn = 
  match lst with 
  | first :: rest ->  fold_left rest first fn 
  | _ -> invalid_arg "Ext_list.reduce_from_left"

let rec fold_left2 l1 l2 accu f =
  match (l1, l2) with
    ([], []) -> accu
  | (a1::l1, a2::l2) -> fold_left2  l1 l2 (f a1 a2 accu) f 
  | (_, _) -> invalid_arg "Ext_list.fold_left2"

let singleton_exn xs = match xs with [x] -> x | _ -> assert false

let rec mem_string (xs : string list) (x : string) = 
  match xs with 
    [] -> false
  | a::l ->  a = x  || mem_string l x

end
module Ext_bytes : sig 
#1 "ext_bytes.mli"
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





external unsafe_blit_string : string -> int -> bytes -> int -> int -> unit
                     = "caml_blit_string" 
[@@noalloc]
    



end = struct
#1 "ext_bytes.ml"
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







external unsafe_blit_string : string -> int -> bytes -> int -> int -> unit
                     = "caml_blit_string" 
[@@noalloc]                     


end
module Ext_string : sig 
#1 "ext_string.mli"
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








(** Extension to the standard library [String] module, fixed some bugs like
    avoiding locale sensitivity *) 

(** default is false *)    
val split_by : ?keep_empty:bool -> (char -> bool) -> string -> string list


(** remove whitespace letters ('\t', '\n', ' ') on both side*)
val trim : string -> string 


(** default is false *)
val split : ?keep_empty:bool -> string -> char -> string list

(** split by space chars for quick scripting *)
val quick_split_by_ws : string -> string list 



val starts_with : string -> string -> bool

(**
   return [-1] when not found, the returned index is useful 
   see [ends_with_then_chop]
*)
val ends_with_index : string -> string -> int

val ends_with : string -> string -> bool

(**
  [ends_with_then_chop name ext]
  @example:
   {[
     ends_with_then_chop "a.cmj" ".cmj"
     "a"
   ]}
   This is useful in controlled or file case sensitve system
*)
val ends_with_then_chop : string -> string -> string option




(**
  [for_all_from  s start p]
  if [start] is negative, it raises,
  if [start] is too large, it returns true
*)
val for_all_from:
  string -> 
  int -> 
  (char -> bool) -> 
  bool 

val for_all : 
  string -> 
  (char -> bool) -> 
  bool

val is_empty : string -> bool

val repeat : int -> string -> string 

val equal : string -> string -> bool

(**
  [extract_until s cursor sep]
   When [sep] not found, the cursor is updated to -1,
   otherwise cursor is increased to 1 + [sep_position]
   User can not determine whether it is found or not by
   telling the return string is empty since 
   "\n\n" would result in an empty string too.
*)
(* val extract_until:
  string -> 
  int ref -> (* cursor to be updated *)
  char -> 
  string *)

val index_count:  
  string -> 
  int ->
  char -> 
  int -> 
  int 

(* val index_next :
  string -> 
  int ->
  char -> 
  int  *)

  
(**
  [find ~start ~sub s]
  returns [-1] if not found
*)
val find : ?start:int -> sub:string -> string -> int

val contain_substring : string -> string -> bool 

val non_overlap_count : sub:string -> string -> int 

val rfind : sub:string -> string -> int

(** [tail_from s 1]
  return a substring from offset 1 (inclusive)
*)
val tail_from : string -> int -> string


(** returns negative number if not found *)
val rindex_neg : string -> char -> int 

val rindex_opt : string -> char -> int option


val no_char : string -> char -> int -> int -> bool 


val no_slash : string -> bool 

(** return negative means no slash, otherwise [i] means the place for first slash *)
val no_slash_idx : string -> int 

val no_slash_idx_from : string -> int -> int 

(** if no conversion happens, reference equality holds *)
val replace_slash_backward : string -> string 

(** if no conversion happens, reference equality holds *)
val replace_backward_slash : string -> string 

val empty : string 


external compare : string -> string -> int = "caml_string_length_based_compare" [@@noalloc];;  
  
val single_space : string

val concat3 : string -> string -> string -> string 
val concat4 : string -> string -> string -> string -> string 
val concat5 : string -> string -> string -> string -> string -> string  
val inter2 : string -> string -> string
val inter3 : string -> string -> string -> string 
val inter4 : string -> string -> string -> string -> string
val concat_array : string -> string array -> string 

val single_colon : string 

val parent_dir_lit : string
val current_dir_lit : string

val capitalize_ascii : string -> string

val capitalize_sub:
  string -> 
  int -> 
  string
  
val uncapitalize_ascii : string -> string

val lowercase_ascii : string -> string 

(** Play parity to {!Ext_buffer.add_int_1} *)
val get_int_1 : string -> int -> int 
val get_int_2 : string -> int -> int 
val get_int_3 : string -> int -> int 
val get_int_4 : string -> int -> int 

val get_1_2_3_4 : 
  string -> 
  off:int ->  
  int -> 
  int 

val unsafe_sub :   
  string -> 
  int -> 
  int -> 
  string
end = struct
#1 "ext_string.ml"
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







(*
   {[ split " test_unsafe_obj_ffi_ppx.cmi" ~keep_empty:false ' ']}
*)
let split_by ?(keep_empty=false) is_delim str =
  let len = String.length str in
  let rec loop acc last_pos pos =
    if pos = -1 then
      if last_pos = 0 && not keep_empty then

        acc
      else 
        String.sub str 0 last_pos :: acc
    else
    if is_delim str.[pos] then
      let new_len = (last_pos - pos - 1) in
      if new_len <> 0 || keep_empty then 
        let v = String.sub str (pos + 1) new_len in
        loop ( v :: acc)
          pos (pos - 1)
      else loop acc pos (pos - 1)
    else loop acc last_pos (pos - 1)
  in
  loop [] len (len - 1)

let trim s = 
  let i = ref 0  in
  let j = String.length s in 
  while !i < j &&  
        let u = String.unsafe_get s !i in 
        u = '\t' || u = '\n' || u = ' ' 
  do 
    incr i;
  done;
  let k = ref (j - 1)  in 
  while !k >= !i && 
        let u = String.unsafe_get s !k in 
        u = '\t' || u = '\n' || u = ' ' do 
    decr k ;
  done;
  String.sub s !i (!k - !i + 1)

let split ?keep_empty  str on = 
  if str = "" then [] else 
    split_by ?keep_empty (fun x -> (x : char) = on) str  ;;

let quick_split_by_ws str : string list = 
  split_by ~keep_empty:false (fun x -> x = '\t' || x = '\n' || x = ' ') str

let starts_with s beg = 
  let beg_len = String.length beg in
  let s_len = String.length s in
  beg_len <=  s_len &&
  (let i = ref 0 in
   while !i <  beg_len 
         && String.unsafe_get s !i =
            String.unsafe_get beg !i do 
     incr i 
   done;
   !i = beg_len
  )

let rec ends_aux s end_ j k = 
  if k < 0 then (j + 1)
  else if String.unsafe_get s j = String.unsafe_get end_ k then 
    ends_aux s end_ (j - 1) (k - 1)
  else  -1   

(** return an index which is minus when [s] does not 
    end with [beg]
*)
let ends_with_index s end_ : int = 
  let s_finish = String.length s - 1 in
  let s_beg = String.length end_ - 1 in
  if s_beg > s_finish then -1
  else
    ends_aux s end_ s_finish s_beg

let ends_with s end_ = ends_with_index s end_ >= 0 

let ends_with_then_chop s beg = 
  let i =  ends_with_index s beg in 
  if i >= 0 then Some (String.sub s 0 i) 
  else None

(* let check_suffix_case = ends_with  *)
(* let check_suffix_case_then_chop = ends_with_then_chop *)

(* let check_any_suffix_case s suffixes = 
  Ext_list.exists suffixes (fun x -> check_suffix_case s x)  *)

(* let check_any_suffix_case_then_chop s suffixes = 
  let rec aux suffixes = 
    match suffixes with 
    | [] -> None 
    | x::xs -> 
      let id = ends_with_index s x in 
      if id >= 0 then Some (String.sub s 0 id)
      else aux xs in 
  aux suffixes     *)




(* it is unsafe to expose such API as unsafe since 
   user can provide bad input range 

*)
let rec unsafe_for_all_range s ~start ~finish p =     
  start > finish ||
  p (String.unsafe_get s start) && 
  unsafe_for_all_range s ~start:(start + 1) ~finish p

let for_all_from s start  p = 
  let len = String.length s in 
  if start < 0  then invalid_arg "Ext_string.for_all_from"
  else unsafe_for_all_range s ~start ~finish:(len - 1) p 


let for_all s (p : char -> bool)  =   
  unsafe_for_all_range s ~start:0  ~finish:(String.length s - 1) p 

let is_empty s = String.length s = 0


let repeat n s  =
  let len = String.length s in
  let res = Bytes.create(n * len) in
  for i = 0 to pred n do
    String.blit s 0 res (i * len) len
  done;
  Bytes.to_string res




let unsafe_is_sub ~sub i s j ~len =
  let rec check k =
    if k = len
    then true
    else 
      String.unsafe_get sub (i+k) = 
      String.unsafe_get s (j+k) && check (k+1)
  in
  j+len <= String.length s && check 0



let find ?(start=0) ~sub s =
  let exception Local_exit in
  let n = String.length sub in
  let s_len = String.length s in 
  let i = ref start in  
  try
    while !i + n <= s_len do
      if unsafe_is_sub ~sub 0 s !i ~len:n then
        raise_notrace Local_exit;
      incr i
    done;
    -1
  with Local_exit ->
    !i

let contain_substring s sub = 
  find s ~sub >= 0 

(** TODO: optimize 
    avoid nonterminating when string is empty 
*)
let non_overlap_count ~sub s = 
  let sub_len = String.length sub in 
  let rec aux  acc off = 
    let i = find ~start:off ~sub s  in 
    if i < 0 then acc 
    else aux (acc + 1) (i + sub_len) in
  if String.length sub = 0 then invalid_arg "Ext_string.non_overlap_count"
  else aux 0 0  


let rfind ~sub s =
  let exception Local_exit in   
  let n = String.length sub in
  let i = ref (String.length s - n) in
  try
    while !i >= 0 do
      if unsafe_is_sub ~sub 0 s !i ~len:n then 
        raise_notrace Local_exit;
      decr i
    done;
    -1
  with Local_exit ->
    !i

let tail_from s x = 
  let len = String.length s  in 
  if  x > len then invalid_arg ("Ext_string.tail_from " ^s ^ " : "^ string_of_int x )
  else String.sub s x (len - x)

let equal (x : string) y  = x = y

(* let rec index_rec s lim i c =
  if i >= lim then -1 else
  if String.unsafe_get s i = c then i 
  else index_rec s lim (i + 1) c *)



let rec index_rec_count s lim i c count =
  if i >= lim then -1 else
  if String.unsafe_get s i = c then 
    if count = 1 then i 
    else index_rec_count s lim (i + 1) c (count - 1)
  else index_rec_count s lim (i + 1) c count

let index_count s i c count =     
  let lim = String.length s in 
  if i < 0 || i >= lim || count < 1 then 
    invalid_arg ("index_count: ( " ^string_of_int i ^ "," ^string_of_int count ^ ")" );
  index_rec_count s lim i c count 

(* let index_next s i c =   
  index_count s i c 1  *)

(* let extract_until s cursor c =       
  let len = String.length s in   
  let start = !cursor in 
  if start < 0 || start >= len then (
    cursor := -1;
    ""
    )
  else 
    let i = index_rec s len start c in   
    let finish = 
      if i < 0 then (      
        cursor := -1 ;
        len 
      )
      else (
        cursor := i + 1;
        i 
      ) in 
    String.sub s start (finish - start) *)
  
let rec rindex_rec s i c =
  if i < 0 then i else
  if String.unsafe_get s i = c then i else rindex_rec s (i - 1) c;;

let rec rindex_rec_opt s i c =
  if i < 0 then None else
  if String.unsafe_get s i = c then Some i else rindex_rec_opt s (i - 1) c;;

let rindex_neg s c = 
  rindex_rec s (String.length s - 1) c;;

let rindex_opt s c = 
  rindex_rec_opt s (String.length s - 1) c;;


(** TODO: can be improved to return a positive integer instead *)
let rec unsafe_no_char x ch i  last_idx = 
  i > last_idx  || 
  (String.unsafe_get x i <> ch && unsafe_no_char x ch (i + 1)  last_idx)

let rec unsafe_no_char_idx x ch i last_idx = 
  if i > last_idx  then -1 
  else 
  if String.unsafe_get x i <> ch then 
    unsafe_no_char_idx x ch (i + 1)  last_idx
  else i

let no_char x ch i len  : bool =
  let str_len = String.length x in 
  if i < 0 || i >= str_len || len >= str_len then invalid_arg "Ext_string.no_char"   
  else unsafe_no_char x ch i len 


let no_slash x = 
  unsafe_no_char x '/' 0 (String.length x - 1)

let no_slash_idx x = 
  unsafe_no_char_idx x '/' 0 (String.length x - 1)

let no_slash_idx_from x from = 
  let last_idx = String.length x - 1  in 
  assert (from >= 0); 
  unsafe_no_char_idx x '/' from last_idx

let replace_slash_backward (x : string ) = 
  let len = String.length x in 
  if unsafe_no_char x '/' 0  (len - 1) then x 
  else 
    String.map (function 
        | '/' -> '\\'
        | x -> x ) x 

let replace_backward_slash (x : string)=
  let len = String.length x in
  if unsafe_no_char x '\\' 0  (len -1) then x 
  else  
    String.map (function 
        |'\\'-> '/'
        | x -> x) x

let empty = ""


external compare : string -> string -> int = "caml_string_length_based_compare" [@@noalloc];;    

let single_space = " "
let single_colon = ":"

let concat_array sep (s : string array) =   
  let s_len = Array.length s in 
  match s_len with 
  | 0 -> empty 
  | 1 -> Array.unsafe_get s 0
  | _ ->     
    let sep_len = String.length sep in 
    let len = ref 0 in 
    for i = 0 to  s_len - 1 do 
      len := !len + String.length (Array.unsafe_get s i)
    done;
    let target = 
      Bytes.create 
        (!len + (s_len - 1) * sep_len ) in    
    let hd = (Array.unsafe_get s 0) in     
    let hd_len = String.length hd in 
    String.unsafe_blit hd  0  target 0 hd_len;   
    let current_offset = ref hd_len in     
    for i = 1 to s_len - 1 do 
      String.unsafe_blit sep 0 target  !current_offset sep_len;
      let cur = Array.unsafe_get s i in 
      let cur_len = String.length cur in     
      let new_off_set = (!current_offset + sep_len ) in
      String.unsafe_blit cur 0 target new_off_set cur_len; 
      current_offset := 
        new_off_set + cur_len ; 
    done;
    Bytes.unsafe_to_string target   

let concat3 a b c = 
  let a_len = String.length a in 
  let b_len = String.length b in 
  let c_len = String.length c in 
  let len = a_len + b_len + c_len in 
  let target = Bytes.create len in 
  String.unsafe_blit a 0 target 0 a_len ; 
  String.unsafe_blit b 0 target a_len b_len;
  String.unsafe_blit c 0 target (a_len + b_len) c_len;
  Bytes.unsafe_to_string target

let concat4 a b c d =
  let a_len = String.length a in 
  let b_len = String.length b in 
  let c_len = String.length c in 
  let d_len = String.length d in 
  let len = a_len + b_len + c_len + d_len in 

  let target = Bytes.create len in 
  String.unsafe_blit a 0 target 0 a_len ; 
  String.unsafe_blit b 0 target a_len b_len;
  String.unsafe_blit c 0 target (a_len + b_len) c_len;
  String.unsafe_blit d 0 target (a_len + b_len + c_len) d_len;
  Bytes.unsafe_to_string target


let concat5 a b c d e =
  let a_len = String.length a in 
  let b_len = String.length b in 
  let c_len = String.length c in 
  let d_len = String.length d in 
  let e_len = String.length e in 
  let len = a_len + b_len + c_len + d_len + e_len in 

  let target = Bytes.create len in 
  String.unsafe_blit a 0 target 0 a_len ; 
  String.unsafe_blit b 0 target a_len b_len;
  String.unsafe_blit c 0 target (a_len + b_len) c_len;
  String.unsafe_blit d 0 target (a_len + b_len + c_len) d_len;
  String.unsafe_blit e 0 target (a_len + b_len + c_len + d_len) e_len;
  Bytes.unsafe_to_string target



let inter2 a b = 
  concat3 a single_space b 


let inter3 a b c = 
  concat5 a  single_space  b  single_space  c 





let inter4 a b c d =
  concat_array single_space [| a; b ; c; d|]


let parent_dir_lit = ".."    
let current_dir_lit = "."


(* reference {!Bytes.unppercase} *)
let capitalize_ascii (s : string) : string = 
  if String.length s = 0 then s 
  else 
    begin
      let c = String.unsafe_get s 0 in 
      if (c >= 'a' && c <= 'z')
      || (c >= '\224' && c <= '\246')
      || (c >= '\248' && c <= '\254') then 
        let uc = Char.unsafe_chr (Char.code c - 32) in 
        let bytes = Bytes.of_string s in
        Bytes.unsafe_set bytes 0 uc;
        Bytes.unsafe_to_string bytes 
      else s 
    end

let capitalize_sub (s : string) len : string = 
  let slen = String.length s in 
  if  len < 0 || len > slen then invalid_arg "Ext_string.capitalize_sub"
  else 
  if len = 0 then ""
  else 
    let bytes = Bytes.create len in 
    let uc = 
      let c = String.unsafe_get s 0 in 
      if (c >= 'a' && c <= 'z')
      || (c >= '\224' && c <= '\246')
      || (c >= '\248' && c <= '\254') then 
        Char.unsafe_chr (Char.code c - 32) else c in 
    Bytes.unsafe_set bytes 0 uc;
    for i = 1 to len - 1 do 
      Bytes.unsafe_set bytes i (String.unsafe_get s i)
    done ;
    Bytes.unsafe_to_string bytes 

    

let uncapitalize_ascii =
    String.uncapitalize_ascii

let lowercase_ascii = String.lowercase_ascii



let get_int_1 (x : string) off : int = 
  Char.code x.[off]

let get_int_2 (x : string) off : int = 
  Char.code x.[off] lor   
  Char.code x.[off+1] lsl 8
  
let get_int_3 (x : string) off : int = 
  Char.code x.[off] lor   
  Char.code x.[off+1] lsl 8  lor 
  Char.code x.[off+2] lsl 16

let get_int_4 (x : string) off : int =   
  Char.code x.[off] lor   
  Char.code x.[off+1] lsl 8  lor 
  Char.code x.[off+2] lsl 16 lor
  Char.code x.[off+3] lsl 24 

let get_1_2_3_4 (x : string) ~off len : int =  
  if len = 1 then get_int_1 x off 
  else if len = 2 then get_int_2 x off 
  else if len = 3 then get_int_3 x off 
  else if len = 4 then get_int_4 x off 
  else assert false

let unsafe_sub  x offs len =
  let b = Bytes.create len in 
  Ext_bytes.unsafe_blit_string x offs b 0 len;
  (Bytes.unsafe_to_string b);
end
module Map_gen
= struct
#1 "map_gen.ml"
(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)
(** adapted from stdlib *)

type ('key,'a) t =
  | Empty
  | Node of ('key,'a) t * 'key * 'a * ('key,'a) t * int

type ('key,'a) enumeration =
  | End
  | More of 'key * 'a * ('key,'a) t * ('key, 'a) enumeration

let rec cardinal_aux acc  = function
  | Empty -> acc 
  | Node (l,_,_,r, _) -> 
    cardinal_aux  (cardinal_aux (acc + 1)  r ) l 

let cardinal s = cardinal_aux 0 s 

let rec bindings_aux accu = function
  | Empty -> accu
  | Node(l, v, d, r, _) -> bindings_aux ((v, d) :: bindings_aux accu r) l

let bindings s =
  bindings_aux [] s

let rec fill_array_with_f (s : _ t) i arr  f : int =    
  match s with 
  | Empty -> i 
  | Node ( l ,k,v,r,_) -> 
    let inext = fill_array_with_f l i arr f in 
    Array.unsafe_set arr inext (f k v);
    fill_array_with_f r (inext + 1) arr f

let rec fill_array_aux (s : _ t) i arr : int =    
  match s with 
  | Empty -> i 
  | Node (l,k,v,r,_) -> 
    let inext = fill_array_aux l i arr in 
    Array.unsafe_set arr inext (k,v);
    fill_array_aux r (inext + 1) arr 


let to_sorted_array (s : ('key,'a) t)  : ('key * 'a ) array =    
  match s with 
  | Empty -> [||]
  | Node(l,k,v,r,_) -> 
    let len = 
      cardinal_aux (cardinal_aux 1 r) l in 
    let arr =
      Array.make len (k,v) in  
    ignore (fill_array_aux s 0 arr : int);
    arr 

let to_sorted_array_with_f (type key a b ) (s : (key,a) t)  (f : key -> a -> b): b array =    
  match s with 
  | Empty -> [||]
  | Node(l,k,v,r,_) -> 
    let len = 
      cardinal_aux (cardinal_aux 1 r) l in 
    let arr =
      Array.make len (f k v) in  
    ignore (fill_array_with_f s 0 arr f: int);
    arr     

let rec keys_aux accu = function
    Empty -> accu
  | Node(l, v, _, r, _) -> keys_aux (v :: keys_aux accu r) l

let keys s = keys_aux [] s



let rec cons_enum m e =
  match m with
    Empty -> e
  | Node(l, v, d, r, _) -> cons_enum l (More(v, d, r, e))


let height = function
  | Empty -> 0
  | Node(_,_,_,_,h) -> h

let create l x d r =
  let hl = height l and hr = height r in
  Node(l, x, d, r, (if hl >= hr then hl + 1 else hr + 1))

let singleton x d = Node(Empty, x, d, Empty, 1)

let bal l x d r =
  let hl = match l with Empty -> 0 | Node(_,_,_,_,h) -> h in
  let hr = match r with Empty -> 0 | Node(_,_,_,_,h) -> h in
  if hl > hr + 2 then begin
    match l with
      Empty -> invalid_arg "Map.bal"
    | Node(ll, lv, ld, lr, _) ->
      if height ll >= height lr then
        create ll lv ld (create lr x d r)
      else begin
        match lr with
          Empty -> invalid_arg "Map.bal"
        | Node(lrl, lrv, lrd, lrr, _)->
          create (create ll lv ld lrl) lrv lrd (create lrr x d r)
      end
  end else if hr > hl + 2 then begin
    match r with
      Empty -> invalid_arg "Map.bal"
    | Node(rl, rv, rd, rr, _) ->
      if height rr >= height rl then
        create (create l x d rl) rv rd rr
      else begin
        match rl with
          Empty -> invalid_arg "Map.bal"
        | Node(rll, rlv, rld, rlr, _) ->
          create (create l x d rll) rlv rld (create rlr rv rd rr)
      end
  end else
    Node(l, x, d, r, (if hl >= hr then hl + 1 else hr + 1))

let empty = Empty

let is_empty = function Empty -> true | _ -> false

let rec min_binding_exn = function
    Empty -> raise Not_found
  | Node(Empty, x, d, _, _) -> (x, d)
  | Node(l, _, _, _, _) -> min_binding_exn l

let choose = min_binding_exn

let rec max_binding_exn = function
    Empty -> raise Not_found
  | Node(_, x, d, Empty, _) -> (x, d)
  | Node(_, _, _, r, _) -> max_binding_exn r

let rec remove_min_binding = function
    Empty -> invalid_arg "Map.remove_min_elt"
  | Node(Empty, _, _, r, _) -> r
  | Node(l, x, d, r, _) -> bal (remove_min_binding l) x d r

let merge t1 t2 =
  match (t1, t2) with
    (Empty, t) -> t
  | (t, Empty) -> t
  | (_, _) ->
    let (x, d) = min_binding_exn t2 in
    bal t1 x d (remove_min_binding t2)


let rec iter x f = match x with 
    Empty -> ()
  | Node(l, v, d, r, _) ->
    iter l f; f v d; iter r f

let rec map x f = match x with
    Empty ->
    Empty
  | Node(l, v, d, r, h) ->
    let l' = map l f in
    let d' = f d in
    let r' = map r f in
    Node(l', v, d', r', h)

let rec mapi x f = match x with
    Empty ->
    Empty
  | Node(l, v, d, r, h) ->
    let l' = mapi l f in
    let d' = f v d in
    let r' = mapi r f in
    Node(l', v, d', r', h)

let rec fold m accu f =
  match m with
    Empty -> accu
  | Node(l, v, d, r, _) ->
    fold r (f v d (fold l accu f)) f 

let rec for_all x p = match x with 
    Empty -> true
  | Node(l, v, d, r, _) -> p v d && for_all l p && for_all r p

let rec exists x p = match x with
    Empty -> false
  | Node(l, v, d, r, _) -> p v d || exists l p || exists r p

(* Beware: those two functions assume that the added k is *strictly*
   smaller (or bigger) than all the present keys in the tree; it
   does not test for equality with the current min (or max) key.

   Indeed, they are only used during the "join" operation which
   respects this precondition.
*)

let rec add_min_binding k v = function
  | Empty -> singleton k v
  | Node (l, x, d, r, _) ->
    bal (add_min_binding k v l) x d r

let rec add_max_binding k v = function
  | Empty -> singleton k v
  | Node (l, x, d, r, _) ->
    bal l x d (add_max_binding k v r)

(* Same as create and bal, but no assumptions are made on the
   relative heights of l and r. *)

let rec join l v d r =
  match (l, r) with
    (Empty, _) -> add_min_binding v d r
  | (_, Empty) -> add_max_binding v d l
  | (Node(ll, lv, ld, lr, lh), Node(rl, rv, rd, rr, rh)) ->
    if lh > rh + 2 then bal ll lv ld (join lr v d r) else
    if rh > lh + 2 then bal (join l v d rl) rv rd rr else
      create l v d r

(* Merge two trees l and r into one.
   All elements of l must precede the elements of r.
   No assumption on the heights of l and r. *)

let concat t1 t2 =
  match (t1, t2) with
    (Empty, t) -> t
  | (t, Empty) -> t
  | (_, _) ->
    let (x, d) = min_binding_exn t2 in
    join t1 x d (remove_min_binding t2)

let concat_or_join t1 v d t2 =
  match d with
  | Some d -> join t1 v d t2
  | None -> concat t1 t2

let rec filter x p = match x with
    Empty -> Empty
  | Node(l, v, d, r, _) ->
    (* call [p] in the expected left-to-right order *)
    let l' = filter l p in
    let pvd = p v d in
    let r' = filter r p in
    if pvd then join l' v d r' else concat l' r'

let rec partition x p = match x with
    Empty -> (Empty, Empty)
  | Node(l, v, d, r, _) ->
    (* call [p] in the expected left-to-right order *)
    let (lt, lf) = partition l p in
    let pvd = p v d in
    let (rt, rf) = partition r p in
    if pvd
    then (join lt v d rt, concat lf rf)
    else (concat lt rt, join lf v d rf)

let compare compare_key cmp_val m1 m2 =
  let rec compare_aux e1  e2 =
    match (e1, e2) with
      (End, End) -> 0
    | (End, _)  -> -1
    | (_, End) -> 1
    | (More(v1, d1, r1, e1), More(v2, d2, r2, e2)) ->
      let c = compare_key v1 v2 in
      if c <> 0 then c else
        let c = cmp_val d1 d2 in
        if c <> 0 then c else
          compare_aux (cons_enum r1 e1) (cons_enum r2 e2)
  in compare_aux (cons_enum m1 End) (cons_enum m2 End)

let equal compare_key cmp m1 m2 =
  let rec equal_aux e1 e2 =
    match (e1, e2) with
      (End, End) -> true
    | (End, _)  -> false
    | (_, End) -> false
    | (More(v1, d1, r1, e1), More(v2, d2, r2, e2)) ->
      compare_key v1 v2 = 0 && cmp d1 d2 &&
      equal_aux (cons_enum r1 e1) (cons_enum r2 e2)
  in equal_aux (cons_enum m1 End) (cons_enum m2 End)



    
module type S =
  sig
    type key
    type +'a t
    val empty: 'a t
    val compare_key: key -> key -> int 
    val is_empty: 'a t -> bool
    val mem: 'a t -> key -> bool
    val to_sorted_array : 
      'a t -> (key * 'a ) array
    val to_sorted_array_with_f : 
      'a t -> (key -> 'a -> 'b) -> 'b array  
    val add: 'a t -> key -> 'a -> 'a t
    (** [add x y m] 
        If [x] was already bound in [m], its previous binding disappears. *)
    val adjust: 'a t -> key -> ('a option->  'a) ->  'a t 
    (** [adjust acc k replace ] if not exist [add (replace None ], otherwise 
        [add k v (replace (Some old))]
    *)
    val singleton: key -> 'a -> 'a t

    val remove: 'a t -> key -> 'a t
    (** [remove x m] returns a map containing the same bindings as
       [m], except for [x] which is unbound in the returned map. *)

    val merge:
         'a t -> 'b t ->
         (key -> 'a option -> 'b option -> 'c option) ->  'c t
    (** [merge f m1 m2] computes a map whose keys is a subset of keys of [m1]
        and of [m2]. The presence of each such binding, and the corresponding
        value, is determined with the function [f].
        @since 3.12.0
     *)

    val disjoint_merge : 'a t -> 'a t -> 'a t
     (* merge two maps, will raise if they have the same key *)
    val compare: 'a t -> 'a t -> ('a -> 'a -> int) -> int
    (** Total ordering between maps.  The first argument is a total ordering
        used to compare data associated with equal keys in the two maps. *)

    val equal: 'a t -> 'a t -> ('a -> 'a -> bool) ->  bool

    val iter: 'a t -> (key -> 'a -> unit) ->  unit
    (** [iter f m] applies [f] to all bindings in map [m].
        The bindings are passed to [f] in increasing order. *)

    val fold: 'a t -> 'b -> (key -> 'a -> 'b -> 'b) -> 'b
    (** [fold f m a] computes [(f kN dN ... (f k1 d1 a)...)],
       where [k1 ... kN] are the keys of all bindings in [m]
       (in increasing order) *)

    val for_all: 'a t -> (key -> 'a -> bool) -> bool
    (** [for_all p m] checks if all the bindings of the map.
        order unspecified
     *)

    val exists: 'a t -> (key -> 'a -> bool) -> bool
    (** [exists p m] checks if at least one binding of the map
        satisfy the predicate [p]. 
        order unspecified
     *)

    val filter: 'a t -> (key -> 'a -> bool) -> 'a t
    (** [filter p m] returns the map with all the bindings in [m]
        that satisfy predicate [p].
        order unspecified
     *)

    val partition: 'a t -> (key -> 'a -> bool) ->  'a t * 'a t
    (** [partition p m] returns a pair of maps [(m1, m2)], where
        [m1] contains all the bindings of [s] that satisfy the
        predicate [p], and [m2] is the map with all the bindings of
        [s] that do not satisfy [p].
     *)

    val cardinal: 'a t -> int
    (** Return the number of bindings of a map. *)

    val bindings: 'a t -> (key * 'a) list
    (** Return the list of all bindings of the given map.
       The returned list is sorted in increasing order with respect
       to the ordering *)
    val keys : 'a t -> key list 
    (* Increasing order *)

    val min_binding_exn: 'a t -> (key * 'a)
    (** raise [Not_found] if the map is empty. *)

    val max_binding_exn: 'a t -> (key * 'a)
    (** Same as {!Map.S.min_binding} *)

    val choose: 'a t -> (key * 'a)
    (** Return one binding of the given map, or raise [Not_found] if
       the map is empty. Which binding is chosen is unspecified,
       but equal bindings will be chosen for equal maps.
     *)

    val split: 'a t -> key -> 'a t * 'a option * 'a t
    (** [split x m] returns a triple [(l, data, r)], where
          [l] is the map with all the bindings of [m] whose key
        is strictly less than [x];
          [r] is the map with all the bindings of [m] whose key
        is strictly greater than [x];
          [data] is [None] if [m] contains no binding for [x],
          or [Some v] if [m] binds [v] to [x].
        @since 3.12.0
     *)

    val find_exn: 'a t -> key ->  'a
    (** [find x m] returns the current binding of [x] in [m],
       or raises [Not_found] if no such binding exists. *)
    val find_opt:  'a t ->  key ->'a option
    val find_default: 'a t -> key  ->  'a  -> 'a 
    val map: 'a t -> ('a -> 'b) -> 'b t
    (** [map f m] returns a map with same domain as [m], where the
       associated value [a] of all bindings of [m] has been
       replaced by the result of the application of [f] to [a].
       The bindings are passed to [f] in increasing order
       with respect to the ordering over the type of the keys. *)

    val mapi: 'a t ->  (key -> 'a -> 'b) -> 'b t
    (** Same as {!Map.S.map}, but the function receives as arguments both the
       key and the associated value for each binding of the map. *)

    val of_list : (key * 'a) list -> 'a t 
    val of_array : (key * 'a ) array -> 'a t 
    val add_list : (key * 'b) list -> 'b t -> 'b t

  end

end
module Map_string : sig 
#1 "map_string.mli"
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


include Map_gen.S with type key = string

end = struct
#1 "map_string.ml"

# 2 "ext/map.cppo.ml"
(* we don't create [map_poly], since some operations require raise an exception which carries [key] *)


  
# 10 "ext/map.cppo.ml"
  type key = string 
  let compare_key = Ext_string.compare

# 22 "ext/map.cppo.ml"
type 'a t = (key,'a) Map_gen.t
exception Duplicate_key of key 

let empty = Map_gen.empty 
let is_empty = Map_gen.is_empty
let iter = Map_gen.iter
let fold = Map_gen.fold
let for_all = Map_gen.for_all 
let exists = Map_gen.exists 
let singleton = Map_gen.singleton 
let cardinal = Map_gen.cardinal
let bindings = Map_gen.bindings
let to_sorted_array = Map_gen.to_sorted_array
let to_sorted_array_with_f = Map_gen.to_sorted_array_with_f
let keys = Map_gen.keys
let choose = Map_gen.choose 
let partition = Map_gen.partition 
let filter = Map_gen.filter 
let map = Map_gen.map 
let mapi = Map_gen.mapi
let bal = Map_gen.bal 
let height = Map_gen.height 
let max_binding_exn = Map_gen.max_binding_exn
let min_binding_exn = Map_gen.min_binding_exn


let rec add (tree : _ Map_gen.t as 'a) x data  : 'a = match tree with 
  | Empty ->
    Node(Empty, x, data, Empty, 1)
  | Node(l, v, d, r, h) ->
    let c = compare_key x v in
    if c = 0 then
      Node(l, x, data, r, h)
    else if c < 0 then
      bal (add l x data ) v d r
    else
      bal l v d (add r x data )


let rec adjust (tree : _ Map_gen.t as 'a) x replace  : 'a = 
  match tree with 
  | Empty ->
    Node(Empty, x, replace None, Empty, 1)
  | Node(l, v, d, r, h) ->
    let c = compare_key x v in
    if c = 0 then
      Node(l, x, replace  (Some d) , r, h)
    else if c < 0 then
      bal (adjust l x  replace ) v d r
    else
      bal l v d (adjust r x  replace )


let rec find_exn (tree : _ Map_gen.t ) x = match tree with 
  | Empty ->
    raise Not_found
  | Node(l, v, d, r, _) ->
    let c = compare_key x v in
    if c = 0 then d
    else find_exn (if c < 0 then l else r) x

let rec find_opt (tree : _ Map_gen.t ) x = match tree with 
  | Empty -> None 
  | Node(l, v, d, r, _) ->
    let c = compare_key x v in
    if c = 0 then Some d
    else find_opt (if c < 0 then l else r) x

let rec find_default (tree : _ Map_gen.t ) x  default     = match tree with 
  | Empty -> default  
  | Node(l, v, d, r, _) ->
    let c = compare_key x v in
    if c = 0 then  d
    else find_default (if c < 0 then l else r) x default

let rec mem (tree : _ Map_gen.t )  x= match tree with 
  | Empty ->
    false
  | Node(l, v, _, r, _) ->
    let c = compare_key x v in
    c = 0 || mem (if c < 0 then l else r) x 

let rec remove (tree : _ Map_gen.t as 'a) x : 'a = match tree with 
  | Empty ->
    Empty
  | Node(l, v, d, r, _) ->
    let c = compare_key x v in
    if c = 0 then
      Map_gen.merge l r
    else if c < 0 then
      bal (remove l x) v d r
    else
      bal l v d (remove r x )


let rec split (tree : _ Map_gen.t as 'a) x : 'a * _ option * 'a  = match tree with 
  | Empty ->
    (Empty, None, Empty)
  | Node(l, v, d, r, _) ->
    let c = compare_key x v in
    if c = 0 then (l, Some d, r)
    else if c < 0 then
      let (ll, pres, rl) = split l x in (ll, pres, Map_gen.join rl v d r)
    else
      let (lr, pres, rr) = split r x in (Map_gen.join l v d lr, pres, rr)

let rec merge (s1 : _ Map_gen.t) (s2  : _ Map_gen.t) f  : _ Map_gen.t =
  match (s1, s2) with
  | (Empty, Empty) -> Empty
  | (Node (l1, v1, d1, r1, h1), _) when h1 >= height s2 ->
    let (l2, d2, r2) = split s2 v1 in
    Map_gen.concat_or_join (merge l1 l2 f) v1 (f v1 (Some d1) d2) (merge r1 r2 f)
  | (_, Node (l2, v2, d2, r2, _)) ->
    let (l1, d1, r1) = split s1 v2 in
    Map_gen.concat_or_join (merge l1 l2 f) v2 (f v2 d1 (Some d2)) (merge r1 r2 f)
  | _ ->
    assert false

let rec disjoint_merge  (s1 : _ Map_gen.t) (s2  : _ Map_gen.t) : _ Map_gen.t =
  match (s1, s2) with
  | (Empty, Empty) -> Empty
  | (Node (l1, v1, d1, r1, h1), _) when h1 >= height s2 ->
    begin match split s2 v1 with 
    | l2, None, r2 -> 
      Map_gen.join (disjoint_merge  l1 l2) v1 d1 (disjoint_merge r1 r2)
    | _, Some _, _ ->
      raise (Duplicate_key  v1)
    end        
  | (_, Node (l2, v2, d2, r2, _)) ->
    begin match  split s1 v2 with 
    | (l1, None, r1) -> 
      Map_gen.join (disjoint_merge  l1 l2) v2 d2 (disjoint_merge  r1 r2)
    | (_, Some _, _) -> 
      raise (Duplicate_key v2)
    end
  | _ ->
    assert false



let compare m1 m2 cmp = Map_gen.compare compare_key cmp m1 m2

let equal m1 m2 cmp = Map_gen.equal compare_key cmp m1 m2 

let add_list (xs : _ list ) init = 
  Ext_list.fold_left xs init (fun  acc (k,v) -> add acc k v )

let of_list xs = add_list xs empty

let of_array xs = 
  Ext_array.fold_left xs empty (fun acc (k,v) -> add acc k v ) 

end
module Bsb_db : sig 
#1 "bsb_db.mli"

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

(** Store a file called [.bsbuild] that can be communicated 
    between [bsb.exe] and [bsb_helper.exe]. 
    [bsb.exe] stores such data which would be retrieved by 
    [bsb_helper.exe]. It is currently used to combine with 
    ocamldep to figure out which module->file it depends on
*) 

type case = bool 

type info = 
  | Mli (* intemediate state *)
  | Ml
  | Ml_mli



type module_info = 
  {
    mutable info : info;
    dir : string;
    is_re : bool;
    case : bool;
    name_sans_extension : string;
  }

type t = module_info Map_string.t 

type ts = t array 

(** store  the meta data indexed by {!Bsb_dir_index}
  {[
    0 --> lib group
    1 --> dev 1 group
    .
    
  ]}
*)






end = struct
#1 "bsb_db.ml"

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

type case = bool
(** true means upper case*)


type info = 
  | Mli (* intemediate state *)
  | Ml
  | Ml_mli
  
type module_info = 
  {
    mutable info : info;
    dir : string ; 
    is_re : bool;
    case : bool;
    name_sans_extension : string  ;
  }


type t = module_info Map_string.t 

type ts = t array 
(** indexed by the group *)






end
module Bsb_dir_index : sig 
#1 "bsb_dir_index.mli"
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

(** Used to index [.bsbuildcache] may not be needed if we flatten dev 
  into  a single group
*)
type t = private int

val lib_dir_index : t 

val is_lib_dir : t -> bool 

val get_dev_index : unit -> t 

val of_int : int -> t 

val get_current_number_of_dev_groups : unit -> int 


val string_of_bsb_dev_include : t -> string 

(** TODO: Need reset
   when generating each ninja file to provide stronger guarantee. 
   Here we get a weak guarantee because only dev group is 
  inside the toplevel project
   *)
val reset : unit -> unit
end = struct
#1 "bsb_dir_index.ml"
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

type t = int 

(** 
   0 : lib 
   1 : dev 1 
   2 : dev 2 
*)  
external of_int : int -> t = "%identity"
let lib_dir_index = 0

let is_lib_dir x = x = lib_dir_index

let dir_index = ref 0 

let get_dev_index ( ) = 
  incr dir_index ; !dir_index

let get_current_number_of_dev_groups =
   (fun () -> !dir_index )


(** bsb generate pre-defined variables [bsc_group_i_includes]
  for each rule, there is variable [bsc_extra_excludes]
  [g_dev_incls] are for app test etc
  it will be like
  {[
    g_dev_incls = ${bsc_group_1_includes}
  ]}
  where [bsc_group_1_includes] will be pre-calcuated
*)
let bsc_group_1_includes = "bsc_group_1_includes"
let bsc_group_2_includes = "bsc_group_2_includes"
let bsc_group_3_includes = "bsc_group_3_includes"
let bsc_group_4_includes = "bsc_group_4_includes"
let string_of_bsb_dev_include i = 
  match i with 
  | 1 -> bsc_group_1_includes 
  | 2 -> bsc_group_2_includes
  | 3 -> bsc_group_3_includes
  | 4 -> bsc_group_4_includes
  | _ -> 
    "bsc_group_" ^ string_of_int i ^ "_includes"


let reset () = dir_index := 0
end
module Set_gen
= struct
#1 "set_gen.ml"
(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)

(** balanced tree based on stdlib distribution *)

type ('a, 'id) t0 = 
  | Empty 
  | Node of ('a, 'id) t0 * 'a * ('a, 'id) t0 * int 

type ('a, 'id) enumeration0 = 
  | End | More of 'a * ('a, 'id) t0 * ('a, 'id) enumeration0


let rec cons_enum s e = 
  match s with 
  | Empty -> e 
  | Node(l,v,r,_) -> cons_enum l (More(v,r,e))

let  height = function
  | Empty -> 0 
  | Node(_,_,_,h) -> h   

(* Smallest and greatest element of a set *)

let rec min_elt = function
    Empty -> raise Not_found
  | Node(Empty, v, _, _) -> v
  | Node(l, _, _, _) -> min_elt l

let rec max_elt = function
    Empty -> raise Not_found
  | Node(_, v, Empty, _) -> v
  | Node(_, _, r, _) -> max_elt r




let empty = Empty

let is_empty = function Empty -> true | _ -> false

let rec cardinal_aux acc  = function
  | Empty -> acc 
  | Node (l,_,r, _) -> 
    cardinal_aux  (cardinal_aux (acc + 1)  r ) l 

let cardinal s = cardinal_aux 0 s 

let rec elements_aux accu = function
  | Empty -> accu
  | Node(l, v, r, _) -> elements_aux (v :: elements_aux accu r) l

let elements s =
  elements_aux [] s

let choose = min_elt

let rec iter  x f = match x with
  | Empty -> ()
  | Node(l, v, r, _) -> iter l f ; f v; iter r f 

let rec fold s accu f =
  match s with
  | Empty -> accu
  | Node(l, v, r, _) -> fold r (f v (fold l accu f)) f 

let rec for_all x p = match x with
  | Empty -> true
  | Node(l, v, r, _) -> p v && for_all l p && for_all r p 

let rec exists x p = match x with
  | Empty -> false
  | Node(l, v, r, _) -> p v || exists l p  || exists r p


let max_int3 (a : int) b c = 
  if a >= b then 
    if a >= c then a 
    else c
  else 
  if b >=c then b
  else c     
let max_int_2 (a : int) b =  
  if a >= b then a else b 



exception Height_invariant_broken
exception Height_diff_borken 

let rec check_height_and_diff = 
  function 
  | Empty -> 0
  | Node(l,_,r,h) -> 
    let hl = check_height_and_diff l in
    let hr = check_height_and_diff r in
    if h <>  max_int_2 hl hr + 1 then raise Height_invariant_broken
    else  
      let diff = (abs (hl - hr)) in  
      if  diff > 2 then raise Height_diff_borken 
      else h     

let check tree = 
  ignore (check_height_and_diff tree)
(* 
    Invariants: 
    1. {[ l < v < r]}
    2. l and r balanced 
    3. [height l] - [height r] <= 2
*)
let create l v r = 
  let hl = match l with Empty -> 0 | Node (_,_,_,h) -> h in
  let hr = match r with Empty -> 0 | Node (_,_,_,h) -> h in
  Node(l,v,r, if hl >= hr then hl + 1 else hr + 1)         

(* Same as create, but performs one step of rebalancing if necessary.
    Invariants:
    1. {[ l < v < r ]}
    2. l and r balanced 
    3. | height l - height r | <= 3.

    Proof by indunction

    Lemma: the height of  [bal l v r] will bounded by [max l r] + 1 
*)
let internal_bal l v r =
  let hl = match l with Empty -> 0 | Node(_,_,_,h) -> h in
  let hr = match r with Empty -> 0 | Node(_,_,_,h) -> h in
  if hl > hr + 2 then begin
    match l with
      Empty -> assert false
    | Node(ll, lv, lr, _) ->   
      if height ll >= height lr then
        (* [ll] >~ [lr] 
           [ll] >~ [r] 
           [ll] ~~ [ lr ^ r]  
        *)
        create ll lv (create lr v r)
      else begin
        match lr with
          Empty -> assert false
        | Node(lrl, lrv, lrr, _)->
          (* [lr] >~ [ll]
             [lr] >~ [r]
             [ll ^ lrl] ~~ [lrr ^ r]   
          *)
          create (create ll lv lrl) lrv (create lrr v r)
      end
  end else if hr > hl + 2 then begin
    match r with
      Empty -> assert false
    | Node(rl, rv, rr, _) ->
      if height rr >= height rl then
        create (create l v rl) rv rr
      else begin
        match rl with
          Empty -> assert false
        | Node(rll, rlv, rlr, _) ->
          create (create l v rll) rlv (create rlr rv rr)
      end
  end else
    Node(l, v, r, (if hl >= hr then hl + 1 else hr + 1))    

let rec remove_min_elt = function
    Empty -> invalid_arg "Set.remove_min_elt"
  | Node(Empty, _, r, _) -> r
  | Node(l, v, r, _) -> internal_bal (remove_min_elt l) v r

let singleton x = Node(Empty, x, Empty, 1)    

(* 
   All elements of l must precede the elements of r.
       Assume | height l - height r | <= 2.
   weak form of [concat] 
*)

let internal_merge l r =
  match (l, r) with
  | (Empty, t) -> t
  | (t, Empty) -> t
  | (_, _) -> internal_bal l (min_elt r) (remove_min_elt r)

(* Beware: those two functions assume that the added v is *strictly*
    smaller (or bigger) than all the present elements in the tree; it
    does not test for equality with the current min (or max) element.
    Indeed, they are only used during the "join" operation which
    respects this precondition.
*)

let rec add_min_element v = function
  | Empty -> singleton v
  | Node (l, x, r, _) ->
    internal_bal (add_min_element v l) x r

let rec add_max_element v = function
  | Empty -> singleton v
  | Node (l, x, r, _) ->
    internal_bal l x (add_max_element v r)

(** 
    Invariants:
    1. l < v < r 
    2. l and r are balanced 

    Proof by induction
    The height of output will be ~~ (max (height l) (height r) + 2)
    Also use the lemma from [bal]
*)
let rec internal_join l v r =
  match (l, r) with
    (Empty, _) -> add_min_element v r
  | (_, Empty) -> add_max_element v l
  | (Node(ll, lv, lr, lh), Node(rl, rv, rr, rh)) ->
    if lh > rh + 2 then 
      (* proof by induction:
         now [height of ll] is [lh - 1] 
      *)
      internal_bal ll lv (internal_join lr v r) 
    else
    if rh > lh + 2 then internal_bal (internal_join l v rl) rv rr 
    else create l v r


(*
    Required Invariants: 
    [t1] < [t2]  
*)
let internal_concat t1 t2 =
  match (t1, t2) with
  | (Empty, t) -> t
  | (t, Empty) -> t
  | (_, _) -> internal_join t1 (min_elt t2) (remove_min_elt t2)

let rec filter x p = match x with 
  | Empty -> Empty
  | Node(l, v, r, _) ->
    (* call [p] in the expected left-to-right order *)
    let l' = filter l p in
    let pv = p v in
    let r' = filter r p in
    if pv then internal_join l' v r' else internal_concat l' r'


let rec partition x p = match x with 
  | Empty -> (Empty, Empty)
  | Node(l, v, r, _) ->
    (* call [p] in the expected left-to-right order *)
    let (lt, lf) = partition l p in
    let pv = p v in
    let (rt, rf) = partition r p in
    if pv
    then (internal_join lt v rt, internal_concat lf rf)
    else (internal_concat lt rt, internal_join lf v rf)

let of_sorted_list l =
  let rec sub n l =
    match n, l with
    | 0, l -> Empty, l
    | 1, x0 :: l -> Node (Empty, x0, Empty, 1), l
    | 2, x0 :: x1 :: l -> Node (Node(Empty, x0, Empty, 1), x1, Empty, 2), l
    | 3, x0 :: x1 :: x2 :: l ->
      Node (Node(Empty, x0, Empty, 1), x1, Node(Empty, x2, Empty, 1), 2),l
    | n, l ->
      let nl = n / 2 in
      let left, l = sub nl l in
      match l with
      | [] -> assert false
      | mid :: l ->
        let right, l = sub (n - nl - 1) l in
        create left mid right, l
  in
  fst (sub (List.length l) l)

let of_sorted_array l =   
  let rec sub start n l  =
    if n = 0 then Empty else 
    if n = 1 then 
      let x0 = Array.unsafe_get l start in
      Node (Empty, x0, Empty, 1)
    else if n = 2 then     
      let x0 = Array.unsafe_get l start in 
      let x1 = Array.unsafe_get l (start + 1) in 
      Node (Node(Empty, x0, Empty, 1), x1, Empty, 2) else
    if n = 3 then 
      let x0 = Array.unsafe_get l start in 
      let x1 = Array.unsafe_get l (start + 1) in
      let x2 = Array.unsafe_get l (start + 2) in
      Node (Node(Empty, x0, Empty, 1), x1, Node(Empty, x2, Empty, 1), 2)
    else 
      let nl = n / 2 in
      let left = sub start nl l in
      let mid = start + nl in 
      let v = Array.unsafe_get l mid in 
      let right = sub (mid + 1) (n - nl - 1) l in        
      create left v right
  in
  sub 0 (Array.length l) l 

let is_ordered ~cmp tree =
  let rec is_ordered_min_max tree =
    match tree with
    | Empty -> `Empty
    | Node(l,v,r,_) -> 
      begin match is_ordered_min_max l with
        | `No -> `No 
        | `Empty ->
          begin match is_ordered_min_max r with
            | `No  -> `No
            | `Empty -> `V (v,v)
            | `V(l,r) ->
              if cmp v l < 0 then
                `V(v,r)
              else
                `No
          end
        | `V(min_v,max_v)->
          begin match is_ordered_min_max r with
            | `No -> `No
            | `Empty -> 
              if cmp max_v v < 0 then 
                `V(min_v,v)
              else
                `No 
            | `V(min_v_r, max_v_r) ->
              if cmp max_v min_v_r < 0 then
                `V(min_v,max_v_r)
              else `No
          end
      end  in 
  is_ordered_min_max tree <> `No 

let invariant ~cmp t = 
  check t ; 
  is_ordered ~cmp t 

let rec compare_aux ~cmp e1 e2 =
  match (e1, e2) with
    (End, End) -> 0
  | (End, _)  -> -1
  | (_, End) -> 1
  | (More(v1, r1, e1), More(v2, r2, e2)) ->
    let c = cmp v1 v2 in
    if c <> 0
    then c
    else compare_aux ~cmp (cons_enum r1 e1) (cons_enum r2 e2)

let compare ~cmp s1 s2 =
  compare_aux ~cmp (cons_enum s1 End) (cons_enum s2 End)


module type S = sig
  type elt 
  type t
  val empty: t
  val is_empty: t -> bool
  val iter: t ->  (elt -> unit) -> unit
  val fold: t -> 'a -> (elt -> 'a -> 'a) -> 'a
  val for_all: t -> (elt -> bool) ->  bool
  val exists: t -> (elt -> bool) -> bool
  val singleton: elt -> t
  val cardinal: t -> int
  val elements: t -> elt list
  val min_elt: t -> elt
  val max_elt: t -> elt
  val choose: t -> elt
  val partition: t -> (elt -> bool) ->  t * t

  val mem: t -> elt -> bool
  val add: t -> elt -> t
  val remove: t -> elt -> t
  val union: t -> t -> t
  val inter: t -> t -> t
  val diff: t -> t -> t
  val compare: t -> t -> int
  val equal: t -> t -> bool
  val subset: t -> t -> bool
  val filter: t -> (elt -> bool) ->  t

  val split: t -> elt -> t * bool * t
  val find:  t -> elt -> elt
  val of_list: elt list -> t
  val of_sorted_list : elt list ->  t
  val of_sorted_array : elt array -> t 
  val of_array : elt array -> t 
  val invariant : t -> bool 
  val print : Format.formatter -> t -> unit 
end 

end
module Set_string : sig 
#1 "set_string.mli"
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




include Set_gen.S with type elt = string
end = struct
#1 "set_string.ml"
# 1 "ext/set.cppo.ml"
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

[@@@warning "-34"]
# 27 "ext/set.cppo.ml"
type elt = string
let compare_elt = Ext_string.compare 
let print_elt = Format.pp_print_string

# 49 "ext/set.cppo.ml"
type ('a, 'id) t0 = ('a, 'id) Set_gen.t0 = 
  | Empty 
  | Node of ('a, 'id) t0 * 'a * ('a, 'id) t0 * int 

type ('a, 'id) enumeration0 = ('a, 'id) Set_gen.enumeration0 = 
  | End 
  | More of 'a * ('a, 'id) t0 * ('a, 'id) enumeration0
    
type  t = (elt, unit) t0
type enumeration = (elt, unit) Set_gen.enumeration0
let empty = Set_gen.empty 
let is_empty = Set_gen.is_empty
let iter = Set_gen.iter
let fold = Set_gen.fold
let for_all = Set_gen.for_all 
let exists = Set_gen.exists 
let singleton = Set_gen.singleton 
let cardinal = Set_gen.cardinal
let elements = Set_gen.elements
let min_elt = Set_gen.min_elt
let max_elt = Set_gen.max_elt
let choose = Set_gen.choose 
(* let of_sorted_list = Set_gen.of_sorted_list *)
(* let of_sorted_array = Set_gen.of_sorted_array *)
let partition = Set_gen.partition 
let filter = Set_gen.filter 
let of_sorted_list = Set_gen.of_sorted_list
let of_sorted_array = Set_gen.of_sorted_array

let rec split (tree : t) x : t * bool * t =  match tree with 
  | Empty ->
    (Empty, false, Empty)
  | Node(l, v, r, _) ->
    let c = compare_elt x v in
    if c = 0 then (l, true, r)
    else if c < 0 then
      let (ll, pres, rl) = split l x in (ll, pres, Set_gen.internal_join rl v r)
    else
      let (lr, pres, rr) = split r x in (Set_gen.internal_join l v lr, pres, rr)
let rec add (tree : t) x : t =  match tree with 
  | Empty -> Node(Empty, x, Empty, 1)
  | Node(l, v, r, _) as t ->
    let c = compare_elt x v in
    if c = 0 then t else
    if c < 0 then Set_gen.internal_bal (add l x ) v r else Set_gen.internal_bal l v (add r x )

let rec union (s1 : t) (s2 : t) : t  =
  match (s1, s2) with
  | (Empty, t2) -> t2
  | (t1, Empty) -> t1
  | (Node(l1, v1, r1, h1), Node(l2, v2, r2, h2)) ->
    if h1 >= h2 then
      if h2 = 1 then add s1 v2 else begin
        let (l2, _, r2) = split s2 v1 in
        Set_gen.internal_join (union l1 l2) v1 (union r1 r2)
      end
    else
    if h1 = 1 then add s2 v1 else begin
      let (l1, _, r1) = split s1 v2 in
      Set_gen.internal_join (union l1 l2) v2 (union r1 r2)
    end    

let rec inter (s1 : t)  (s2 : t) : t  =
  match (s1, s2) with
  | (Empty, _) -> Empty
  | (_, Empty) -> Empty
  | (Node(l1, v1, r1, _), t2) ->
    begin match split t2 v1 with
      | (l2, false, r2) ->
        Set_gen.internal_concat (inter l1 l2) (inter r1 r2)
      | (l2, true, r2) ->
        Set_gen.internal_join (inter l1 l2) v1 (inter r1 r2)
    end 

let rec diff (s1 : t) (s2 : t) : t  =
  match (s1, s2) with
  | (Empty, _) -> Empty
  | (t1, Empty) -> t1
  | (Node(l1, v1, r1, _), t2) ->
    begin match split t2 v1 with
      | (l2, false, r2) ->
        Set_gen.internal_join (diff l1 l2) v1 (diff r1 r2)
      | (l2, true, r2) ->
        Set_gen.internal_concat (diff l1 l2) (diff r1 r2)    
    end


let rec mem (tree : t) x =  match tree with 
  | Empty -> false
  | Node(l, v, r, _) ->
    let c = compare_elt x v in
    c = 0 || mem (if c < 0 then l else r) x

let rec remove (tree : t)  x : t = match tree with 
  | Empty -> Empty
  | Node(l, v, r, _) ->
    let c = compare_elt x v in
    if c = 0 then Set_gen.internal_merge l r else
    if c < 0 then Set_gen.internal_bal (remove l x) v r else Set_gen.internal_bal l v (remove r x )

let compare s1 s2 = Set_gen.compare ~cmp:compare_elt s1 s2 


let equal s1 s2 =
  compare s1 s2 = 0

let rec subset (s1 : t) (s2 : t) =
  match (s1, s2) with
  | Empty, _ ->
    true
  | _, Empty ->
    false
  | Node (l1, v1, r1, _), (Node (l2, v2, r2, _) as t2) ->
    let c = compare_elt v1 v2 in
    if c = 0 then
      subset l1 l2 && subset r1 r2
    else if c < 0 then
      subset (Node (l1, v1, Empty, 0)) l2 && subset r1 t2
    else
      subset (Node (Empty, v1, r1, 0)) r2 && subset l1 t2




let rec find (tree : t) x = match tree with
  | Empty -> raise Not_found
  | Node(l, v, r, _) ->
    let c = compare_elt x v in
    if c = 0 then v
    else find (if c < 0 then l else r) x 



let of_list l =
  match l with
  | [] -> empty
  | [x0] -> singleton x0
  | [x0; x1] -> add (singleton x0) x1 
  | [x0; x1; x2] -> add (add (singleton x0)  x1) x2 
  | [x0; x1; x2; x3] -> add (add (add (singleton x0) x1 ) x2 ) x3 
  | [x0; x1; x2; x3; x4] -> add (add (add (add (singleton x0) x1) x2 ) x3 ) x4 
  | _ -> of_sorted_list (List.sort_uniq compare_elt l)

let of_array l = 
  Ext_array.fold_left l empty (fun  acc x -> add acc x ) 

(* also check order *)
let invariant t =
  Set_gen.check t ;
  Set_gen.is_ordered ~cmp:compare_elt t          

let print fmt s = 
  Format.fprintf 
   fmt   "@[<v>{%a}@]@."
    (fun fmt s   -> 
       iter s
         (fun e -> Format.fprintf fmt "@[<v>%a@],@ " 
         print_elt e) 
    )
    s     






end
module Bsb_file_groups : sig 
#1 "bsb_file_groups.mli"
(* Copyright (C) 2018- Authors of BuckleScript
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


 type public = 
  | Export_none
  | Export_all 
  | Export_set of Set_string.t 
  

type build_generator = 
  { input : string list ;
    output : string list;
    command : string}  


type  file_group = 
  { dir : string ;
    sources : Bsb_db.t; 
    resources : string list ;
    public : public ;
    dir_index : Bsb_dir_index.t  ;
    generators : build_generator list ; 
    (* output of [generators] should be added to [sources],
       if it is [.ml,.mli,.re,.rei]
    *)
  }     

type file_groups = file_group list 

type t 
  = private
  { files :  file_groups; 
    globbed_dirs : string list ; 
  }

val empty : t    

val merge : 
  t -> 
  t -> 
  t   

val cons :   
  file_group:file_group ->
  ?globbed_dir:string ->
  t ->
  t

val is_empty :   
  file_group ->
  bool


end = struct
#1 "bsb_file_groups.ml"
(* Copyright (C) 2018- Authors of BuckleScript
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


 type public = 
  | Export_none
  | Export_all 
  | Export_set of Set_string.t 
  

type build_generator = 
  { input : string list ;
    output : string list;
    command : string}  


type  file_group = 
  { dir : string ;
    sources : Bsb_db.t; 
    resources : string list ;
    public : public ;
    dir_index : Bsb_dir_index.t  ;
    generators : build_generator list ; 
    (* output of [generators] should be added to [sources],
       if it is [.ml,.mli,.re,.rei]
    *)
  }     

type file_groups = file_group list 

type t =   
  { files :  file_groups; 
    globbed_dirs : string list ; 
  }



let empty : t = { files = []; globbed_dirs = [];  }



let merge (u : t)  (v : t)  = 
  if u == empty then v 
  else if v == empty then u 
  else 
    {
      files = Ext_list.append u.files  v.files ; 
      globbed_dirs = Ext_list.append u.globbed_dirs  v.globbed_dirs ; 
    }  

let cons ~file_group ?globbed_dir (v : t) : t =  
  {
    files = file_group :: v.files;
    globbed_dirs = 
      match globbed_dir with 
      | None -> v.globbed_dirs
      | Some f -> f :: v.globbed_dirs
  }
(** when [is_empty file_group]
    we don't need issue [-I] [-S] in [.merlin] file
*)  
let is_empty (x : file_group) = 
  Map_string.is_empty x.sources &&
  x.resources = [] &&
  x.generators = []    
end
module Ext_pervasives : sig 
#1 "ext_pervasives.mli"
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








(** Extension to standard library [Pervavives] module, safe to open 
  *)

external reraise: exn -> 'a = "%reraise"

val finally : 
  'a ->
  clean:('a -> 'c) -> 
  ('a -> 'b) -> 'b

(* val try_it : (unit -> 'a) ->  unit  *)

val with_file_as_chan : string -> (out_channel -> 'a) -> 'a













(* external id : 'a -> 'a = "%identity" *)

(** Copied from {!Btype.hash_variant}:
    need sync up and add test case
 *)
(* val hash_variant : string -> int *)

(* val todo : string -> 'a *)

val nat_of_string_exn : string -> int

val parse_nat_of_string:
  string -> 
  int ref -> 
  int 
end = struct
#1 "ext_pervasives.ml"
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






external reraise: exn -> 'a = "%reraise"

let finally v ~clean:action f   = 
  match f v with
  | exception e -> 
      action v ;
      reraise e 
  | e ->  action v ; e 

(* let try_it f  =   
  try ignore (f ()) with _ -> () *)

let with_file_as_chan filename f = 
  finally (open_out_bin filename) ~clean:close_out f 






(* external id : 'a -> 'a = "%identity" *)

(* 
let hash_variant s =
  let accu = ref 0 in
  for i = 0 to String.length s - 1 do
    accu := 223 * !accu + Char.code s.[i]
  done;
  (* reduce to 31 bits *)
  accu := !accu land (1 lsl 31 - 1);
  (* make it signed for 64 bits architectures *)
  if !accu > 0x3FFFFFFF then !accu - (1 lsl 31) else !accu *)

(* let todo loc = 
  failwith (loc ^ " Not supported yet")
 *)



let rec int_of_string_aux s acc off len =  
  if off >= len then acc 
  else 
    let d = (Char.code (String.unsafe_get s off) - 48) in 
    if d >=0 && d <= 9 then 
      int_of_string_aux s (10*acc + d) (off + 1) len
    else -1 (* error *)

let nat_of_string_exn (s : string) = 
  let acc = int_of_string_aux s 0 0 (String.length s) in 
  if acc < 0 then invalid_arg s 
  else acc 


(** return index *)
let parse_nat_of_string (s : string) (cursor : int ref) =  
  let current = !cursor in 
  assert (current >= 0);
  let acc = ref 0 in 
  let s_len = String.length s in 
  let todo = ref true in 
  let cur = ref current in 
  while !todo && !cursor < s_len do 
    let d = Char.code (String.unsafe_get s !cur) - 48 in 
    if d >=0 && d <= 9 then begin 
      acc := 10* !acc + d;
      incr cur
    end else todo := false
  done ;
  cursor := !cur;
  !acc 
end
module Ext_fmt
= struct
#1 "ext_fmt.ml"


let with_file_as_pp filename f = 
  Ext_pervasives.finally (open_out_bin filename) ~clean:close_out
    (fun chan -> 
      let fmt = Format.formatter_of_out_channel chan in
      let v = f  fmt in
      Format.pp_print_flush fmt ();
      v
    ) 



let failwithf ~loc fmt = Format.ksprintf (fun s -> failwith (loc ^ s))
    fmt
    
let invalid_argf fmt = Format.ksprintf invalid_arg fmt


end
module Ext_sys : sig 
#1 "ext_sys.mli"
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


(* Not used yet *)
(* val is_directory_no_exn : string -> bool *)


val is_windows_or_cygwin : bool 


end = struct
#1 "ext_sys.ml"
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

(** TODO: not exported yet, wait for Windows Fix*)
(* let is_directory_no_exn f = 
  try Sys.is_directory f with _ -> false  *)


let is_windows_or_cygwin = Sys.win32 || Sys.cygwin



end
module Literals : sig 
#1 "literals.mli"
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






val js_array_ctor : string 
val js_type_number : string
val js_type_string : string
val js_type_object : string
val js_type_boolean : string
val js_undefined : string
val js_prop_length : string

val param : string
val partial_arg : string
val prim : string

(**temporary varaible used in {!Js_ast_util} *)
val tmp : string 

val create : string 
val runtime : string
val stdlib : string
val imul : string

val setter_suffix : string
val setter_suffix_len : int


val debugger : string

val unsafe_downgrade : string
val fn_run : string
val method_run : string
val fn_method : string
val fn_mk : string

(** callback actually, not exposed to user yet *)
(* val js_fn_runmethod : string *)

val bs_deriving : string
val bs_deriving_dot : string
val bs_type : string

(** nodejs *)

val node_modules : string
val node_modules_length : int
val package_json : string
val bsconfig_json : string
val build_ninja : string

(* Name of the library file created for each external dependency. *)
val library_file : string

val suffix_a : string
val suffix_cmj : string
val suffix_cmo : string
val suffix_cma : string
val suffix_cmi : string
val suffix_cmx : string
val suffix_cmxa : string
val suffix_ml : string
val suffix_mlast : string 
val suffix_mlast_simple : string
val suffix_mliast : string
val suffix_reast : string
val suffix_reiast : string

val suffix_mliast_simple : string
val suffix_mlmap : string
val suffix_mll : string
val suffix_re : string
val suffix_rei : string 

val suffix_d : string
val suffix_js : string
val suffix_bs_js : string 
(* val suffix_re_js : string *)
val suffix_gen_js : string 
val suffix_gen_tsx: string

val suffix_tsx : string

val suffix_mli : string 
val suffix_cmt : string 
val suffix_cmti : string 

val commonjs : string 

val es6 : string 
val es6_global : string

val unused_attribute : string 
val dash_nostdlib : string

val reactjs_jsx_ppx_2_exe : string 
val reactjs_jsx_ppx_3_exe : string 

val native : string
val bytecode : string
val js : string

val node_sep : string 
val node_parent : string 
val node_current : string 
val gentype_import : string

val bsbuild_cache : string

val sourcedirs_meta : string

val ns_sep_char : char
val ns_sep : string
end = struct
#1 "literals.ml"
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







let js_array_ctor = "Array"
let js_type_number = "number"
let js_type_string = "string"
let js_type_object = "object" 
let js_type_boolean = "boolean"
let js_undefined = "undefined"
let js_prop_length = "length"

let prim = "prim"
let param = "param"
let partial_arg = "partial_arg"
let tmp = "tmp"

let create = "create" (* {!Caml_exceptions.create}*)

let runtime = "runtime" (* runtime directory *)

let stdlib = "stdlib"

let imul = "imul" (* signed int32 mul *)

let setter_suffix = "#="
let setter_suffix_len = String.length setter_suffix

let debugger = "debugger"
let unsafe_downgrade = "unsafe_downgrade"
let fn_run = "fn_run"
let method_run = "method_run"

let fn_method = "fn_method"
let fn_mk = "fn_mk"
(*let js_fn_runmethod = "js_fn_runmethod"*)

let bs_deriving = "bs.deriving"
let bs_deriving_dot = "bs.deriving."
let bs_type = "bs.type"


(** nodejs *)
let node_modules = "node_modules"
let node_modules_length = String.length "node_modules"
let package_json = "package.json"
let bsconfig_json = "bsconfig.json"
let build_ninja = "build.ninja"

(* Name of the library file created for each external dependency. *)
let library_file = "lib"

let suffix_a = ".a"
let suffix_cmj = ".cmj"
let suffix_cmo = ".cmo"
let suffix_cma = ".cma"
let suffix_cmi = ".cmi"
let suffix_cmx = ".cmx"
let suffix_cmxa = ".cmxa"
let suffix_mll = ".mll"
let suffix_ml = ".ml"
let suffix_mli = ".mli"
let suffix_re = ".re"
let suffix_rei = ".rei"
let suffix_mlmap = ".mlmap"

let suffix_cmt = ".cmt" 
let suffix_cmti = ".cmti" 
let suffix_mlast = ".mlast"
let suffix_mlast_simple = ".mlast_simple"
let suffix_mliast = ".mliast"
let suffix_reast = ".reast"
let suffix_reiast = ".reiast"
let suffix_mliast_simple = ".mliast_simple"
let suffix_d = ".d"
let suffix_js = ".js"
let suffix_bs_js = ".bs.js"
(* let suffix_re_js = ".re.js" *)
let suffix_gen_js = ".gen.js"
let suffix_gen_tsx = ".gen.tsx"
let suffix_tsx = ".tsx"

let commonjs = "commonjs" 

let es6 = "es6"
let es6_global = "es6-global"

let unused_attribute = "Unused attribute " 
let dash_nostdlib = "-nostdlib"

let reactjs_jsx_ppx_2_exe = "reactjs_jsx_ppx_2.exe"
let reactjs_jsx_ppx_3_exe  = "reactjs_jsx_ppx_3.exe"

let native = "native"
let bytecode = "bytecode"
let js = "js"



(** Used when produce node compatible paths *)
let node_sep = "/"
let node_parent = ".."
let node_current = "."

let gentype_import = "genType.import"

let bsbuild_cache = ".bsbuild"    

let sourcedirs_meta = ".sourcedirs.json"

(* Note the build system should check the validity of filenames
   espeically, it should not contain '-'
*)
let ns_sep_char = '-'
let ns_sep = "-"

end
module Ext_path : sig 
#1 "ext_path.mli"
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

type t 


(** Js_output is node style, which means 
    separator is only '/'

    if the path contains 'node_modules', 
    [node_relative_path] will discard its prefix and 
    just treat it as a library instead
*)
val simple_convert_node_path_to_os_path : string -> string



(**
   [combine path1 path2]
   1. add some simplifications when concatenating
   2. when [path2] is absolute, return [path2]
*)  
val combine : 
  string -> 
  string -> 
  string    



(**
   {[
     get_extension "a.txt" = ".txt"
       get_extension "a" = ""
   ]}
*)





val node_rebase_file :
  from:string -> 
  to_:string ->
  string -> 
  string 

(** 
   TODO: could be highly optimized
   if [from] and [to] resolve to the same path, a zero-length string is returned 
   Given that two paths are directory

   A typical use case is 
   {[
     Filename.concat 
       (rel_normalized_absolute_path cwd (Filename.dirname a))
       (Filename.basename a)
   ]}
*)
val rel_normalized_absolute_path : from:string -> string -> string 


val normalize_absolute_path : string -> string 


val absolute_cwd_path : string -> string 

(** [concat dirname filename]
    The same as {!Filename.concat} except a tiny optimization 
    for current directory simplification
*)
val concat : string -> string -> string 

val check_suffix_case : 
  string -> string -> bool



(* It is lazy so that it will not hit errors when in script mode *)
val package_dir : string Lazy.t

end = struct
#1 "ext_path.ml"
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

(* [@@@warning "-37"] *)
type t =  
  (* | File of string  *)
  | Dir of string  
[@@unboxed]

let simple_convert_node_path_to_os_path =
  if Sys.unix then fun x -> x 
  else if Sys.win32 || Sys.cygwin then 
    Ext_string.replace_slash_backward 
  else failwith ("Unknown OS : " ^ Sys.os_type)


let cwd = lazy (Sys.getcwd())

let split_by_sep_per_os : string -> string list = 
  if Ext_sys.is_windows_or_cygwin then 
  fun x -> 
    (* on Windows, we can still accept -bs-package-output lib/js *)
    Ext_string.split_by 
      (fun x -> match x with |'/' |'\\' -> true | _ -> false) x
  else 
  fun x -> Ext_string.split x '/'

(** example
    {[
      "/bb/mbigc/mbig2899/bgit/bucklescript/jscomp/stdlib/external/pervasives.cmj"
        "/bb/mbigc/mbig2899/bgit/bucklescript/jscomp/stdlib/ocaml_array.ml"
    ]}

    The other way
    {[

      "/bb/mbigc/mbig2899/bgit/bucklescript/jscomp/stdlib/ocaml_array.ml"
        "/bb/mbigc/mbig2899/bgit/bucklescript/jscomp/stdlib/external/pervasives.cmj"
    ]}
    {[
      "/bb/mbigc/mbig2899/bgit/bucklescript/jscomp/stdlib//ocaml_array.ml"
    ]}
    {[
      /a/b
      /c/d
    ]}
*)
let node_relative_path 
    ~from:(file_or_dir_2 : t )
    (file_or_dir_1 : t) 
  = 
  let relevant_dir1 = 
    match file_or_dir_1 with 
    | Dir x -> x 
    (* | File file1 ->  Filename.dirname file1 *) in
  let relevant_dir2 = 
    match file_or_dir_2 with 
    | Dir x -> x 
    (* | File file2 -> Filename.dirname file2  *) in
  let dir1 = split_by_sep_per_os relevant_dir1 in
  let dir2 = split_by_sep_per_os relevant_dir2 in
  let rec go (dir1 : string list) (dir2 : string list) = 
    match dir1, dir2 with 
    | "." :: xs, ys -> go xs ys 
    | xs , "." :: ys -> go xs ys 
    | x::xs , y :: ys when x = y
      -> go xs ys 
    | _, _ -> 
      Ext_list.map_append  dir2  dir1  (fun _ ->  Literals.node_parent)
  in
  match go dir1 dir2 with
  | (x :: _ ) as ys when x = Literals.node_parent -> 
    String.concat Literals.node_sep ys
  | ys -> 
    String.concat Literals.node_sep  
    @@ Literals.node_current :: ys


let node_concat ~dir base =
  dir ^ Literals.node_sep ^ base 

let node_rebase_file ~from ~to_ file = 
  
  node_concat
    ~dir:(
      if from = to_ then Literals.node_current
      else node_relative_path ~from:(Dir from) (Dir to_)) 
    file
    
    
(***
   {[
     Filename.concat "." "";;
     "./"
   ]}
*)
let combine path1 path2 =  
  if Filename.is_relative path2 then
    if Ext_string.is_empty path2 then 
      path1
    else 
    if path1 = Filename.current_dir_name then 
      path2
    else
    if path2 = Filename.current_dir_name 
    then path1
    else
      Filename.concat path1 path2 
  else
    path2








let (//) x y =
  if x = Filename.current_dir_name then y
  else if y = Filename.current_dir_name then x 
  else Filename.concat x y 

(**
   {[
     split_aux "//ghosg//ghsogh/";;
     - : string * string list = ("/", ["ghosg"; "ghsogh"])
   ]}
   Note that 
   {[
     Filename.dirname "/a/" = "/"
       Filename.dirname "/a/b/" = Filename.dirname "/a/b" = "/a"
   ]}
   Special case:
   {[
     basename "//" = "/"
       basename "///"  = "/"
   ]}
   {[
     basename "" =  "."
       basename "" = "."
       dirname "" = "."
       dirname "" =  "."
   ]}  
*)
let split_aux p =
  let rec go p acc =
    let dir = Filename.dirname p in
    if dir = p then dir, acc
    else
      let new_path = Filename.basename p in 
      if Ext_string.equal new_path Filename.dir_sep then 
        go dir acc 
        (* We could do more path simplification here
           leave to [rel_normalized_absolute_path]
        *)
      else 
        go dir (new_path :: acc)

  in go p []





(** 
   TODO: optimization
   if [from] and [to] resolve to the same path, a zero-length string is returned 

   This function is useed in [es6-global] and 
   [amdjs-global] format and tailored for `rollup`
*)
let rel_normalized_absolute_path ~from to_ =
  let root1, paths1 = split_aux from in 
  let root2, paths2 = split_aux to_ in 
  if root1 <> root2 then root2
  else
    let rec go xss yss =
      match xss, yss with 
      | x::xs, y::ys -> 
        if Ext_string.equal x  y then go xs ys 
        else if x = Filename.current_dir_name then go xs yss 
        else if y = Filename.current_dir_name then go xss ys
        else 
          let start = 
            Ext_list.fold_left xs Ext_string.parent_dir_lit (fun acc  _  -> acc // Ext_string.parent_dir_lit )
          in 
          Ext_list.fold_left yss start (fun acc v -> acc // v)
      | [], [] -> Ext_string.empty
      | [], y::ys -> Ext_list.fold_left ys y (fun acc x -> acc // x) 
      | _::xs, [] ->
        Ext_list.fold_left xs Ext_string.parent_dir_lit (fun acc _ -> acc // Ext_string.parent_dir_lit )
     in
    let v =  go paths1 paths2  in 

    if Ext_string.is_empty v then  Literals.node_current
    else 
    if
      v = "."
      || v = ".."
      || Ext_string.starts_with v "./"  
      || Ext_string.starts_with v "../" 
    then v 
    else "./" ^ v 

(*TODO: could be hgighly optimized later 
  {[
    normalize_absolute_path "/gsho/./..";;

    normalize_absolute_path "/a/b/../c../d/e/f";;

    normalize_absolute_path "/gsho/./..";;

    normalize_absolute_path "/gsho/./../..";;

    normalize_absolute_path "/a/b/c/d";;

    normalize_absolute_path "/a/b/c/d/";;

    normalize_absolute_path "/a/";;

    normalize_absolute_path "/a";;
  ]}
*)
(** See tests in {!Ounit_path_tests} *)
let normalize_absolute_path x =
  let drop_if_exist xs =
    match xs with 
    | [] -> []
    | _ :: xs -> xs in 
  let rec normalize_list acc paths =
    match paths with 
    | [] -> acc 
    | x :: xs -> 
      if Ext_string.equal x Ext_string.current_dir_lit then 
        normalize_list acc xs 
      else if Ext_string.equal x Ext_string.parent_dir_lit then 
        normalize_list (drop_if_exist acc ) xs 
      else   
        normalize_list (x::acc) xs 
  in
  let root, paths = split_aux x in
  let rev_paths =  normalize_list [] paths in 
  let rec go acc rev_paths =
    match rev_paths with 
    | [] -> Filename.concat root acc 
    | last::rest ->  go (Filename.concat last acc ) rest  in 
  match rev_paths with 
  | [] -> root 
  | last :: rest -> go last rest 




let absolute_path cwd s = 
  let process s = 
    let s = 
      if Filename.is_relative s then
        Lazy.force cwd // s 
      else s in
    (* Now simplify . and .. components *)
    let rec aux s =
      let base,dir  = Filename.basename s, Filename.dirname s  in
      if dir = s then dir
      else if base = Filename.current_dir_name then aux dir
      else if base = Filename.parent_dir_name then Filename.dirname (aux dir)
      else aux dir // base
    in aux s  in 
  process s 

let absolute_cwd_path s = 
  absolute_path cwd  s 

(* let absolute cwd s =   
  match s with 
  | File x -> File (absolute_path cwd x )
  | Dir x -> Dir (absolute_path cwd x) *)

let concat dirname filename =
  if filename = Filename.current_dir_name then dirname
  else if dirname = Filename.current_dir_name then filename
  else Filename.concat dirname filename
  

let check_suffix_case =
  Ext_string.ends_with

(* Input must be absolute directory *)
let rec find_root_filename ~cwd filename   = 
  if Sys.file_exists ( Filename.concat cwd  filename) then cwd
  else 
    let cwd' = Filename.dirname cwd in 
    if String.length cwd' < String.length cwd then  
      find_root_filename ~cwd:cwd'  filename 
    else 
      Ext_fmt.failwithf 
        ~loc:__LOC__
        "%s not found from %s" filename cwd


let find_package_json_dir cwd  = 
  find_root_filename ~cwd  Literals.bsconfig_json

let package_dir = lazy (find_package_json_dir (Lazy.force cwd))

end
module Bsb_config : sig 
#1 "bsb_config.mli"
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


val ocaml_bin_install_prefix : string -> string
val proj_rel : string -> string

val lib_lit : string
val lib_js : string 
val lib_bs : string
val lib_es6 : string 
val lib_es6_global : string 
val lib_ocaml : string
val all_lib_artifacts : string list 
(* we need generate path relative to [lib/bs] directory in the opposite direction *)
val rev_lib_bs_prefix : string -> string


(** default not install, only when -make-world, its dependencies will be installed  *)


end = struct
#1 "bsb_config.ml"
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
let (//) = Ext_path.combine 

let lib_lit = "lib"
let lib_js = lib_lit //"js"

let lib_ocaml = lib_lit // "ocaml"
let lib_bs = lib_lit // "bs"
let lib_es6 = lib_lit // "es6"
let lib_es6_global = lib_lit // "es6_global"

let all_lib_artifacts = 
  [ lib_js ; 
    lib_ocaml;
    lib_bs ; 
    lib_es6 ; 
    lib_es6_global;
  ]
let rev_lib_bs = ".."// ".."


let rev_lib_bs_prefix p = rev_lib_bs // p 

let ocaml_bin_install_prefix p = lib_ocaml // p

let lazy_src_root_dir = "$src_root_dir" 
let proj_rel path = lazy_src_root_dir // path

(** it may not be a bad idea to hard code the binary path 
    of bsb in configuration time
*)






(* let cmd_package_specs = ref None  *)


end
module Bs_version : sig 
#1 "bs_version.mli"
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

val version : string

val header : string 

val package_name : string
end = struct
#1 "bs_version.ml"

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
let version = "7.2.0-dev.4"
let header = 
   "// Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE"  
let package_name = "bs-platform-native"   
    
end
module Bsb_pkg_types : sig 
#1 "bsb_pkg_types.mli"
(* Copyright (C) 2019- Authors of BuckleScript
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


type t = 
  | Global of string
  | Scope of string * scope
and scope = string  

val to_string : t -> string 
val print : Format.formatter -> t -> unit 
val equal : t -> t -> bool 

(* The second element could be empty or dropped 
*)
val extract_pkg_name_and_file : string -> t * string 
val string_as_package : string -> t 
end = struct
#1 "bsb_pkg_types.ml"

(* Copyright (C) 2018- Authors of BuckleScript
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

let (//) = Filename.concat

type t = 
  | Global of string
  | Scope of string * scope
and scope = string  

let to_string (x : t) = 
  match x with
  | Global s -> s
  | Scope (s,scope) -> scope // s 

let print fmt (x : t) = 
  match x with   
  | Global s -> Format.pp_print_string fmt s 
  | Scope(name,scope) -> 
    Format.fprintf fmt "%s/%s" scope name

let equal (x : t) y = 
  match x, y with 
  | Scope(a0,a1), Scope(b0,b1) 
    -> a0 = b0 && a1 = b1
  | Global a0, Global b0 -> a0 = b0
  | Scope _, Global _ 
  | Global _, Scope _ -> false

(**
  input: {[
    @hello/yy/xx
    hello/yy
  ]}
  FIXME: fix invalid input
  {[
    hello//xh//helo
  ]}
*)
let extract_pkg_name_and_file (s : string) =   
  let len = String.length s in 
  assert (len  > 0 ); 
  let v = String.unsafe_get s 0 in 
  if v = '@' then 
    let scope_id = 
      Ext_string.no_slash_idx s  in 
    assert (scope_id > 0);
    let pkg_id =   
      Ext_string.no_slash_idx_from
        s (scope_id + 1)   in 
     let scope =     
      String.sub s 0 scope_id in 
     
     if pkg_id < 0 then     
      (Scope(String.sub s (scope_id + 1) (len - scope_id - 1), scope),"")
     else 
      (Scope(
        String.sub s (scope_id + 1) (pkg_id - scope_id - 1), scope), 
       String.sub s (pkg_id + 1) (len - pkg_id - 1))
  else     
      let pkg_id = Ext_string.no_slash_idx s in 
      if pkg_id < 0 then 
      Global s , ""
      else 
      Global (String.sub s 0 pkg_id), 
              (String.sub s (pkg_id + 1) (len - pkg_id - 1))


let string_as_package (s : string) : t = 
  let len = String.length s in 
  assert (len > 0); 
  let v = String.unsafe_get s 0 in 
  if v = '@' then 
    let scope_id = 
        Ext_string.no_slash_idx s in 
    assert (scope_id > 0);
    Scope(
      String.sub s (scope_id + 1) (len - scope_id - 1),
      String.sub s 0 scope_id
      )    
  else Global s       
end
module Ext_json_types
= struct
#1 "ext_json_types.ml"
(* Copyright (C) 2015-2017 Bloomberg Finance L.P.
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

type loc = Lexing.position
type json_str = 
  { str : string ; loc : loc}

type json_flo  =
  { flo : string ; loc : loc}
type json_array =
  { content : t array ; 
    loc_start : loc ; 
    loc_end : loc ; 
  }

and json_map = 
  { map : t Map_string.t ; loc :  loc }
and t = 
  | True of loc 
  | False of loc 
  | Null of loc 
  | Flo of json_flo
  | Str of json_str
  | Arr  of json_array
  | Obj of json_map
   

end
module Ext_position : sig 
#1 "ext_position.mli"
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


type t = Lexing.position = {
    pos_fname : string ;
    pos_lnum : int ;
    pos_bol : int ;
    pos_cnum : int
}

(** [offset pos newpos]
    return a new position
    here [newpos] is zero based, the use case is that
    at position [pos], we get a string and Lexing from that string,
    therefore, we get a [newpos] and we need rebase it on top of 
    [pos]
*)
val offset : t -> t -> t 

val lexbuf_from_channel_with_fname:
    in_channel -> string -> 
    Lexing.lexbuf

val print : Format.formatter -> t -> unit 
end = struct
#1 "ext_position.ml"
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


type t = Lexing.position = {
    pos_fname : string ;
    pos_lnum : int ;
    pos_bol : int ;
    pos_cnum : int
}

let offset (x : t) (y:t) =
  {
    x with 
    pos_lnum =
       x.pos_lnum + y.pos_lnum - 1;
    pos_cnum = 
      x.pos_cnum + y.pos_cnum;
    pos_bol = 
      if y.pos_lnum = 1 then 
        x.pos_bol
      else x.pos_cnum + y.pos_bol
  }

let print fmt (pos : t) =
  Format.fprintf fmt "(line %d, column %d)" pos.pos_lnum (pos.pos_cnum - pos.pos_bol)



let lexbuf_from_channel_with_fname ic fname = 
  let x = Lexing.from_function (fun buf n -> input ic buf 0 n) in 
  let pos : t = {
    pos_fname = fname ; 
    pos_lnum = 1; 
    pos_bol = 0;
    pos_cnum = 0 (* copied from zero_pos*)
  } in 
  x.lex_start_p <- pos;
  x.lex_curr_p <- pos ; 
  x


end
module Ext_json : sig 
#1 "ext_json.mli"
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


type path = string list 
type status = 
  | No_path
  | Found of Ext_json_types.t 
  | Wrong_type of path 


type callback = 
  [
    `Str of (string -> unit) 
  | `Str_loc of (string -> Lexing.position -> unit)
  | `Flo of (string -> unit )
  | `Flo_loc of (string -> Lexing.position -> unit )
  | `Bool of (bool -> unit )
  | `Obj of (Ext_json_types.t Map_string.t -> unit)
  | `Arr of (Ext_json_types.t array -> unit )
  | `Arr_loc of 
    (Ext_json_types.t array -> Lexing.position -> Lexing.position -> unit)
  | `Null of (unit -> unit)
  | `Not_found of (unit -> unit)
  | `Id of (Ext_json_types.t -> unit )
  ]

val test:
  ?fail:(unit -> unit) ->
  string -> callback 
  -> Ext_json_types.t Map_string.t
   -> Ext_json_types.t Map_string.t


val loc_of : Ext_json_types.t -> Ext_position.t

val equal : Ext_json_types.t -> Ext_json_types.t -> bool 

end = struct
#1 "ext_json.ml"
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

type callback = 
  [
    `Str of (string -> unit) 
  | `Str_loc of (string -> Lexing.position -> unit)
  | `Flo of (string -> unit )
  | `Flo_loc of (string -> Lexing.position -> unit )
  | `Bool of (bool -> unit )
  | `Obj of (Ext_json_types.t Map_string.t -> unit)
  | `Arr of (Ext_json_types.t array -> unit )
  | `Arr_loc of (Ext_json_types.t array -> Lexing.position -> Lexing.position -> unit)
  | `Null of (unit -> unit)
  | `Not_found of (unit -> unit)
  | `Id of (Ext_json_types.t -> unit )
  ]


type path = string list 

type status = 
  | No_path
  | Found  of Ext_json_types.t 
  | Wrong_type of path 

let test   ?(fail=(fun () -> ())) key 
    (cb : callback) (m  : Ext_json_types.t Map_string.t)
  =
  begin match Map_string.find_exn m key, cb with 
    | exception Not_found  ->
      begin match cb with `Not_found f ->  f ()
                        | _ -> fail ()
      end      
    | True _, `Bool cb -> cb true
    | False _, `Bool cb  -> cb false 
    | Flo {flo = s} , `Flo cb  -> cb s 
    | Flo {flo = s; loc} , `Flo_loc cb  -> cb s loc
    | Obj {map = b} , `Obj cb -> cb b 
    | Arr {content}, `Arr cb -> cb content 
    | Arr {content; loc_start ; loc_end}, `Arr_loc cb -> 
      cb content  loc_start loc_end 
    | Null _, `Null cb  -> cb ()
    | Str {str = s }, `Str cb  -> cb s 
    | Str {str = s ; loc }, `Str_loc cb -> cb s loc 
    |  any  , `Id  cb -> cb any
    | _, _ -> fail () 
  end;
  m


let loc_of (x : Ext_json_types.t) =
  match x with
  | True p | False p | Null p -> p 
  | Str p -> p.loc 
  | Arr p -> p.loc_start
  | Obj p -> p.loc
  | Flo p -> p.loc


let rec equal 
    (x : Ext_json_types.t)
    (y : Ext_json_types.t) = 
  match x with 
  | Null _ -> (* [%p? Null _ ] *)
    begin match y with
      | Null _ -> true
      | _ -> false end
  | Str {str } -> 
    begin match y with 
      | Str rhs -> str = rhs.str
      | _ -> false end
  | Flo {flo} 
    ->
    begin match y with
      |  Flo rhs -> 
        flo = rhs.flo
      | _ -> false
    end
  | True _ -> 
    begin match y with 
      | True _ -> true 
      | _ -> false 
    end
  | False _ -> 
    begin match y with 
      | False _ -> true 
      | _ -> false 
    end     
  | Arr {content} 
    -> 
    begin match y with 
      | Arr rhs
        ->
        Ext_array.for_all2_no_exn content rhs.content equal
      | _ -> false 
    end

  | Obj {map} -> 
    begin match y with 
      | Obj rhs -> 
        Map_string.equal map rhs.map equal
      | _ -> false 
    end 


end
module Bsb_exception : sig 
#1 "bsb_exception.mli"
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


(**
    This module is used for fatal errros
*)
type error  
exception Error of error

val print : Format.formatter -> error -> unit 
val package_not_found : pkg:Bsb_pkg_types.t -> json:string option -> 'a

val conflict_module:
    string -> string -> string -> 'a 
    
val errorf : loc:Ext_position.t ->  ('a, unit, string, 'b) format4 -> 'a

val config_error : Ext_json_types.t -> string -> 'a 

val missing_object_file : string -> 'a
val missing_entry : string -> 'a
val no_files_to_pack : string -> 'a

val invalid_spec : string -> 'a

val invalid_json : string -> 'a

val no_implementation : string -> 'a

val not_consistent : string -> 'a

end = struct
#1 "bsb_exception.ml"
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



type error =
  | Package_not_found of Bsb_pkg_types.t * string option (* json file *)
  | Json_config of Ext_position.t * string
  | Invalid_json of string
  | Invalid_spec of string
  | Conflict_module of string * string * string
  | No_implementation of string
  | Not_consistent of string 
  | Missing_object_file of string
  | Missing_entry of string
  | No_files_to_pack of string

exception Error of error

let error err = raise (Error err)
let package_not_found ~pkg ~json =
  error (Package_not_found(pkg,json))

let print (fmt : Format.formatter) (x : error) =
  match x with
  | Conflict_module (modname,dir1,dir2) ->
    Format.fprintf fmt
    "@{<error>Error:@} %s found in two directories: (%s, %s)\n\
    File names must be unique per project"
      modname dir1 dir2
  | Not_consistent modname ->     
    Format.fprintf fmt 
    "@{<error>Error:@} %s has implementation/interface in non-consistent syntax(reason/ocaml)" modname
  | No_implementation (modname) ->     
    Format.fprintf fmt 
    "@{<error>Error:@} %s does not have implementation file" modname
  | Package_not_found (name,json_opt) ->
    let in_json = match json_opt with
    | None -> Ext_string.empty
    | Some x -> " in " ^ x in
    let name = Bsb_pkg_types.to_string name in 
    if Ext_string.equal name Bs_version.package_name then
      Format.fprintf fmt
      "File \"bsconfig.json\", line 1\n\
       @{<error>Error:@} package @{<error>bs-platform@} is not found %s\n\
       It's the basic, required package. If you have it installed globally,\n\
       Please run `npm link bs-platform` to make it available" in_json
    else
      Format.fprintf fmt
        "File \"bsconfig.json\", line 1\n\
         @{<error>Error:@} package @{<error>%s@} not found or built %s\n\
         - Did you install it?\n\
         - If you did, did you run `bsb -make-world`?"
         name
         in_json

  | Json_config (pos,s) ->
    Format.fprintf fmt "File \"bsconfig.json\", line %d:\n\
                        @{<error>Error:@} %s \n\
                        For more details, please checkout the schema http://bucklescript.github.io/bucklescript/docson/#build-schema.json"
                        pos.pos_lnum s
  | Invalid_spec s ->
    Format.fprintf fmt
    "@{<error>Error: Invalid bsconfig.json %s@}" s
  | Invalid_json s ->
    Format.fprintf fmt
    "File %S, line 1\n\
    @{<error>Error: Invalid json format@}" s
  | Missing_object_file name ->
    Format.fprintf fmt
    "@{<error>Error:@} build.ninja is missing the file '%s' that was used in the project. Try force-regenerating but this shouldn't happen.\n"
    name
  | Missing_entry name ->
    Format.fprintf fmt
    "@{<error>Error:@} Could not find an item in the entries field to compile to '%s'\n"
    name
  | No_files_to_pack suffix ->
    Format.fprintf fmt
    "@{<error>Error:@} No %s to pack into a lib.\n"
    suffix

let conflict_module modname dir1 dir2 =
  error (Conflict_module (modname,dir1,dir2))
let no_implementation modname =   
  error (No_implementation modname)
let not_consistent modname =   
  error (Not_consistent modname)
let errorf ~loc fmt =
  Format.ksprintf (fun s -> error (Json_config (loc,s))) fmt

let missing_object_file name = error (Missing_object_file name)
let missing_entry name = error (Missing_entry name)
let no_files_to_pack suffix = error (No_files_to_pack suffix)

let config_error config fmt =
  let loc = Ext_json.loc_of config in

  error (Json_config (loc,fmt))

let invalid_spec s = error (Invalid_spec s)

let invalid_json s = error (Invalid_json s)

let () =
  Printexc.register_printer (fun x ->
      match x with
      | Error x ->
        Some (Format.asprintf "%a" print x )
      | _ -> None
    )

end
module Ext_buffer : sig 
#1 "ext_buffer.mli"
(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*  Pierre Weis and Xavier Leroy, projet Cristal, INRIA Rocquencourt   *)
(*                                                                     *)
(*  Copyright 1999 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)

(** Extensible buffers.

   This module implements buffers that automatically expand
   as necessary.  It provides accumulative concatenation of strings
   in quasi-linear time (instead of quadratic time when strings are
   concatenated pairwise).
*)

(* BuckleScript customization: customized for efficient digest *)

type t
(** The abstract type of buffers. *)

val create : int -> t
(** [create n] returns a fresh buffer, initially empty.
   The [n] parameter is the initial size of the internal byte sequence
   that holds the buffer contents. That byte sequence is automatically
   reallocated when more than [n] characters are stored in the buffer,
   but shrinks back to [n] characters when [reset] is called.
   For best performance, [n] should be of the same order of magnitude
   as the number of characters that are expected to be stored in
   the buffer (for instance, 80 for a buffer that holds one output
   line).  Nothing bad will happen if the buffer grows beyond that
   limit, however. In doubt, take [n = 16] for instance.
   If [n] is not between 1 and {!Sys.max_string_length}, it will
   be clipped to that interval. *)

val contents : t -> string
(** Return a copy of the current contents of the buffer.
    The buffer itself is unchanged. *)

val length : t -> int
(** Return the number of characters currently contained in the buffer. *)

val is_empty : t -> bool

(* val clear : t -> unit *)
(** Empty the buffer. *)


val add_char : t -> char -> unit
(** [add_char b c] appends the character [c] at the end of the buffer [b]. *)

val add_string : t -> string -> unit
(** [add_string b s] appends the string [s] at the end of the buffer [b]. *)

(* val add_bytes : t -> bytes -> unit *)
(** [add_string b s] appends the string [s] at the end of the buffer [b].
    @since 4.02 *)

(* val add_substring : t -> string -> int -> int -> unit *)
(** [add_substring b s ofs len] takes [len] characters from offset
   [ofs] in string [s] and appends them at the end of the buffer [b]. *)

(* val add_subbytes : t -> bytes -> int -> int -> unit *)
(** [add_substring b s ofs len] takes [len] characters from offset
    [ofs] in byte sequence [s] and appends them at the end of the buffer [b].
    @since 4.02 *)

(* val add_buffer : t -> t -> unit *)
(** [add_buffer b1 b2] appends the current contents of buffer [b2]
   at the end of buffer [b1].  [b2] is not modified. *)    

(* val add_channel : t -> in_channel -> int -> unit *)
(** [add_channel b ic n] reads exactly [n] character from the
   input channel [ic] and stores them at the end of buffer [b].
   Raise [End_of_file] if the channel contains fewer than [n]
   characters. *)

val output_buffer : out_channel -> t -> unit
(** [output_buffer oc b] writes the current contents of buffer [b]
   on the output channel [oc]. *)   

val digest : t -> Digest.t   

val not_equal : 
  t -> 
  string -> 
  bool 

val add_int_1 :    
   t -> int -> unit 

val add_int_2 :    
   t -> int -> unit 

val add_int_3 :    
   t -> int -> unit 

val add_int_4 :    
   t -> int -> unit 

val add_string_char :    
   t -> 
   string ->
   char -> 
   unit

val add_char_string :    
   t -> 
   char -> 
   string -> 
   unit
end = struct
#1 "ext_buffer.ml"
(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*    Pierre Weis and Xavier Leroy, projet Cristal, INRIA Rocquencourt    *)
(*                                                                        *)
(*   Copyright 1999 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Extensible buffers *)

type t =
 {mutable buffer : bytes;
  mutable position : int;
  mutable length : int;
  initial_buffer : bytes}

let create n =
 let n = if n < 1 then 1 else n in
 
 let n = if n > Sys.max_string_length then Sys.max_string_length else n in
 
 let s = Bytes.create n in
 {buffer = s; position = 0; length = n; initial_buffer = s}

let contents b = Bytes.sub_string b.buffer 0 b.position
(* let to_bytes b = Bytes.sub b.buffer 0 b.position  *)

(* let sub b ofs len =
  if ofs < 0 || len < 0 || ofs > b.position - len
  then invalid_arg "Ext_buffer.sub"
  else Bytes.sub_string b.buffer ofs len *)


(* let blit src srcoff dst dstoff len =
  if len < 0 || srcoff < 0 || srcoff > src.position - len
             || dstoff < 0 || dstoff > (Bytes.length dst) - len
  then invalid_arg "Ext_buffer.blit"
  else
    Bytes.unsafe_blit src.buffer srcoff dst dstoff len *)

let length b = b.position
let is_empty b = b.position = 0
(* let clear b = b.position <- 0 *)

(* let reset b =
  b.position <- 0; b.buffer <- b.initial_buffer;
  b.length <- Bytes.length b.buffer *)

let resize b more =
  let len = b.length in
  let new_len = ref len in
  while b.position + more > !new_len do new_len := 2 * !new_len done;
   
  if !new_len > Sys.max_string_length then begin
    if b.position + more <= Sys.max_string_length
    then new_len := Sys.max_string_length
    else failwith "Ext_buffer.add: cannot grow buffer"
  end;
  
  let new_buffer = Bytes.create !new_len in
  (* PR#6148: let's keep using [blit] rather than [unsafe_blit] in
     this tricky function that is slow anyway. *)
  Bytes.blit b.buffer 0 new_buffer 0 b.position;
  b.buffer <- new_buffer;
  b.length <- !new_len ;
  assert (b.position + more <= b.length)

let add_char b c =
  let pos = b.position in
  if pos >= b.length then resize b 1;
  Bytes.unsafe_set b.buffer pos c;
  b.position <- pos + 1  

(* let add_substring b s offset len =
  if offset < 0 || len < 0 || offset > String.length s - len
  then invalid_arg "Ext_buffer.add_substring/add_subbytes";
  let new_position = b.position + len in
  if new_position > b.length then resize b len;
  Ext_bytes.unsafe_blit_string s offset b.buffer b.position len;
  b.position <- new_position   *)


(* let add_subbytes b s offset len =
  add_substring b (Bytes.unsafe_to_string s) offset len *)

let add_string b s =
  let len = String.length s in
  let new_position = b.position + len in
  if new_position > b.length then resize b len;
  Ext_bytes.unsafe_blit_string s 0 b.buffer b.position len;
  b.position <- new_position  

(* TODO: micro-optimzie *)
let add_string_char b s c =
  let s_len = String.length s in
  let len = s_len + 1 in 
  let new_position = b.position + len in
  if new_position > b.length then resize b len;
  let b_buffer = b.buffer in 
  Ext_bytes.unsafe_blit_string s 0 b_buffer b.position s_len;
  Bytes.unsafe_set b_buffer (new_position - 1) c;
  b.position <- new_position 

let add_char_string b c s  =
  let s_len = String.length s in
  let len = s_len + 1 in 
  let new_position = b.position + len in
  if new_position > b.length then resize b len;
  let b_buffer = b.buffer in 
  let b_position = b.position in 
  Bytes.unsafe_set b_buffer b_position c ; 
  Ext_bytes.unsafe_blit_string s 0 b_buffer (b_position + 1) s_len;
  b.position <- new_position


(* let add_bytes b s = add_string b (Bytes.unsafe_to_string s)

let add_buffer b bs =
  add_subbytes b bs.buffer 0 bs.position *)

(* let add_channel b ic len =
  if len < 0 
    || len > Sys.max_string_length 
    then   (* PR#5004 *)
    invalid_arg "Ext_buffer.add_channel";
  if b.position + len > b.length then resize b len;
  really_input ic b.buffer b.position len;
  b.position <- b.position + len *)

let output_buffer oc b =
  output oc b.buffer 0 b.position  

external unsafe_string: bytes -> int -> int -> Digest.t = "caml_md5_string"

let digest b = 
  unsafe_string 
  b.buffer 0 b.position    

let rec not_equal_aux (b : bytes) (s : string) i len = 
    if i >= len then false
    else 
      (Bytes.unsafe_get b i 
      <>
      String.unsafe_get s i )
      || not_equal_aux b s (i + 1) len 

(** avoid a large copy *)
let not_equal  (b : t) (s : string) = 
  let b_len = b.position in 
  let s_len = String.length s in 
  b_len <> s_len 
  || not_equal_aux b.buffer s 0 s_len


(**
  It could be one byte, two bytes, three bytes and four bytes 
  TODO: inline for better performance
*)
let add_int_1 (b : t ) (x : int ) = 
  let c = (Char.unsafe_chr (x land 0xff)) in 
  let pos = b.position in
  if pos >= b.length then resize b 1;
  Bytes.unsafe_set b.buffer pos c;
  b.position <- pos + 1  
  
let add_int_2 (b : t ) (x : int ) = 
  let c1 = (Char.unsafe_chr (x land 0xff)) in 
  let c2 = (Char.unsafe_chr (x lsr 8 land 0xff)) in   
  let pos = b.position in
  if pos + 1 >= b.length then resize b 2;
  let b_buffer = b.buffer in 
  Bytes.unsafe_set b_buffer pos c1;
  Bytes.unsafe_set b_buffer (pos + 1) c2;
  b.position <- pos + 2

let add_int_3 (b : t ) (x : int ) = 
  let c1 = (Char.unsafe_chr (x land 0xff)) in 
  let c2 = (Char.unsafe_chr (x lsr 8 land 0xff)) in   
  let c3 = (Char.unsafe_chr (x lsr 16 land 0xff)) in
  let pos = b.position in
  if pos + 2 >= b.length then resize b 3;
  let b_buffer = b.buffer in 
  Bytes.unsafe_set b_buffer pos c1;
  Bytes.unsafe_set b_buffer (pos + 1) c2;
  Bytes.unsafe_set b_buffer (pos + 2) c3;
  b.position <- pos + 3


let add_int_4 (b : t ) (x : int ) = 
  let c1 = (Char.unsafe_chr (x land 0xff)) in 
  let c2 = (Char.unsafe_chr (x lsr 8 land 0xff)) in   
  let c3 = (Char.unsafe_chr (x lsr 16 land 0xff)) in
  let c4 = (Char.unsafe_chr (x lsr 24 land 0xff)) in
  let pos = b.position in
  if pos + 3 >= b.length then resize b 4;
  let b_buffer = b.buffer in 
  Bytes.unsafe_set b_buffer pos c1;
  Bytes.unsafe_set b_buffer (pos + 1) c2;
  Bytes.unsafe_set b_buffer (pos + 2) c3;
  Bytes.unsafe_set b_buffer (pos + 3) c4;
  b.position <- pos + 4




end
module Ext_filename : sig 
#1 "ext_filename.mli"
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





(* TODO:
   Change the module name, this code is not really an extension of the standard 
    library but rather specific to JS Module name convention. 
*)





(** An extension module to calculate relative path follow node/npm style. 
    TODO : this short name will have to change upon renaming the file.
*)

val is_dir_sep : 
  char -> bool 
  
val maybe_quote:
  string -> 
  string

val chop_extension_maybe:
  string -> 
  string

(* return an empty string if no extension found *)  
val get_extension_maybe:   
  string -> 
  string


val new_extension:  
  string -> 
  string -> 
  string

val chop_all_extensions_maybe:
  string -> 
  string  

(* OCaml specific abstraction*)
val module_name:  
  string ->
  string




type module_info = {
  module_name : string ;
  case : bool;
}   



val as_module:
  basename:string -> 
  module_info option
end = struct
#1 "ext_filename.ml"
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




let is_dir_sep_unix c = c = '/'
let is_dir_sep_win_cygwin c = 
  c = '/' || c = '\\' || c = ':'

let is_dir_sep = 
  if Sys.unix then is_dir_sep_unix else is_dir_sep_win_cygwin

(* reference ninja.cc IsKnownShellSafeCharacter *)
let maybe_quote ( s : string) = 
  let noneed_quote = 
    Ext_string.for_all s (function
        | '0' .. '9' 
        | 'a' .. 'z' 
        | 'A' .. 'Z'
        | '_' | '+' 
        | '-' | '.'
        | '/' 
        | '@' -> true
        | _ -> false
      )  in 
  if noneed_quote then
    s
  else Filename.quote s 


let chop_extension_maybe name =
  let rec search_dot i =
    if i < 0 || is_dir_sep (String.unsafe_get name i) then name
    else if String.unsafe_get name i = '.' then String.sub name 0 i
    else search_dot (i - 1) in
  search_dot (String.length name - 1)

let get_extension_maybe name =   
  let name_len = String.length name in  
  let rec search_dot name i name_len =
    if i < 0 || is_dir_sep (String.unsafe_get name i) then ""
    else if String.unsafe_get name i = '.' then String.sub name i (name_len - i)
    else search_dot name (i - 1) name_len in
  search_dot name (name_len - 1) name_len

let chop_all_extensions_maybe name =
  let rec search_dot i last =
    if i < 0 || is_dir_sep (String.unsafe_get name i) then 
      (match last with 
      | None -> name
      | Some i -> String.sub name 0 i)  
    else if String.unsafe_get name i = '.' then 
      search_dot (i - 1) (Some i)
    else search_dot (i - 1) last in
  search_dot (String.length name - 1) None


let new_extension name (ext : string) = 
  let rec search_dot name i ext =
    if i < 0 || is_dir_sep (String.unsafe_get name i) then 
      name ^ ext 
    else if String.unsafe_get name i = '.' then 
      let ext_len = String.length ext in
      let buf = Bytes.create (i + ext_len) in 
      Bytes.blit_string name 0 buf 0 i;
      Bytes.blit_string ext 0 buf i ext_len;
      Bytes.unsafe_to_string buf
    else search_dot name (i - 1) ext  in
  search_dot name (String.length name - 1) ext



(** TODO: improve efficiency
   given a path, calcuate its module name 
   Note that `ocamlc.opt -c aa.xx.mli` gives `aa.xx.cmi`
   we can not strip all extensions, otherwise
   we can not tell the difference between "x.cpp.ml" 
   and "x.ml"
*)
let module_name name = 
  let rec search_dot i  name =
    if i < 0  then 
      Ext_string.capitalize_ascii name
    else 
    if String.unsafe_get name i = '.' then 
      Ext_string.capitalize_sub name i 
    else 
      search_dot (i - 1) name in  
  let name = Filename.basename  name in 
  let name_len = String.length name in 
  search_dot (name_len - 1)  name 

type module_info = {
  module_name : string ;
  case : bool;
} 



let rec valid_module_name_aux name off len =
  if off >= len then true 
  else 
    let c = String.unsafe_get name off in 
    match c with 
    | 'A'..'Z' | 'a'..'z' | '0'..'9' | '_' | '\'' -> 
      valid_module_name_aux name (off + 1) len 
    | _ -> false

type state = 
  | Invalid
  | Upper
  | Lower

let valid_module_name name len =     
  if len = 0 then Invalid
  else 
    let c = String.unsafe_get name 0 in 
    match c with 
    | 'A' .. 'Z'
      -> 
      if valid_module_name_aux name 1 len then 
        Upper
      else Invalid  
    | 'a' .. 'z' 
      -> 
      if valid_module_name_aux name 1 len then
        Lower
      else Invalid
    | _ -> Invalid


let as_module ~basename =
  let rec search_dot i  name name_len =
    if i < 0  then
      (* Input e.g, [a_b] *)
      match valid_module_name name name_len with 
      | Invalid -> None 
      | Upper ->  Some {module_name = name; case = true }
      | Lower -> Some {module_name = Ext_string.capitalize_ascii name; case = false}
    else 
    if String.unsafe_get name i = '.' then 
      (*Input e.g, [A_b] *)
      match valid_module_name  name i with 
      | Invalid -> None 
      | Upper -> 
        Some {module_name = Ext_string.capitalize_sub name i; case = true}
      | Lower -> 
        Some {module_name = Ext_string.capitalize_sub name i; case = false}
    else 
      search_dot (i - 1) name name_len in  
  let name_len = String.length basename in       
  search_dot (name_len - 1)  basename name_len
    
end
module Ext_js_file_kind
= struct
#1 "ext_js_file_kind.ml"
(* Copyright (C) 2020- Authors of BuckleScript
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

type t = 
  | Upper_js
  | Upper_bs
  | Little_js 
  | Little_bs


end
module Ext_namespace : sig 
#1 "ext_namespace.mli"
(* Copyright (C) 2017- Authors of BuckleScript
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



val try_split_module_name :
  string -> (string * string ) option



(* Note  we have to output uncapitalized file Name, 
   or at least be consistent, since by reading cmi file on Case insensitive OS, we don't really know it is `list.cmi` or `List.cmi`, so that `require (./list.js)` or `require(./List.js)`
   relevant issues: #1609, #913  

   #1933 when removing ns suffix, don't pass the bound
   of basename
*)
val change_ext_ns_suffix :  
  string -> 
  string ->
  string


  
(** [js_name_of_modulename ~little A-Ns]
  *)
val js_name_of_modulename : 
  string -> 
  Ext_js_file_kind.t -> 
  string

(* TODO handle cases like 
   '@angular/core'
   its directory structure is like 
   {[
     @angular
     |-------- core
   ]}
*)
val is_valid_npm_package_name : string -> bool 

val namespace_of_package_name : string -> string

end = struct
#1 "ext_namespace.ml"

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





let rec rindex_rec s i  =
  if i < 0 then i else
    let char = String.unsafe_get s i in
    if Ext_filename.is_dir_sep char  then -1 
    else if char = Literals.ns_sep_char then i 
    else
      rindex_rec s (i - 1) 

let change_ext_ns_suffix name ext =
  let i = rindex_rec name (String.length name - 1)  in 
  if i < 0 then name ^ ext
  else String.sub name 0 i ^ ext (* FIXME: micro-optimizaiton*)

let try_split_module_name name = 
  let len = String.length name in 
  let i = rindex_rec name (len - 1)  in 
  if i < 0 then None 
  else 
    Some (String.sub name (i+1) (len - i - 1),
          String.sub name 0 i )



  
(* let js_name_of_basename bs_suffix s =   
  change_ext_ns_suffix  s 
  (if bs_suffix then Literals.suffix_bs_js else  Literals.suffix_js ) *)

let js_name_of_modulename s (little : Ext_js_file_kind.t) : string = 
  match little with 
  | Little_js -> 
    change_ext_ns_suffix (Ext_string.uncapitalize_ascii s)  Literals.suffix_js
  | Little_bs -> 
    change_ext_ns_suffix (Ext_string.uncapitalize_ascii s)  Literals.suffix_bs_js
  | Upper_js ->
    change_ext_ns_suffix s  Literals.suffix_js
  | Upper_bs -> 
    change_ext_ns_suffix s  Literals.suffix_bs_js

(* https://docs.npmjs.com/files/package.json 
   Some rules:
   The name must be less than or equal to 214 characters. This includes the scope for scoped packages.
   The name can't start with a dot or an underscore.
   New packages must not have uppercase letters in the name.
   The name ends up being part of a URL, an argument on the command line, and a folder name. Therefore, the name can't contain any non-URL-safe characters.
*)
let is_valid_npm_package_name (s : string) = 
  let len = String.length s in 
  len <= 214 && (* magic number forced by npm *)
  len > 0 &&
  match String.unsafe_get s 0 with 
  | 'a' .. 'z' | '@' -> 
    Ext_string.for_all_from s 1 
      (fun x -> 
         match x with 
         |  'a'..'z' | '0'..'9' | '_' | '-' -> true
         | _ -> false )
  | _ -> false 


let namespace_of_package_name (s : string) : string = 
  let len = String.length s in 
  let buf = Ext_buffer.create len in 
  let add capital ch = 
    Ext_buffer.add_char buf 
      (if capital then 
         (Char.uppercase_ascii ch)
       else ch) in    
  let rec aux capital off len =     
    if off >= len then ()
    else 
      let ch = String.unsafe_get s off in
      match ch with 
      | 'a' .. 'z' 
      | 'A' .. 'Z' 
      | '0' .. '9'
      | '_'
        ->
        add capital ch ; 
        aux false (off + 1) len 
      | '/'
      | '-' -> 
        aux true (off + 1) len 
      | _ -> aux capital (off+1) len
  in 
  aux true 0 len ;
  Ext_buffer.contents buf 

end
module Bsb_package_specs : sig 
#1 "bsb_package_specs.mli"
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

type t


val default_package_specs : t

val from_json:
  Ext_json_types.t -> t 

val get_list_of_output_js : 
  t -> bool -> string -> string list

(**
  Sample output: {[ -bs-package-output commonjs:lib/js/jscomp/test]}
*)
val package_flag_of_package_specs : 
  t -> string -> string

val list_dirs_by :   
  t -> 
  (string -> unit) -> 
  unit
end = struct
#1 "bsb_package_specs.ml"
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


let (//) = Ext_path.combine 



(* TODO: sync up with {!Js_packages_info.module_system}  *)
type format = 
  | NodeJS | Es6 | Es6_global

type spec = {
  format : format;
  in_source : bool 
}

module Spec_set = Set.Make( struct type t = spec 
    let compare = Pervasives.compare 
  end)

type t = Spec_set.t 


let bad_module_format_message_exn ~loc format =
  Bsb_exception.errorf ~loc "package-specs: `%s` isn't a valid output module format. It has to be one of:  %s, %s or %s"
    format
    Literals.commonjs
    Literals.es6
    Literals.es6_global

let supported_format (x : string) loc = 
  if x = Literals.commonjs then NodeJS
  else if x = Literals.es6 then Es6
  else if x = Literals.es6_global then Es6_global
  else bad_module_format_message_exn ~loc x 

let string_of_format (x : format) =
  match x with 
  | NodeJS -> Literals.commonjs
  | Es6 -> Literals.es6
  | Es6_global -> Literals.es6_global

let prefix_of_format (x : format)  =   
  (match x with 
  | NodeJS -> Bsb_config.lib_js 
  | Es6 -> Bsb_config.lib_es6 
  | Es6_global -> Bsb_config.lib_es6_global )

let rec from_array (arr : Ext_json_types.t array) : Spec_set.t =
  let spec = ref Spec_set.empty in
  let has_in_source = ref false in
  Ext_array.iter arr (fun x ->
      let result = from_json_single x  in
      if result.in_source then 
        (
          if not !has_in_source then
            has_in_source:= true
          else 
            Bsb_exception.errorf 
              ~loc:(Ext_json.loc_of x) 
              "package-specs: we've detected two module formats that are both configured to be in-source." 
        );
      spec := Spec_set.add result !spec
    );
  !spec

(* TODO: FIXME: better API without mutating *)
and from_json_single (x : Ext_json_types.t) : spec =
  match x with
  | Str {str = format; loc } ->    
      {format = supported_format format loc  ; in_source = false }    
  | Obj {map; loc} ->
    begin match Map_string.find_exn map "module" with
      | Str {str = format} ->
        let in_source = 
          match Map_string.find_opt map  Bsb_build_schemas.in_source with
          | Some (True _) -> true
          | Some _
          | None -> false
        in        
          {format = supported_format format loc ; in_source  }        
      | Arr _ ->
        Bsb_exception.errorf ~loc
          "package-specs: when the configuration is an object, `module` field should be a string, not an array. If you want to pass multiple module specs, try turning package-specs into an array of objects (or strings) instead."
      | _ ->
        Bsb_exception.errorf ~loc
          "package-specs: the `module` field of the configuration object should be a string."
      | exception _ ->
        Bsb_exception.errorf ~loc
          "package-specs: when the configuration is an object, the `module` field is mandatory."
    end
  | _ -> Bsb_exception.errorf ~loc:(Ext_json.loc_of x)
           "package-specs: we expect either a string or an object."

let  from_json (x : Ext_json_types.t) : Spec_set.t =
  match x with
  | Arr {content ; _} -> from_array content
  | _ -> Spec_set.singleton (from_json_single x )


let bs_package_output = "-bs-package-output"

(** Assume input is valid 
    {[ -bs-package-output commonjs:lib/js/jscomp/test ]}
*)
let package_flag ({format; in_source } : spec) dir =
  Ext_string.inter2
    bs_package_output 
    (Ext_string.concat3
       (string_of_format format)
       Ext_string.single_colon
       (if in_source then dir else
        prefix_of_format format // dir))

let package_flag_of_package_specs (package_specs : t) 
    (dirname : string ) : string  = 
  Spec_set.fold (fun format acc ->
      Ext_string.inter2 acc (package_flag format dirname )
    ) package_specs Ext_string.empty

let default_package_specs = 
  Spec_set.singleton 
    { format = NodeJS ; in_source = false }



(**
    [get_list_of_output_js specs "src/hi/hello"]

*)
let get_list_of_output_js 
    (package_specs : Spec_set.t)
    (bs_suffix : bool)
    (output_file_sans_extension : string)
    = 
  Spec_set.fold 
    (fun (spec : spec) acc ->
        let basename =  Ext_namespace.change_ext_ns_suffix
             output_file_sans_extension
             (if bs_suffix then Literals.suffix_bs_js else Literals.suffix_js)
        in 
        (Bsb_config.proj_rel @@ (if spec.in_source then basename
        else prefix_of_format spec.format // basename))         
       :: acc
    ) package_specs []


let list_dirs_by
  (package_specs : Spec_set.t)
  (f : string -> unit)
  =  
  Spec_set.iter (fun (spec : spec)  -> 
    if not spec.in_source then     
      f (prefix_of_format spec.format) 
  ) package_specs 
end
module Bsc_warnings
= struct
#1 "bsc_warnings.ml"
(* Copyright (C) 2020- Authors of BuckleScript 
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

 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)



(**
  See the meanings of the warning codes here: https://caml.inria.fr/pub/docs/manual-ocaml/comp.html#sec281

  - 30 Two labels or constructors of the same name are defined in two mutually recursive types.
  - 40 Constructor or label name used out of scope.

  - 6 Label omitted in function application.
  - 7 Method overridden.
  - 9 Missing fields in a record pattern. (*Not always desired, in some cases need [@@@warning "+9"] *)
  - 27 Innocuous unused variable: unused variable that is not bound with let nor as, and doesn’t start with an underscore (_) character.
  - 29 Unescaped end-of-line in a string constant (non-portable code).
  - 32 .. 39 Unused blabla
  - 44 Open statement shadows an already defined identifier.
  - 45 Open statement shadows an already defined label or constructor.
  - 48 Implicit elimination of optional arguments. https://caml.inria.fr/mantis/view.php?id=6352
  - 101 (bsb-specific) unsafe polymorphic comparison.
*) 


(*
  The purpose of default warning set is to make it strict while
  not annoy user too much

  -4 Fragile pattern matching: matching that will remain complete even if additional con- structors are added to one of the variant types matched.
  We turn it off since common pattern
  {[
    match x with | A -> .. |  _ -> false
  ]}

  -9 Missing fields in a record pattern.
  only in some special cases that we need all fields being listed

  We encourage people to write code based on type based disambigution
  40,41,42 are enabled for compatiblity reasons  
  -40 Constructor or label name used out of scope
  This is intentional, we should never warn it
  - 41 Ambiguous constructor or label name.
  It is turned off since it prevents such cases below:
  {[
    type a = A |B 
    type b = A | B | C
  ]}
  - 42 Disambiguated constructor or label name (compatibility warning).
  
  - 50 Unexpected documentation comment.

  - 102 Bs_polymorphic_comparison
*)
let defaults_w = "+a-4-9-40-41-42-50-61-102"
let defaults_warn_error = "-a+5+101";;
(*TODO: add +10*)

end
module Bsb_warning : sig 
#1 "bsb_warning.mli"
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




type t

(** Extra work is need to make merlin happy *)
val to_merlin_string : t  -> string



val from_map : Ext_json_types.t Map_string.t -> t 

(** [to_bsb_string not_dev warning]
*)
val to_bsb_string : 
  toplevel:bool -> 
  t  -> 
  string

val use_default : t
end = struct
#1 "bsb_warning.ml"
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


type warning_error =
  | Warn_error_false
  (* default [false] to make our changes non-intrusive *)
  | Warn_error_true
  | Warn_error_number of string

type t0 = {
  number : string option;
  error : warning_error
}

type nonrec t = t0 option 

let use_default = None

let prepare_warning_concat ~(beg : bool) s =
  let s = Ext_string.trim s in 
  if s = "" then s 
  else 
    match s.[0] with 
    | '0' .. '9' -> if beg then "-w +" ^ s else "+" ^ s 
    | 'a' .. 'z' -> 
      if beg then "-w " ^ s else "-" ^ s 
    | 'A' .. 'Z' -> 
      if beg then "-w " ^ s else "+" ^ s  
    | _ -> 
      if beg then "-w " ^ s else s

let to_merlin_string x =
  "-w " ^ Bsc_warnings.defaults_w
  ^
  (match x with
   | Some {number =None}
   | None ->  Ext_string.empty
   | Some {number = Some x} -> 
    prepare_warning_concat ~beg:false x )


   
let from_map (m : Ext_json_types.t Map_string.t) =
  let number_opt = Map_string.find_opt m Bsb_build_schemas.number in
  let error_opt = Map_string.find_opt m  Bsb_build_schemas.error in
  match number_opt, error_opt  with
  | None, None -> None
  | _, _ ->
    let error  =
      match error_opt with
      | Some (True _) -> Warn_error_true
      | Some (False _) -> Warn_error_false
      | Some (Str {str ; })
        -> Warn_error_number str
      | Some x -> Bsb_exception.config_error x "expect true/false or string"
      | None -> Warn_error_false
      (** To make it less intrusive : warning error has to be enabled*)
    in
    let number =
      match number_opt with
      | Some (Str { str = number}) -> Some number
      | None -> None
      | Some x -> Bsb_exception.config_error x "expect a string"
    in
    Some {number; error }


let to_bsb_string ~toplevel warning =
  if toplevel then
    match warning with
    | None -> Ext_string.empty
    | Some warning ->     
      (match warning.number with
       | None ->
         Ext_string.empty
       | Some x ->
         prepare_warning_concat ~beg:true x  
      ) ^
      (
        match warning.error with
        | Warn_error_true ->
          " -warn-error A"
        | Warn_error_number y ->
          " -warn-error " ^ y
        | Warn_error_false ->
          Ext_string.empty
      )
  else " -w a" 
  (* TODO: this is the current default behavior *)

end
module Bs_hash_stubs
= struct
#1 "bs_hash_stubs.ml"


external hash_string :  string -> int = "caml_bs_hash_string" [@@noalloc];;

external hash_string_int :  string -> int  -> int = "caml_bs_hash_string_and_int" [@@noalloc];;

external hash_string_small_int :  string -> int  -> int = "caml_bs_hash_string_and_small_int" [@@noalloc];;

external hash_stamp_and_name : int -> string -> int = "caml_bs_hash_stamp_and_name" [@@noalloc];;

external hash_small_int : int -> int = "caml_bs_hash_small_int" [@@noalloc];;

external hash_int :  int  -> int = "caml_bs_hash_int" [@@noalloc];;

external string_length_based_compare : string -> string -> int  = "caml_string_length_based_compare" [@@noalloc];;

external    
    int_unsafe_blit : 
    int array -> int -> int array -> int -> int -> unit = "caml_int_array_blit" [@@noalloc];;

    

end
module Ext_util : sig 
#1 "ext_util.mli"
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


 
val power_2_above : int -> int -> int


val stats_to_string : Hashtbl.statistics -> string 
end = struct
#1 "ext_util.ml"
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

(**
   {[
     (power_2_above 16 63 = 64)
       (power_2_above 16 76 = 128)
   ]}
*)
let rec power_2_above x n =
  if x >= n then x
  else if x * 2 > Sys.max_array_length then x
  else power_2_above (x * 2) n


let stats_to_string ({num_bindings; num_buckets; max_bucket_length; bucket_histogram} : Hashtbl.statistics) = 
  Printf.sprintf 
    "bindings: %d,buckets: %d, longest: %d, hist:[%s]" 
    num_bindings 
    num_buckets 
    max_bucket_length
    (String.concat "," (Array.to_list (Array.map string_of_int bucket_histogram)))
end
module Hash_set_gen
= struct
#1 "hash_set_gen.ml"
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


(* We do dynamic hashing, and resize the table and rehash the elements
   when buckets become too long. *)

type 'a bucket = 
  | Empty
  | Cons of {
      mutable key : 'a ; 
      mutable next : 'a bucket 
    }

type 'a t =
  { mutable size: int;                        (* number of entries *)
    mutable data: 'a bucket array;  (* the buckets *)
    initial_size: int;                        (* initial array size *)
  }




let create  initial_size =
  let s = Ext_util.power_2_above 16 initial_size in
  { initial_size = s; size = 0; data = Array.make s Empty }

let clear h =
  h.size <- 0;
  let len = Array.length h.data in
  for i = 0 to len - 1 do
    Array.unsafe_set h.data i  Empty
  done

let reset h =
  h.size <- 0;
  h.data <- Array.make h.initial_size Empty

let length h = h.size

let resize indexfun h =
  let odata = h.data in
  let osize = Array.length odata in
  let nsize = osize * 2 in
  if nsize < Sys.max_array_length then begin
    let ndata = Array.make nsize Empty in
    let ndata_tail = Array.make nsize Empty in 
    h.data <- ndata;          (* so that indexfun sees the new bucket count *)
    let rec insert_bucket = function
        Empty -> ()
      | Cons {key; next} as cell ->
        let nidx = indexfun h key in
        begin match Array.unsafe_get ndata_tail nidx with 
        | Empty ->
          Array.unsafe_set ndata nidx cell
        | Cons tail -> 
          tail.next <- cell
        end;
        Array.unsafe_set ndata_tail nidx  cell;          
        insert_bucket next
    in
    for i = 0 to osize - 1 do
      insert_bucket (Array.unsafe_get odata i)
    done;
    for i = 0 to nsize - 1 do 
      match Array.unsafe_get ndata_tail i with 
      | Empty -> ()
      | Cons tail -> tail.next <- Empty
    done 
  end

let iter h f =
  let rec do_bucket = function
    | Empty ->
      ()
    | Cons l  ->
      f l.key  ; do_bucket l.next in
  let d = h.data in
  for i = 0 to Array.length d - 1 do
    do_bucket (Array.unsafe_get d i)
  done

let fold h init f =
  let rec do_bucket b accu =
    match b with
      Empty ->
      accu
    | Cons l  ->
      do_bucket l.next (f l.key  accu) in
  let d = h.data in
  let accu = ref init in
  for i = 0 to Array.length d - 1 do
    accu := do_bucket (Array.unsafe_get d i) !accu
  done;
  !accu


let elements set = 
  fold set [] List.cons




let rec small_bucket_mem eq key lst =
  match lst with 
  | Empty -> false 
  | Cons lst -> 
    eq key lst.key ||
    match lst.next with 
    | Empty -> false 
    | Cons lst  -> 
      eq key   lst.key ||
      match lst.next with 
      | Empty -> false 
      | Cons lst  -> 
        eq key lst.key ||
        small_bucket_mem eq key lst.next 

let rec remove_bucket 
    (h : _ t) (i : int)
    key 
    ~(prec : _ bucket) 
    (buck : _ bucket) 
    eq_key = 
  match buck with   
  | Empty ->
    ()
  | Cons {key=k; next } ->
    if eq_key k key 
    then begin
      h.size <- h.size - 1;
      match prec with
      | Empty -> Array.unsafe_set h.data i  next
      | Cons c -> c.next <- next
    end
    else remove_bucket h i key ~prec:buck next eq_key


module type S =
sig
  type key
  type t
  val create: int ->  t
  val clear : t -> unit
  val reset : t -> unit
  (* val copy: t -> t *)
  val remove:  t -> key -> unit
  val add :  t -> key -> unit
  val of_array : key array -> t 
  val check_add : t -> key -> bool
  val mem : t -> key -> bool
  val iter: t -> (key -> unit) -> unit
  val fold: t -> 'b  -> (key -> 'b -> 'b) -> 'b
  val length:  t -> int
  (* val stats:  t -> Hashtbl.statistics *)
  val elements : t -> key list 
end



end
module Hash_set_string : sig 
#1 "hash_set_string.mli"
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


include Hash_set_gen.S with type key = string

end = struct
#1 "hash_set_string.ml"
# 1 "ext/hash_set.cppo.ml"
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
[@@@warning "-32"] (* FIXME *)
# 32 "ext/hash_set.cppo.ml"
type key = string 
let key_index (h :  _ Hash_set_gen.t ) (key : key) =
  (Bs_hash_stubs.hash_string  key) land (Array.length h.data - 1)
let eq_key = Ext_string.equal 
type  t = key  Hash_set_gen.t 


# 65 "ext/hash_set.cppo.ml"
let create = Hash_set_gen.create
let clear = Hash_set_gen.clear
let reset = Hash_set_gen.reset
(* let copy = Hash_set_gen.copy *)
let iter = Hash_set_gen.iter
let fold = Hash_set_gen.fold
let length = Hash_set_gen.length
(* let stats = Hash_set_gen.stats *)
let elements = Hash_set_gen.elements



let remove (h : _ Hash_set_gen.t ) key =
  let i = key_index h key in
  let h_data = h.data in 
  Hash_set_gen.remove_bucket h i key ~prec:Empty (Array.unsafe_get h_data i) eq_key    



let add (h : _ Hash_set_gen.t) key =
  let i = key_index h key  in 
  let h_data = h.data in 
  let old_bucket = (Array.unsafe_get h_data i) in
  if not (Hash_set_gen.small_bucket_mem eq_key key old_bucket) then 
    begin 
      Array.unsafe_set h_data i (Cons {key = key ; next =  old_bucket});
      h.size <- h.size + 1 ;
      if h.size > Array.length h_data lsl 1 then Hash_set_gen.resize key_index h
    end

let of_array arr = 
  let len = Array.length arr in 
  let tbl = create len in 
  for i = 0 to len - 1  do
    add tbl (Array.unsafe_get arr i);
  done ;
  tbl 
  
    
let check_add (h : _ Hash_set_gen.t) key : bool =
  let i = key_index h key  in 
  let h_data = h.data in  
  let old_bucket = (Array.unsafe_get h_data i) in
  if not (Hash_set_gen.small_bucket_mem eq_key key old_bucket) then 
    begin 
      Array.unsafe_set h_data i  (Cons { key = key ; next =  old_bucket});
      h.size <- h.size + 1 ;
      if h.size > Array.length h_data lsl 1 then Hash_set_gen.resize key_index h;
      true 
    end
  else false 


let mem (h :  _ Hash_set_gen.t) key =
  Hash_set_gen.small_bucket_mem eq_key key (Array.unsafe_get h.data (key_index h key)) 

  

end
module Bsb_config_types
= struct
#1 "bsb_config_types.ml"
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


type dependency = 
  {
    package_name : Bsb_pkg_types.t ; 
    package_install_path : string ; 
  }
type dependencies = dependency list 

(* `string` is a path to the entrypoint *)
type entries_t = JsTarget of string | NativeTarget of string | BytecodeTarget of string

type compilation_kind_t = Bytecode | Native

type reason_react_jsx = 
  | Jsx_v2
  | Jsx_v3
  (* string option  *)

type refmt = string option

type gentype_config = {
  path : string (* resolved *)
}
type command = string

type ppx = {
  name : string;
  args : string list
}
type t = 
  {
    package_name : string ; 
    (* [captial-package] *)
    namespace : string option; 
    (* CapitalPackage *)
    external_includes : string list ; 
    bsc_flags : string list ;
    ppx_files : ppx list ;
    pp_file : string option;
    bs_dependencies : dependencies;
    bs_dev_dependencies : dependencies;
    built_in_dependency : dependency option; 
    warning : Bsb_warning.t;
    (*TODO: maybe we should always resolve bs-platform 
      so that we can calculate correct relative path in 
      [.merlin]
    *)
    refmt : refmt;
    js_post_build_cmd : string option;
    package_specs : Bsb_package_specs.t ; 
    file_groups : Bsb_file_groups.t;
    files_to_install : Hash_set_string.t ;
    generate_merlin : bool ; 
    reason_react_jsx : reason_react_jsx option; (* whether apply PPX transform or not*)
    entries : entries_t list ;
    generators : command Map_string.t ; 
    cut_generators : bool; (* note when used as a dev mode, we will always ignore it *)
    bs_suffix : bool ; (* true means [.bs.js] we should pass [-bs-suffix] flag *)
    gentype_config : gentype_config option;
    number_of_dev_groups : int;

    static_libraries: string list;
    c_linker_flags: string list;
    otherlibs: string list;
    dev_otherlibs: string list;
  }

end
module Bsb_default : sig 
#1 "bsb_default.mli"
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





val main_entries : Bsb_config_types.entries_t list

val ocaml_flags : string list

val filter_otherlibs : string -> bool

end = struct
#1 "bsb_default.ml"
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







let main_entries = []

let ocaml_flags = ["-g"; "-bin-annot"; "-color"; "always"]

let filter_otherlibs s = match s with 
  | "unix" | "bigarray" | "str" | "nums" | "threads" | "dynlink" | "compiler-libs" -> true
  | _ -> false

end
module Ext_color : sig 
#1 "ext_color.mli"
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

type color 
  = Black
  | Red
  | Green
  | Yellow
  | Blue
  | Magenta
  | Cyan
  | White

type style 
  = FG of color 
  | BG of color 
  | Bold
  | Dim

(** Input is the tag for example `@{<warning>@}` return escape code *)
val ansi_of_tag : string -> string 

val reset_lit : string

end = struct
#1 "ext_color.ml"
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




type color 
  = Black
  | Red
  | Green
  | Yellow
  | Blue
  | Magenta
  | Cyan
  | White

type style 
  = FG of color 
  | BG of color 
  | Bold
  | Dim


(* let ansi_of_color = function
  | Black -> "0"
  | Red -> "1"
  | Green -> "2"
  | Yellow -> "3"
  | Blue -> "4"
  | Magenta -> "5"
  | Cyan -> "6"
  | White -> "7" *)

let code_of_style = function
  | FG Black -> "30"
  | FG Red -> "31"
  | FG Green -> "32"
  | FG Yellow -> "33"
  | FG Blue -> "34"
  | FG Magenta -> "35"
  | FG Cyan -> "36"
  | FG White -> "37"
  
  | BG Black -> "40"
  | BG Red -> "41"
  | BG Green -> "42"
  | BG Yellow -> "43"
  | BG Blue -> "44"
  | BG Magenta -> "45"
  | BG Cyan -> "46"
  | BG White -> "47"

  | Bold -> "1"
  | Dim -> "2"



(** TODO: add more styles later *)
let style_of_tag s = match s with
  | "error" -> [Bold; FG Red]
  | "warning" -> [Bold; FG Magenta]
  | "info" -> [Bold; FG Yellow]
  | "dim" -> [Dim]
  | "filename" -> [FG Cyan]
  | _ -> []

let ansi_of_tag s = 
  let l = style_of_tag s in
  let s =  String.concat ";" (Ext_list.map l  code_of_style) in
  "\x1b[" ^ s ^ "m"



let reset_lit = "\x1b[0m" 





end
module Bsb_log : sig 
#1 "bsb_log.mli"
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


val setup : unit -> unit 

type level = 
  | Debug
  | Info 
  | Warn
  | Error 

val log_level : level ref

type 'a fmt = Format.formatter -> ('a, Format.formatter, unit) format -> 'a

type 'a log = ('a, Format.formatter, unit) format -> 'a

val verbose : unit -> unit 
val debug  : 'a log
val info : 'a log 
val warn : 'a log 
val error : 'a log

val info_args : string array -> unit

end = struct
#1 "bsb_log.ml"
(* Copyright (C) 2017- Authors of BuckleScript
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



let ninja_ansi_forced = lazy 
  (try Sys.getenv "NINJA_ANSI_FORCED" with 
    Not_found  ->""
  )  
let color_enabled = lazy (Unix.isatty Unix.stdout)

(* same logic as [ninja.exe] *)
let get_color_enabled () = 
  let colorful = 
    match ninja_ansi_forced with 
    | lazy "1" -> true     
    | lazy ("0" | "false") -> false
    | _ ->
      Lazy.force color_enabled  in 
  colorful 



let color_functions : Format.formatter_tag_functions = {
  mark_open_tag = (fun s ->  if get_color_enabled () then  Ext_color.ansi_of_tag s else Ext_string.empty) ;
  mark_close_tag = (fun _ ->  if get_color_enabled () then Ext_color.reset_lit else Ext_string.empty);
  print_open_tag = (fun _ -> ());
  print_close_tag = (fun _ -> ())
}

(* let set_color ppf =
  Format.pp_set_formatter_tag_functions ppf color_functions *)


let setup () = 
  begin 
    Format.pp_set_mark_tags Format.std_formatter true ;
    Format.pp_set_mark_tags Format.err_formatter true;
    Format.pp_set_formatter_tag_functions 
      Format.std_formatter color_functions;
    Format.pp_set_formatter_tag_functions
      Format.err_formatter color_functions
  end

type level = 
  | Debug
  | Info 
  | Warn
  | Error 

let int_of_level (x : level) = 
  match x with 
  | Debug -> 0 
  | Info -> 1 
  | Warn -> 2 
  | Error -> 3 

let log_level = ref Warn

let verbose () =
   log_level := Debug
let dfprintf level fmt = 
  if int_of_level level >= int_of_level  !log_level then 
    Format.fprintf fmt 
  else Format.ifprintf fmt  

type 'a fmt = 
  Format.formatter -> ('a, Format.formatter, unit) format -> 'a
type 'a log = 
  ('a, Format.formatter, unit) format -> 'a

let debug fmt = dfprintf  Debug Format.std_formatter fmt 
let info fmt = dfprintf Info Format.std_formatter fmt
let warn fmt = dfprintf Warn Format.err_formatter fmt 
let error fmt = dfprintf Error Format.err_formatter fmt


let info_args (args : string array) = 
  if int_of_level Info >= int_of_level !log_level then 
    begin
      for i  = 0 to Array.length args - 1 do
        Format.pp_print_string Format.std_formatter (Array.unsafe_get args i) ;
        Format.pp_print_string Format.std_formatter Ext_string.single_space;
      done ;
      Format.pp_print_newline Format.std_formatter ()
    end
  else ()
  

end
module Bsb_real_path : sig 
#1 "bsb_real_path.mli"

(* Copyright (C) 2020- Authors of BuckleScript
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


val is_same_paths_via_io : string -> string -> bool 

end = struct
#1 "bsb_real_path.ml"
let (//) = Filename.concat

let getchdir s =
  let p = Sys.getcwd () in
  Unix.chdir s;
  p

let normalize s = getchdir (getchdir s)

let real_path p =
  match (try Some (Sys.is_directory p) with Sys_error _ -> None) with
  | None ->
    let rec resolve dir =
      if Sys.file_exists dir then normalize dir else
      let parent = Filename.dirname dir in
      if dir = parent then dir
      else  (resolve parent) // (Filename.basename dir)
    in
    let p =
      if Filename.is_relative p then (Sys.getcwd ()) // p
      else p
    in
    resolve p
  | Some true -> normalize p
  | Some false ->
    let dir = normalize (Filename.dirname p) in
    match Filename.basename p with
    | "." -> dir
    | base -> dir // base


let is_same_paths_via_io a b =
  if a = b
  then true
  else (real_path a) = (real_path b)

end
module Hash_gen
= struct
#1 "hash_gen.ml"
(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)

(* Hash tables *)




(* We do dynamic hashing, and resize the table and rehash the elements
   when buckets become too long. *)

type ('a, 'b) bucket =
  | Empty
  | Cons of {
      mutable key : 'a ; 
      mutable data : 'b ; 
      mutable next :  ('a, 'b) bucket
    }

type ('a, 'b) t =
  { mutable size: int;                        (* number of entries *)
    mutable data: ('a, 'b) bucket array;  (* the buckets *)
    initial_size: int;                        (* initial array size *)
  }



let create  initial_size =
  let s = Ext_util.power_2_above 16 initial_size in
  { initial_size = s; size = 0; data = Array.make s Empty }

let clear h =
  h.size <- 0;
  let len = Array.length h.data in
  for i = 0 to len - 1 do
    Array.unsafe_set h.data i  Empty  
  done

let reset h =
  h.size <- 0;
  h.data <- Array.make h.initial_size Empty


let length h = h.size

let resize indexfun h =
  let odata = h.data in
  let osize = Array.length odata in
  let nsize = osize * 2 in
  if nsize < Sys.max_array_length then begin
    let ndata = Array.make nsize Empty in
    let ndata_tail = Array.make nsize Empty in 
    h.data <- ndata;          (* so that indexfun sees the new bucket count *)
    let rec insert_bucket = function
        Empty -> ()
      | Cons {key; next} as cell ->
        let nidx = indexfun h key in
        begin match Array.unsafe_get ndata_tail nidx with 
        | Empty -> 
          Array.unsafe_set ndata nidx cell
        | Cons tail ->
          tail.next <- cell  
        end;
        Array.unsafe_set ndata_tail nidx cell;
        insert_bucket next
    in
    for i = 0 to osize - 1 do
      insert_bucket (Array.unsafe_get odata i)
    done;
    for i = 0 to nsize - 1 do 
      match Array.unsafe_get ndata_tail i with 
      | Empty -> ()  
      | Cons tail -> tail.next <- Empty
    done   
  end



let iter h f =
  let rec do_bucket = function
    | Empty ->
      ()
    | Cons l  ->
      f l.key l.data; do_bucket l.next in
  let d = h.data in
  for i = 0 to Array.length d - 1 do
    do_bucket (Array.unsafe_get d i)
  done

let fold h init f =
  let rec do_bucket b accu =
    match b with
      Empty ->
      accu
    | Cons l ->
      do_bucket l.next (f l.key l.data accu) in
  let d = h.data in
  let accu = ref init in
  for i = 0 to Array.length d - 1 do
    accu := do_bucket (Array.unsafe_get d i) !accu
  done;
  !accu

let to_list h f =
  fold h [] (fun k data acc -> f k data :: acc)  




let rec small_bucket_mem (lst : _ bucket) eq key  =
  match lst with 
  | Empty -> false 
  | Cons lst -> 
    eq  key lst.key ||
    match lst.next with
    | Empty -> false 
    | Cons lst -> 
      eq key lst.key  || 
      match lst.next with 
      | Empty -> false 
      | Cons lst -> 
        eq key lst.key  ||
        small_bucket_mem lst.next eq key 


let rec small_bucket_opt eq key (lst : _ bucket) : _ option =
  match lst with 
  | Empty -> None 
  | Cons lst -> 
    if eq  key lst.key then Some lst.data else 
      match lst.next with
      | Empty -> None 
      | Cons lst -> 
        if eq key lst.key then Some lst.data else 
          match lst.next with 
          | Empty -> None 
          | Cons lst -> 
            if eq key lst.key  then Some lst.data else 
              small_bucket_opt eq key lst.next


let rec small_bucket_key_opt eq key (lst : _ bucket) : _ option =
  match lst with 
  | Empty -> None 
  | Cons {key=k;  next} -> 
    if eq  key k then Some k else 
      match next with
      | Empty -> None 
      | Cons {key=k; next} -> 
        if eq key k then Some k else 
          match next with 
          | Empty -> None 
          | Cons {key=k; next} -> 
            if eq key k  then Some k else 
              small_bucket_key_opt eq key next


let rec small_bucket_default eq key default (lst : _ bucket) =
  match lst with 
  | Empty -> default 
  | Cons lst -> 
    if eq  key lst.key then  lst.data else 
      match lst.next with
      | Empty -> default 
      | Cons lst -> 
        if eq key lst.key then  lst.data else 
          match lst.next with 
          | Empty -> default 
          | Cons lst -> 
            if eq key lst.key  then lst.data else 
              small_bucket_default eq key default lst.next

let rec remove_bucket 
    h  (i : int)
    key 
    ~(prec : _ bucket) 
    (buck : _ bucket) 
    eq_key = 
  match buck with   
  | Empty ->
    ()
  | Cons {key=k; next }  ->
    if eq_key k key 
    then begin
      h.size <- h.size - 1;
      match prec with
      | Empty -> Array.unsafe_set h.data i  next
      | Cons c -> c.next <- next
    end
    else remove_bucket h i key ~prec:buck next eq_key

let rec replace_bucket key data (buck : _ bucket) eq_key = 
  match buck with   
  | Empty ->
    true
  | Cons slot ->
    if eq_key slot.key key
    then (slot.key <- key; slot.data <- data; false)
    else replace_bucket key data slot.next eq_key

module type S = sig 
  type key
  type 'a t
  val create: int -> 'a t
  val clear: 'a t -> unit
  val reset: 'a t -> unit

  val add: 'a t -> key -> 'a -> unit
  val add_or_update: 
    'a t -> 
    key -> 
    update:('a -> 'a) -> 
    'a -> unit 
  val remove: 'a t -> key -> unit
  val find_exn: 'a t -> key -> 'a
  val find_all: 'a t -> key -> 'a list
  val find_opt: 'a t -> key  -> 'a option

  (** return the key found in the hashtbl.
      Use case: when you find the key existed in hashtbl, 
      you want to use the one stored in the hashtbl. 
      (they are semantically equivlanent, but may have other information different) 
  *)
  val find_key_opt: 'a t -> key -> key option 

  val find_default: 'a t -> key -> 'a -> 'a 

  val replace: 'a t -> key -> 'a -> unit
  val mem: 'a t -> key -> bool
  val iter: 'a t -> (key -> 'a -> unit) -> unit
  val fold: 
    'a t -> 'b ->
    (key -> 'a -> 'b -> 'b) ->  'b
  val length: 'a t -> int
  (* val stats: 'a t -> Hashtbl.statistics *)
  val to_list : 'a t -> (key -> 'a -> 'c) -> 'c list
  val of_list2: key list -> 'a list -> 'a t
end





end
module Hash : sig 
#1 "hash.mli"


module Make (Key : Hashtbl.HashedType) : Hash_gen.S with type key = Key.t

end = struct
#1 "hash.ml"
# 22 "ext/hash.cppo.ml"
module Make (Key : Hashtbl.HashedType) = struct 
  type key = Key.t 
  type 'a t = (key, 'a)  Hash_gen.t 
  let key_index (h : _ t ) (key : key) =
    (Key.hash  key ) land (Array.length h.data - 1)
  let eq_key = Key.equal   


# 33 "ext/hash.cppo.ml"
type ('a, 'b) bucket = ('a,'b) Hash_gen.bucket
let create = Hash_gen.create
let clear = Hash_gen.clear
let reset = Hash_gen.reset
let iter = Hash_gen.iter
let to_list = Hash_gen.to_list
let fold = Hash_gen.fold
let length = Hash_gen.length
(* let stats = Hash_gen.stats *)



let add (h : _ t) key data =
  let i = key_index h key in
  let h_data = h.data in   
  Array.unsafe_set h_data i (Cons{key; data; next=Array.unsafe_get h_data i});
  h.size <- h.size + 1;
  if h.size > Array.length h_data lsl 1 then Hash_gen.resize key_index h

(* after upgrade to 4.04 we should provide an efficient [replace_or_init] *)
let add_or_update 
  (h : 'a t) 
  (key : key) 
  ~update:(modf : 'a -> 'a) 
  (default :  'a) : unit =
  let rec find_bucket (bucketlist : _ bucket) : bool =
    match bucketlist with
    | Cons rhs  ->
      if eq_key rhs.key key then begin rhs.data <- modf rhs.data; false end
      else find_bucket rhs.next
    | Empty -> true in
  let i = key_index h key in 
  let h_data = h.data in 
  if find_bucket (Array.unsafe_get h_data i) then
    begin 
      Array.unsafe_set h_data i  (Cons{key; data=default; next = Array.unsafe_get h_data i});
      h.size <- h.size + 1 ;
      if h.size > Array.length h_data lsl 1 then Hash_gen.resize key_index h 
    end

let remove (h : _ t ) key =
  let i = key_index h key in
  let h_data = h.data in 
  Hash_gen.remove_bucket h i key ~prec:Empty (Array.unsafe_get h_data i) eq_key

(* for short bucket list, [find_rec is not called ] *)
let rec find_rec key (bucketlist : _ bucket) = match bucketlist with  
  | Empty ->
    raise Not_found
  | Cons rhs  ->
    if eq_key key rhs.key then rhs.data else find_rec key rhs.next

let find_exn (h : _ t) key =
  match Array.unsafe_get h.data (key_index h key) with
  | Empty -> raise Not_found
  | Cons rhs  ->
    if eq_key key rhs.key then rhs.data else
      match rhs.next with
      | Empty -> raise Not_found
      | Cons rhs  ->
        if eq_key key rhs.key then rhs.data else
          match rhs.next with
          | Empty -> raise Not_found
          | Cons rhs ->
            if eq_key key rhs.key  then rhs.data else find_rec key rhs.next

let find_opt (h : _ t) key =
  Hash_gen.small_bucket_opt eq_key key (Array.unsafe_get h.data (key_index h key))

let find_key_opt (h : _ t) key =
  Hash_gen.small_bucket_key_opt eq_key key (Array.unsafe_get h.data (key_index h key))
  
let find_default (h : _ t) key default = 
  Hash_gen.small_bucket_default eq_key key default (Array.unsafe_get h.data (key_index h key))

let find_all (h : _ t) key =
  let rec find_in_bucket (bucketlist : _ bucket) = match bucketlist with 
    | Empty ->
      []
    | Cons rhs  ->
      if eq_key key rhs.key
      then rhs.data :: find_in_bucket rhs.next
      else find_in_bucket rhs.next in
  find_in_bucket (Array.unsafe_get h.data (key_index h key))


let replace h key data =
  let i = key_index h key in
  let h_data = h.data in 
  let l = Array.unsafe_get h_data i in
  if Hash_gen.replace_bucket key data l eq_key then 
    begin 
      Array.unsafe_set h_data i (Cons{key; data; next=l});
      h.size <- h.size + 1;
      if h.size > Array.length h_data lsl 1 then Hash_gen.resize key_index h;
    end 

let mem (h : _ t) key = 
  Hash_gen.small_bucket_mem 
    (Array.unsafe_get h.data (key_index h key))
    eq_key key 


let of_list2 ks vs = 
  let len = List.length ks in 
  let map = create len in 
  List.iter2 (fun k v -> add map k v) ks vs ; 
  map

# 143 "ext/hash.cppo.ml"
end

end
module Bsb_pkg : sig 
#1 "bsb_pkg.mli"

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


(** [resolve cwd module_name], 
    [cwd] is current working directory, absolute path
    Trying to find paths to load [module_name]
    it is sepcialized for option [-bs-package-include] which requires
    [npm_package_name/lib/ocaml]

    it relies on [npm_config_prefix] env variable for global npm modules
*)

(** @raise  when not found *)
val resolve_bs_package : 
    cwd:string ->  Bsb_pkg_types.t -> string 


val to_list:    
  (Bsb_pkg_types.t  ->
   string ->
   'a
  ) -> 'a list
end = struct
#1 "bsb_pkg.ml"

(* Copyright (C) 2017- Authors of BuckleScript
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

let (//) = Filename.concat

type t = Bsb_pkg_types.t

(* TODO: be more restrict 
  [bsconfig.json] does not always make sense, 
  when resolving [ppx-flags]
*)
let make_sub_path (x : t) : string = 
  Literals.node_modules // Bsb_pkg_types.to_string x

let node_paths : string list Lazy.t =     
  lazy (try Ext_string.split (Sys.getenv "NODE_PATH")
              (if Sys.win32 then ';' else ':')   
        with _ -> [])
(** It makes sense to have this function raise, when [bsb] could not resolve a package, it used to mean
    a failure
*)
let check_dir dir =
  match Sys.file_exists dir with
  | true -> Some(dir)
  | false -> None

let  resolve_bs_package_aux  ~cwd (pkg : t) =
  (* First try to resolve recursively from the current working directory  *)
  let sub_path = make_sub_path pkg   in
  let rec aux  cwd  =
    let abs_marker =  cwd // sub_path in
    if Sys.file_exists abs_marker then abs_marker
    else
      let another_cwd = Filename.dirname cwd in (* TODO: may non-terminating when see symlinks *)
      if String.length another_cwd < String.length cwd then
        aux    another_cwd
      else (* To the end try other possiblilities [NODE_PATH]*)
        (match Ext_list.find_opt (Lazy.force node_paths)
                 (fun dir -> check_dir (dir // Bsb_pkg_types.to_string pkg))  with
        | Some(resolved_dir) -> resolved_dir
        | None -> Bsb_exception.package_not_found ~pkg ~json:None)    
  in
   aux cwd 
    
    
    
    
    

module Coll = Hash.Make(struct
  type nonrec t = t 
  let equal = Bsb_pkg_types.equal
  let hash (x : t) = Hashtbl.hash x     
end)


let cache : string Coll.t = Coll.create 0


let to_list cb  =   
  Coll.to_list cache  cb 
  
(* Some package managers will implement "postinstall" caches, that do not
 * keep their build artifacts in the local node_modules. Similar to
 * npm_config_prefix, bs_custom_resolution allows these to specify the
 * exact location of build cache, but on a per-package basis. Implemented as
 * environment lookup to avoid invasive changes to bsconfig and mandates. *)
let custom_resolution = lazy
  (match Sys.getenv "bs_custom_resolution" with
  | exception Not_found  -> false
  | "true"  -> true
  | _ -> false)

let pkg_name_as_variable package =
  Bsb_pkg_types.to_string package
  |> fun s -> Ext_string.split s '@'
  |> String.concat ""
  |> fun s -> Ext_string.split s '_'
  |> String.concat "__"
  |> fun s -> Ext_string.split s '/'
  |> String.concat "__slash__"
  |> fun s -> Ext_string.split s '.'
  |> String.concat "__dot__"
  |> fun s -> Ext_string.split s '-'
  |> String.concat "_"

(** TODO: collect all warnings and print later *)
let resolve_bs_package ~cwd (package : t) =
  if Lazy.force custom_resolution then
  begin
    Bsb_log.info "@{<info>Using Custom Resolution@}@.";
    let custom_pkg_loc = pkg_name_as_variable package ^ "__install" in
    let custom_pkg_location = lazy (Sys.getenv custom_pkg_loc) in
    match Lazy.force custom_pkg_location with
    | exception Not_found ->
        begin
          Bsb_log.error
            "@{<error>Custom resolution of package %s does not exist in var %s @}@."
            (Bsb_pkg_types.to_string package)
            custom_pkg_loc;
          Bsb_exception.package_not_found ~pkg:package ~json:None
        end
    | path when not (Sys.file_exists path) ->
        begin
          Bsb_log.error
            "@{<error>Custom resolution of package %s does not exist on disk: %s=%s @}@."
            (Bsb_pkg_types.to_string package)
            custom_pkg_loc
            path;
          Bsb_exception.package_not_found ~pkg:package ~json:None
        end
    | path ->
      begin
        Bsb_log.info
          "@{<info>Custom Resolution of package %s in var %s found at %s@}@."
          (Bsb_pkg_types.to_string package)
          custom_pkg_loc
          path;
        path
      end
    end
  else
    match Coll.find_opt cache package with
    | None ->
      let result = resolve_bs_package_aux ~cwd package in
      Bsb_log.info "@{<info>Package@} %a -> %s@." Bsb_pkg_types.print package result ;
      Coll.add cache package result ;
      result
    | Some x
      ->
      let result = resolve_bs_package_aux ~cwd package in
      if not (Bsb_real_path.is_same_paths_via_io result x) then
        begin
          Bsb_log.warn
            "@{<warning>Duplicated package:@} %a %s (chosen) vs %s in %s @." 
              Bsb_pkg_types.print package x result cwd;
        end;
      x


(** The package does not need to be a bspackage
    example:
    {[
      resolve_npm_package_file ~cwd "reason/refmt";;
      resolve_npm_package_file ~cwd "reason/refmt/xx/yy"
    ]}
    It also returns the path name
    Note the input [sub_path] is already converted to physical meaning path according to OS
*)
(* let resolve_npm_package_file ~cwd sub_path = *) 
(*   let rec aux  cwd  =  *)
(*     let abs_marker =  cwd // Literals.node_modules // sub_path in  *)
(*     if Sys.file_exists abs_marker then Some abs_marker *)
(*     else  *)
(*       let cwd' = Filename.dirname cwd in  *)
(*       if String.length cwd' < String.length cwd then   *)
(*         aux cwd'  *)
(*       else  *)
(*         try  *)
(*           let abs_marker =  *)
(*             Sys.getenv "npm_config_prefix"  *)
(*             // "lib" // Literals.node_modules // sub_path in *)
(*           if Sys.file_exists abs_marker *)
(*           then Some  abs_marker *)
(*           else None *)
(*             (\* Bs_exception.error (Bs_package_not_found name) *\) *)
(*         with  *)
(*           Not_found -> None *)
(*           (\* Bs_exception.error (Bs_package_not_found name)           *\) *)
(*   in *)
(*   aux cwd *)

end
module Ext_json_parse : sig 
#1 "ext_json_parse.mli"
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

type error

val report_error : Format.formatter -> error -> unit 

exception Error of Lexing.position * Lexing.position * error

val parse_json_from_string : string -> Ext_json_types.t 

val parse_json_from_chan :
  string ->  in_channel -> Ext_json_types.t 

val parse_json_from_file  : string -> Ext_json_types.t


end = struct
#1 "ext_json_parse.ml"
# 1 "ext/ext_json_parse.mll"
 
type error =
  | Illegal_character of char
  | Unterminated_string
  | Unterminated_comment
  | Illegal_escape of string
  | Unexpected_token 
  | Expect_comma_or_rbracket
  | Expect_comma_or_rbrace
  | Expect_colon
  | Expect_string_or_rbrace 
  | Expect_eof 
  (* | Trailing_comma_in_obj *)
  (* | Trailing_comma_in_array *)


let fprintf  = Format.fprintf
let report_error ppf = function
  | Illegal_character c ->
      fprintf ppf "Illegal character (%s)" (Char.escaped c)
  | Illegal_escape s ->
      fprintf ppf "Illegal backslash escape in string or character (%s)" s
  | Unterminated_string -> 
      fprintf ppf "Unterminated_string"
  | Expect_comma_or_rbracket ->
    fprintf ppf "Expect_comma_or_rbracket"
  | Expect_comma_or_rbrace -> 
    fprintf ppf "Expect_comma_or_rbrace"
  | Expect_colon -> 
    fprintf ppf "Expect_colon"
  | Expect_string_or_rbrace  -> 
    fprintf ppf "Expect_string_or_rbrace"
  | Expect_eof  -> 
    fprintf ppf "Expect_eof"
  | Unexpected_token 
    ->
    fprintf ppf "Unexpected_token"
  (* | Trailing_comma_in_obj  *)
  (*   -> fprintf ppf "Trailing_comma_in_obj" *)
  (* | Trailing_comma_in_array  *)
  (*   -> fprintf ppf "Trailing_comma_in_array" *)
  | Unterminated_comment 
    -> fprintf ppf "Unterminated_comment"
         

exception Error of Lexing.position * Lexing.position * error


let () = 
  Printexc.register_printer
    (function x -> 
     match x with 
     | Error (loc_start,loc_end,error) -> 
       Some (Format.asprintf 
          "@[%a:@ %a@ -@ %a)@]" 
          report_error  error
          Ext_position.print loc_start
          Ext_position.print loc_end
       )

     | _ -> None
    )





type token = 
  | Comma
  | Eof
  | False
  | Lbrace
  | Lbracket
  | Null
  | Colon
  | Number of string
  | Rbrace
  | Rbracket
  | String of string
  | True   
  
let error  (lexbuf : Lexing.lexbuf) e = 
  raise (Error (lexbuf.lex_start_p, lexbuf.lex_curr_p, e))


let lexeme_len (x : Lexing.lexbuf) =
  x.lex_curr_pos - x.lex_start_pos

let update_loc ({ lex_curr_p; _ } as lexbuf : Lexing.lexbuf) diff =
  lexbuf.lex_curr_p <-
    {
      lex_curr_p with
      pos_lnum = lex_curr_p.pos_lnum + 1;
      pos_bol = lex_curr_p.pos_cnum - diff;
    }

let char_for_backslash = function
  | 'n' -> '\010'
  | 'r' -> '\013'
  | 'b' -> '\008'
  | 't' -> '\009'
  | c -> c

let dec_code c1 c2 c3 =
  100 * (Char.code c1 - 48) + 10 * (Char.code c2 - 48) + (Char.code c3 - 48)

let hex_code c1 c2 =
  let d1 = Char.code c1 in
  let val1 =
    if d1 >= 97 then d1 - 87
    else if d1 >= 65 then d1 - 55
    else d1 - 48 in
  let d2 = Char.code c2 in
  let val2 =
    if d2 >= 97 then d2 - 87
    else if d2 >= 65 then d2 - 55
    else d2 - 48 in
  val1 * 16 + val2

let lf = '\010'

# 124 "ext/ext_json_parse.ml"
let __ocaml_lex_tables = {
  Lexing.lex_base =
   "\000\000\239\255\240\255\241\255\000\000\025\000\011\000\244\255\
    \245\255\246\255\247\255\248\255\249\255\000\000\000\000\000\000\
    \041\000\001\000\254\255\005\000\005\000\253\255\001\000\002\000\
    \252\255\000\000\000\000\003\000\251\255\001\000\003\000\250\255\
    \079\000\089\000\099\000\121\000\131\000\141\000\153\000\163\000\
    \001\000\253\255\254\255\023\000\255\255\006\000\246\255\189\000\
    \248\255\215\000\255\255\249\255\249\000\181\000\252\255\009\000\
    \063\000\075\000\234\000\251\255\032\001\250\255";
  Lexing.lex_backtrk =
   "\255\255\255\255\255\255\255\255\013\000\013\000\016\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\016\000\016\000\016\000\
    \016\000\016\000\255\255\000\000\012\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\013\000\255\255\013\000\255\255\013\000\255\255\
    \255\255\255\255\255\255\001\000\255\255\255\255\255\255\008\000\
    \255\255\255\255\255\255\255\255\006\000\006\000\255\255\006\000\
    \001\000\002\000\255\255\255\255\255\255\255\255";
  Lexing.lex_default =
   "\001\000\000\000\000\000\000\000\255\255\255\255\255\255\000\000\
    \000\000\000\000\000\000\000\000\000\000\255\255\255\255\255\255\
    \255\255\255\255\000\000\255\255\020\000\000\000\255\255\255\255\
    \000\000\255\255\255\255\255\255\000\000\255\255\255\255\000\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \042\000\000\000\000\000\255\255\000\000\047\000\000\000\047\000\
    \000\000\051\000\000\000\000\000\255\255\255\255\000\000\255\255\
    \255\255\255\255\255\255\000\000\255\255\000\000";
  Lexing.lex_trans =
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\019\000\018\000\018\000\019\000\017\000\019\000\255\255\
    \048\000\019\000\255\255\057\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \019\000\000\000\003\000\000\000\000\000\019\000\000\000\000\000\
    \050\000\000\000\000\000\043\000\008\000\006\000\033\000\016\000\
    \004\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\007\000\004\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\032\000\044\000\033\000\
    \056\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\021\000\057\000\000\000\000\000\000\000\
    \020\000\000\000\000\000\012\000\000\000\011\000\032\000\056\000\
    \000\000\025\000\049\000\000\000\000\000\032\000\014\000\024\000\
    \028\000\000\000\000\000\057\000\026\000\030\000\013\000\031\000\
    \000\000\000\000\022\000\027\000\015\000\029\000\023\000\000\000\
    \000\000\000\000\039\000\010\000\039\000\009\000\032\000\038\000\
    \038\000\038\000\038\000\038\000\038\000\038\000\038\000\038\000\
    \038\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\037\000\000\000\037\000\000\000\
    \035\000\036\000\036\000\036\000\036\000\036\000\036\000\036\000\
    \036\000\036\000\036\000\036\000\036\000\036\000\036\000\036\000\
    \036\000\036\000\036\000\036\000\036\000\036\000\036\000\036\000\
    \036\000\036\000\036\000\036\000\036\000\036\000\036\000\255\255\
    \035\000\038\000\038\000\038\000\038\000\038\000\038\000\038\000\
    \038\000\038\000\038\000\038\000\038\000\038\000\038\000\038\000\
    \038\000\038\000\038\000\038\000\038\000\000\000\000\000\255\255\
    \000\000\056\000\000\000\000\000\055\000\058\000\058\000\058\000\
    \058\000\058\000\058\000\058\000\058\000\058\000\058\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\054\000\
    \000\000\054\000\000\000\000\000\000\000\000\000\054\000\000\000\
    \002\000\041\000\000\000\000\000\000\000\255\255\046\000\053\000\
    \053\000\053\000\053\000\053\000\053\000\053\000\053\000\053\000\
    \053\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\255\255\059\000\059\000\059\000\059\000\059\000\059\000\
    \059\000\059\000\059\000\059\000\000\000\000\000\000\000\000\000\
    \000\000\060\000\060\000\060\000\060\000\060\000\060\000\060\000\
    \060\000\060\000\060\000\054\000\000\000\000\000\000\000\000\000\
    \000\000\054\000\060\000\060\000\060\000\060\000\060\000\060\000\
    \000\000\000\000\000\000\000\000\000\000\054\000\000\000\000\000\
    \000\000\054\000\000\000\054\000\000\000\000\000\000\000\052\000\
    \061\000\061\000\061\000\061\000\061\000\061\000\061\000\061\000\
    \061\000\061\000\060\000\060\000\060\000\060\000\060\000\060\000\
    \000\000\061\000\061\000\061\000\061\000\061\000\061\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\061\000\061\000\061\000\061\000\061\000\061\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\255\255\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\255\255\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000";
  Lexing.lex_check =
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\000\000\017\000\000\000\000\000\019\000\020\000\
    \045\000\019\000\020\000\055\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\255\255\000\000\255\255\255\255\019\000\255\255\255\255\
    \045\000\255\255\255\255\040\000\000\000\000\000\004\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\006\000\006\000\006\000\006\000\006\000\
    \006\000\006\000\006\000\006\000\006\000\004\000\043\000\005\000\
    \056\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\016\000\057\000\255\255\255\255\255\255\
    \016\000\255\255\255\255\000\000\255\255\000\000\005\000\056\000\
    \255\255\014\000\045\000\255\255\255\255\004\000\000\000\023\000\
    \027\000\255\255\255\255\057\000\025\000\029\000\000\000\030\000\
    \255\255\255\255\015\000\026\000\000\000\013\000\022\000\255\255\
    \255\255\255\255\032\000\000\000\032\000\000\000\005\000\032\000\
    \032\000\032\000\032\000\032\000\032\000\032\000\032\000\032\000\
    \032\000\033\000\033\000\033\000\033\000\033\000\033\000\033\000\
    \033\000\033\000\033\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\035\000\255\255\035\000\255\255\
    \034\000\035\000\035\000\035\000\035\000\035\000\035\000\035\000\
    \035\000\035\000\035\000\036\000\036\000\036\000\036\000\036\000\
    \036\000\036\000\036\000\036\000\036\000\037\000\037\000\037\000\
    \037\000\037\000\037\000\037\000\037\000\037\000\037\000\047\000\
    \034\000\038\000\038\000\038\000\038\000\038\000\038\000\038\000\
    \038\000\038\000\038\000\039\000\039\000\039\000\039\000\039\000\
    \039\000\039\000\039\000\039\000\039\000\255\255\255\255\047\000\
    \255\255\049\000\255\255\255\255\049\000\053\000\053\000\053\000\
    \053\000\053\000\053\000\053\000\053\000\053\000\053\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\049\000\
    \255\255\049\000\255\255\255\255\255\255\255\255\049\000\255\255\
    \000\000\040\000\255\255\255\255\255\255\020\000\045\000\049\000\
    \049\000\049\000\049\000\049\000\049\000\049\000\049\000\049\000\
    \049\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\047\000\058\000\058\000\058\000\058\000\058\000\058\000\
    \058\000\058\000\058\000\058\000\255\255\255\255\255\255\255\255\
    \255\255\052\000\052\000\052\000\052\000\052\000\052\000\052\000\
    \052\000\052\000\052\000\049\000\255\255\255\255\255\255\255\255\
    \255\255\049\000\052\000\052\000\052\000\052\000\052\000\052\000\
    \255\255\255\255\255\255\255\255\255\255\049\000\255\255\255\255\
    \255\255\049\000\255\255\049\000\255\255\255\255\255\255\049\000\
    \060\000\060\000\060\000\060\000\060\000\060\000\060\000\060\000\
    \060\000\060\000\052\000\052\000\052\000\052\000\052\000\052\000\
    \255\255\060\000\060\000\060\000\060\000\060\000\060\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\060\000\060\000\060\000\060\000\060\000\060\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\047\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\049\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255";
  Lexing.lex_base_code =
   "";
  Lexing.lex_backtrk_code =
   "";
  Lexing.lex_default_code =
   "";
  Lexing.lex_trans_code =
   "";
  Lexing.lex_check_code =
   "";
  Lexing.lex_code =
   "";
}

let rec lex_json buf lexbuf =
   __ocaml_lex_lex_json_rec buf lexbuf 0
and __ocaml_lex_lex_json_rec buf lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 142 "ext/ext_json_parse.mll"
          ( lex_json buf lexbuf)
# 314 "ext/ext_json_parse.ml"

  | 1 ->
# 143 "ext/ext_json_parse.mll"
                   ( 
    update_loc lexbuf 0;
    lex_json buf  lexbuf
  )
# 322 "ext/ext_json_parse.ml"

  | 2 ->
# 147 "ext/ext_json_parse.mll"
                ( comment buf lexbuf)
# 327 "ext/ext_json_parse.ml"

  | 3 ->
# 148 "ext/ext_json_parse.mll"
         ( True)
# 332 "ext/ext_json_parse.ml"

  | 4 ->
# 149 "ext/ext_json_parse.mll"
          (False)
# 337 "ext/ext_json_parse.ml"

  | 5 ->
# 150 "ext/ext_json_parse.mll"
         (Null)
# 342 "ext/ext_json_parse.ml"

  | 6 ->
# 151 "ext/ext_json_parse.mll"
       (Lbracket)
# 347 "ext/ext_json_parse.ml"

  | 7 ->
# 152 "ext/ext_json_parse.mll"
       (Rbracket)
# 352 "ext/ext_json_parse.ml"

  | 8 ->
# 153 "ext/ext_json_parse.mll"
       (Lbrace)
# 357 "ext/ext_json_parse.ml"

  | 9 ->
# 154 "ext/ext_json_parse.mll"
       (Rbrace)
# 362 "ext/ext_json_parse.ml"

  | 10 ->
# 155 "ext/ext_json_parse.mll"
       (Comma)
# 367 "ext/ext_json_parse.ml"

  | 11 ->
# 156 "ext/ext_json_parse.mll"
        (Colon)
# 372 "ext/ext_json_parse.ml"

  | 12 ->
# 157 "ext/ext_json_parse.mll"
                      (lex_json buf lexbuf)
# 377 "ext/ext_json_parse.ml"

  | 13 ->
# 159 "ext/ext_json_parse.mll"
         ( Number (Lexing.lexeme lexbuf))
# 382 "ext/ext_json_parse.ml"

  | 14 ->
# 161 "ext/ext_json_parse.mll"
      (
  let pos = Lexing.lexeme_start_p lexbuf in
  scan_string buf pos lexbuf;
  let content = (Buffer.contents  buf) in 
  Buffer.clear buf ;
  String content 
)
# 393 "ext/ext_json_parse.ml"

  | 15 ->
# 168 "ext/ext_json_parse.mll"
       (Eof )
# 398 "ext/ext_json_parse.ml"

  | 16 ->
let
# 169 "ext/ext_json_parse.mll"
       c
# 404 "ext/ext_json_parse.ml"
= Lexing.sub_lexeme_char lexbuf lexbuf.Lexing.lex_start_pos in
# 169 "ext/ext_json_parse.mll"
          ( error lexbuf (Illegal_character c ))
# 408 "ext/ext_json_parse.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_lex_json_rec buf lexbuf __ocaml_lex_state

and comment buf lexbuf =
   __ocaml_lex_comment_rec buf lexbuf 40
and __ocaml_lex_comment_rec buf lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 171 "ext/ext_json_parse.mll"
              (lex_json buf lexbuf)
# 420 "ext/ext_json_parse.ml"

  | 1 ->
# 172 "ext/ext_json_parse.mll"
     (comment buf lexbuf)
# 425 "ext/ext_json_parse.ml"

  | 2 ->
# 173 "ext/ext_json_parse.mll"
       (error lexbuf Unterminated_comment)
# 430 "ext/ext_json_parse.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_comment_rec buf lexbuf __ocaml_lex_state

and scan_string buf start lexbuf =
   __ocaml_lex_scan_string_rec buf start lexbuf 45
and __ocaml_lex_scan_string_rec buf start lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 177 "ext/ext_json_parse.mll"
      ( () )
# 442 "ext/ext_json_parse.ml"

  | 1 ->
# 179 "ext/ext_json_parse.mll"
  (
        let len = lexeme_len lexbuf - 2 in
        update_loc lexbuf len;

        scan_string buf start lexbuf
      )
# 452 "ext/ext_json_parse.ml"

  | 2 ->
# 186 "ext/ext_json_parse.mll"
      (
        let len = lexeme_len lexbuf - 3 in
        update_loc lexbuf len;
        scan_string buf start lexbuf
      )
# 461 "ext/ext_json_parse.ml"

  | 3 ->
let
# 191 "ext/ext_json_parse.mll"
                                               c
# 467 "ext/ext_json_parse.ml"
= Lexing.sub_lexeme_char lexbuf (lexbuf.Lexing.lex_start_pos + 1) in
# 192 "ext/ext_json_parse.mll"
      (
        Buffer.add_char buf (char_for_backslash c);
        scan_string buf start lexbuf
      )
# 474 "ext/ext_json_parse.ml"

  | 4 ->
let
# 196 "ext/ext_json_parse.mll"
                 c1
# 480 "ext/ext_json_parse.ml"
= Lexing.sub_lexeme_char lexbuf (lexbuf.Lexing.lex_start_pos + 1)
and
# 196 "ext/ext_json_parse.mll"
                               c2
# 485 "ext/ext_json_parse.ml"
= Lexing.sub_lexeme_char lexbuf (lexbuf.Lexing.lex_start_pos + 2)
and
# 196 "ext/ext_json_parse.mll"
                                             c3
# 490 "ext/ext_json_parse.ml"
= Lexing.sub_lexeme_char lexbuf (lexbuf.Lexing.lex_start_pos + 3)
and
# 196 "ext/ext_json_parse.mll"
                                                    s
# 495 "ext/ext_json_parse.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos (lexbuf.Lexing.lex_start_pos + 4) in
# 197 "ext/ext_json_parse.mll"
      (
        let v = dec_code c1 c2 c3 in
        if v > 255 then
          error lexbuf (Illegal_escape s) ;
        Buffer.add_char buf (Char.chr v);

        scan_string buf start lexbuf
      )
# 506 "ext/ext_json_parse.ml"

  | 5 ->
let
# 205 "ext/ext_json_parse.mll"
                        c1
# 512 "ext/ext_json_parse.ml"
= Lexing.sub_lexeme_char lexbuf (lexbuf.Lexing.lex_start_pos + 2)
and
# 205 "ext/ext_json_parse.mll"
                                         c2
# 517 "ext/ext_json_parse.ml"
= Lexing.sub_lexeme_char lexbuf (lexbuf.Lexing.lex_start_pos + 3) in
# 206 "ext/ext_json_parse.mll"
      (
        let v = hex_code c1 c2 in
        Buffer.add_char buf (Char.chr v);

        scan_string buf start lexbuf
      )
# 526 "ext/ext_json_parse.ml"

  | 6 ->
let
# 212 "ext/ext_json_parse.mll"
             c
# 532 "ext/ext_json_parse.ml"
= Lexing.sub_lexeme_char lexbuf (lexbuf.Lexing.lex_start_pos + 1) in
# 213 "ext/ext_json_parse.mll"
      (
        Buffer.add_char buf '\\';
        Buffer.add_char buf c;

        scan_string buf start lexbuf
      )
# 541 "ext/ext_json_parse.ml"

  | 7 ->
# 220 "ext/ext_json_parse.mll"
      (
        update_loc lexbuf 0;
        Buffer.add_char buf lf;

        scan_string buf start lexbuf
      )
# 551 "ext/ext_json_parse.ml"

  | 8 ->
# 227 "ext/ext_json_parse.mll"
      (
        let ofs = lexbuf.lex_start_pos in
        let len = lexbuf.lex_curr_pos - ofs in
        Buffer.add_subbytes buf lexbuf.lex_buffer ofs len;

        scan_string buf start lexbuf
      )
# 562 "ext/ext_json_parse.ml"

  | 9 ->
# 235 "ext/ext_json_parse.mll"
      (
        error lexbuf Unterminated_string
      )
# 569 "ext/ext_json_parse.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_scan_string_rec buf start lexbuf __ocaml_lex_state

;;

# 239 "ext/ext_json_parse.mll"
 






let  parse_json lexbuf =
  let buf = Buffer.create 64 in 
  let look_ahead = ref None in
  let token () : token = 
    match !look_ahead with 
    | None ->  
      lex_json buf lexbuf 
    | Some x -> 
      look_ahead := None ;
      x 
  in
  let push e = look_ahead := Some e in 
  let rec json (lexbuf : Lexing.lexbuf) : Ext_json_types.t = 
    match token () with 
    | True -> True lexbuf.lex_start_p
    | False -> False lexbuf.lex_start_p
    | Null -> Null lexbuf.lex_start_p
    | Number s ->  Flo {flo = s; loc = lexbuf.lex_start_p}  
    | String s -> Str { str = s; loc =    lexbuf.lex_start_p}
    | Lbracket -> parse_array  lexbuf.lex_start_p lexbuf.lex_curr_p [] lexbuf
    | Lbrace -> parse_map lexbuf.lex_start_p Map_string.empty lexbuf
    |  _ -> error lexbuf Unexpected_token
(** Note if we remove [trailing_comma] support 
    we should report errors (actually more work), for example 
    {[
    match token () with 
    | Rbracket ->
      if trailing_comma then
        error lexbuf Trailing_comma_in_array
      else
    ]} 
    {[
    match token () with 
    | Rbrace -> 
      if trailing_comma then
        error lexbuf Trailing_comma_in_obj
      else

    ]}   
 *)
  and parse_array   loc_start loc_finish acc lexbuf 
    : Ext_json_types.t =
    match token () with 
    | Rbracket ->
        Arr {loc_start ; content = Ext_array.reverse_of_list acc ; 
              loc_end = lexbuf.lex_curr_p }
    | x -> 
      push x ;
      let new_one = json lexbuf in 
      begin match token ()  with 
      | Comma -> 
          parse_array  loc_start loc_finish (new_one :: acc) lexbuf 
      | Rbracket 
        -> Arr {content = (Ext_array.reverse_of_list (new_one::acc));
                     loc_start ; 
                     loc_end = lexbuf.lex_curr_p }
      | _ -> 
        error lexbuf Expect_comma_or_rbracket
      end
  and parse_map loc_start  acc lexbuf : Ext_json_types.t = 
    match token () with 
    | Rbrace -> 
        Obj { map = acc ; loc = loc_start}
    | String key -> 
      begin match token () with 
      | Colon ->
        let value = json lexbuf in
        begin match token () with 
        | Rbrace -> Obj {map = Map_string.add acc key value  ; loc = loc_start}
        | Comma -> 
          parse_map loc_start  (Map_string.add acc key value ) lexbuf 
        | _ -> error lexbuf Expect_comma_or_rbrace
        end
      | _ -> error lexbuf Expect_colon
      end
    | _ -> error lexbuf Expect_string_or_rbrace
  in 
  let v = json lexbuf in 
  match token () with 
  | Eof -> v 
  | _ -> error lexbuf Expect_eof

let parse_json_from_string s = 
  parse_json (Lexing.from_string s )

let parse_json_from_chan fname in_chan = 
  let lexbuf = 
    Ext_position.lexbuf_from_channel_with_fname
    in_chan fname in 
  parse_json lexbuf 

let parse_json_from_file s = 
  let in_chan = open_in s in 
  let lexbuf = 
    Ext_position.lexbuf_from_channel_with_fname
    in_chan s in 
  match parse_json lexbuf with 
  | exception e -> close_in in_chan ; raise e
  | v  -> close_in in_chan;  v





# 688 "ext/ext_json_parse.ml"

end
module Hash_string : sig 
#1 "hash_string.mli"
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


include Hash_gen.S with type key = string




end = struct
#1 "hash_string.ml"
# 9 "ext/hash.cppo.ml"
type key = string
type 'a t = (key, 'a)  Hash_gen.t 
let key_index (h : _ t ) (key : key) =
  (Bs_hash_stubs.hash_string  key ) land (Array.length h.data - 1)
let eq_key = Ext_string.equal 

# 33 "ext/hash.cppo.ml"
type ('a, 'b) bucket = ('a,'b) Hash_gen.bucket
let create = Hash_gen.create
let clear = Hash_gen.clear
let reset = Hash_gen.reset
let iter = Hash_gen.iter
let to_list = Hash_gen.to_list
let fold = Hash_gen.fold
let length = Hash_gen.length
(* let stats = Hash_gen.stats *)



let add (h : _ t) key data =
  let i = key_index h key in
  let h_data = h.data in   
  Array.unsafe_set h_data i (Cons{key; data; next=Array.unsafe_get h_data i});
  h.size <- h.size + 1;
  if h.size > Array.length h_data lsl 1 then Hash_gen.resize key_index h

(* after upgrade to 4.04 we should provide an efficient [replace_or_init] *)
let add_or_update 
  (h : 'a t) 
  (key : key) 
  ~update:(modf : 'a -> 'a) 
  (default :  'a) : unit =
  let rec find_bucket (bucketlist : _ bucket) : bool =
    match bucketlist with
    | Cons rhs  ->
      if eq_key rhs.key key then begin rhs.data <- modf rhs.data; false end
      else find_bucket rhs.next
    | Empty -> true in
  let i = key_index h key in 
  let h_data = h.data in 
  if find_bucket (Array.unsafe_get h_data i) then
    begin 
      Array.unsafe_set h_data i  (Cons{key; data=default; next = Array.unsafe_get h_data i});
      h.size <- h.size + 1 ;
      if h.size > Array.length h_data lsl 1 then Hash_gen.resize key_index h 
    end

let remove (h : _ t ) key =
  let i = key_index h key in
  let h_data = h.data in 
  Hash_gen.remove_bucket h i key ~prec:Empty (Array.unsafe_get h_data i) eq_key

(* for short bucket list, [find_rec is not called ] *)
let rec find_rec key (bucketlist : _ bucket) = match bucketlist with  
  | Empty ->
    raise Not_found
  | Cons rhs  ->
    if eq_key key rhs.key then rhs.data else find_rec key rhs.next

let find_exn (h : _ t) key =
  match Array.unsafe_get h.data (key_index h key) with
  | Empty -> raise Not_found
  | Cons rhs  ->
    if eq_key key rhs.key then rhs.data else
      match rhs.next with
      | Empty -> raise Not_found
      | Cons rhs  ->
        if eq_key key rhs.key then rhs.data else
          match rhs.next with
          | Empty -> raise Not_found
          | Cons rhs ->
            if eq_key key rhs.key  then rhs.data else find_rec key rhs.next

let find_opt (h : _ t) key =
  Hash_gen.small_bucket_opt eq_key key (Array.unsafe_get h.data (key_index h key))

let find_key_opt (h : _ t) key =
  Hash_gen.small_bucket_key_opt eq_key key (Array.unsafe_get h.data (key_index h key))
  
let find_default (h : _ t) key default = 
  Hash_gen.small_bucket_default eq_key key default (Array.unsafe_get h.data (key_index h key))

let find_all (h : _ t) key =
  let rec find_in_bucket (bucketlist : _ bucket) = match bucketlist with 
    | Empty ->
      []
    | Cons rhs  ->
      if eq_key key rhs.key
      then rhs.data :: find_in_bucket rhs.next
      else find_in_bucket rhs.next in
  find_in_bucket (Array.unsafe_get h.data (key_index h key))


let replace h key data =
  let i = key_index h key in
  let h_data = h.data in 
  let l = Array.unsafe_get h_data i in
  if Hash_gen.replace_bucket key data l eq_key then 
    begin 
      Array.unsafe_set h_data i (Cons{key; data; next=l});
      h.size <- h.size + 1;
      if h.size > Array.length h_data lsl 1 then Hash_gen.resize key_index h;
    end 

let mem (h : _ t) key = 
  Hash_gen.small_bucket_mem 
    (Array.unsafe_get h.data (key_index h key))
    eq_key key 


let of_list2 ks vs = 
  let len = List.length ks in 
  let map = create len in 
  List.iter2 (fun k v -> add map k v) ks vs ; 
  map


end
module Bsb_build_util : sig 
#1 "bsb_build_util.mli"
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

(**
  Use:
  {[
  flag_concat "-ppx" [ppxs]
  ]}
  *)
  val flag_concat : string -> string list -> string

(**
Build quoted commandline arguments for bsc.exe for the given ppx flags

Use:
{[
ppx_flags [ppxs]
]}
*)
val ppx_flags : Bsb_config_types.ppx list -> string

val pp_flag : string  -> string

(**
Build unquoted command line arguments for bsc.exe for the given include dirs

Use:
{[
include_dirs [dirs]
]}
*)
val include_dirs : string list -> string

val include_dirs_by : 
  'a list ->   
  ('a -> string ) ->
  string
  

val mkp : string -> unit


(* The path of [bsc] and [bsdep] is normalized so that the invokation of [./jscomp/bin/bsb.exe] 
   and [bsb.exe] (combined with a dirty bsconfig.json) will not trigger unnecessary rebuild.
   
   The location of [bsc] and [bsdep] is configured by the combination of [Sys.executable_name] 
   and [cwd].
   
   In theory, we should also check the integrity of [bsb.exe], if it is changed, the rebuild 
   should be regen, but that is too much in practice, not only you need check the integrity of 
   path of [bsb.exe] but also the timestamp, to make it 100% correct, also the integrity of 
   [bsdep.exe] [bsc.exe] etc.
*)





val get_list_string_acc : 
    Ext_json_types.t array -> 
    string list -> 
    string list

val get_list_string : 
    Ext_json_types.t array -> 
    string list

type result = { path : string; checked : bool }    

(* [resolve_bsb_magic_file]
   returns a tuple (path,checked)
   when checked is true, it means such file should exist without depending on env
*)
val resolve_bsb_magic_file : 
  cwd:string -> 
  desc:string ->
  string -> 
  result

type package_context = {
  proj_dir : string ; 
  top : bool ; 
}

val walk_all_deps : string -> (package_context -> unit) -> unit

end = struct
#1 "bsb_build_util.ml"
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

let flag_concat flag xs =   
  String.concat Ext_string.single_space
    (Ext_list.flat_map xs  (fun x -> [flag ; x]))
  
let (//) = Ext_path.combine



let ppx_flags (xs : Bsb_config_types.ppx list) =
  flag_concat "-ppx"
    (Ext_list.map xs 
       (fun x -> 
          if x.args = [] then Ext_filename.maybe_quote x.name else 
            let fmt : _ format = 
              if Ext_sys.is_windows_or_cygwin then "\"%s %s\""
              else "'%s %s'" in 
            Printf.sprintf fmt x.name (String.concat " " x.args) 
       ))

let pp_flag (xs : string) = 
   "-pp " ^ Ext_filename.maybe_quote xs

let include_dirs dirs = 
  String.concat Ext_string.single_space
    (Ext_list.flat_map dirs (fun x -> ["-I"; Ext_filename.maybe_quote x]))


let include_dirs_by dirs fn = 
  String.concat Ext_string.single_space
    (Ext_list.flat_map dirs (fun x -> ["-I"; Ext_filename.maybe_quote (fn x)]))


(* we use lazy $src_root_dir *)



(* It does several conversion:
   First, it will convert unix path to windows backward on windows platform.
   Then if it is absolute path, it will do thing
   Else if it is relative path, it will be rebased on project's root directory  *)

let convert_and_resolve_path : string -> string -> string =
  if Sys.unix then (//)
  else fun cwd path ->
    if Ext_sys.is_windows_or_cygwin then 
      let p = Ext_string.replace_slash_backward path in
      cwd // p
    else failwith ("Unknown OS :" ^ Sys.os_type)
(* we only need convert the path in the beginning *)

type result = { path : string; checked : bool }

(* Magic path resolution:
   foo => foo
   foo/ => /absolute/path/to/projectRoot/node_modules/foo
   foo/bar => /absolute/path/to/projectRoot/node_modules/foo/bar
   /foo/bar => /foo/bar
   ./foo/bar => /absolute/path/to/projectRoot/./foo/bar
   Input is node path, output is OS dependent (normalized) path
*)
let resolve_bsb_magic_file ~cwd ~desc p : result  =

  let no_slash = Ext_string.no_slash_idx p in
  if no_slash < 0 then
    (* Single file FIXME: better error message for "" input *)
    { path = p; checked =  false  }
  else 
    let first_char = String.unsafe_get p 0 in 
    if Filename.is_relative p &&  
       first_char  <> '.' then
      let package_name, rest = 
        Bsb_pkg_types.extract_pkg_name_and_file p 
      in 
      let relative_path = 
        if Ext_sys.is_windows_or_cygwin then Ext_string.replace_slash_backward rest 
        else rest in       
      (* let p = if Ext_sys.is_windows_or_cygwin then Ext_string.replace_slash_backward p else p in *)
      let package_dir = Bsb_pkg.resolve_bs_package ~cwd package_name in
      let path = package_dir // relative_path in 
      if Sys.file_exists path then {path; checked = true}
      else 
        begin 
          Bsb_log.error "@{<error>Could not resolve @} %s in %s@." p cwd ; 
          failwith (p ^ " not found when resolving " ^ desc)
        end

    else
      (* relative path [./x/y]*)
      { path = convert_and_resolve_path cwd p; checked = true}



(** converting a file from Linux path format to Windows *)



(** 
   {[
     mkp "a/b/c/d";;
     mkp "/a/b/c/d"
   ]}
*)
let rec mkp dir = 
  if not (Sys.file_exists dir) then 
    let parent_dir  = Filename.dirname dir in
    if  parent_dir = Filename.current_dir_name then 
      Unix.mkdir dir 0o777 (* leaf node *)
    else 
      begin 
        mkp parent_dir ; 
        Unix.mkdir dir 0o777 
      end
  else if not  @@ Sys.is_directory dir then 
    failwith ( dir ^ " exists but it is not a directory, plz remove it first")
  else ()


let get_list_string_acc (s : Ext_json_types.t array) acc = 
  Ext_array.to_list_map_acc s acc (fun x ->
      match x with 
      | Str x -> Some x.str
      | _ -> None
    ) 

let get_list_string s = get_list_string_acc s []   


(* Key is the path *)
let (|?)  m (key, cb) =
  m  |> Ext_json.test key cb

type package_context = {
  proj_dir : string ; 
  top : bool ; 
}

(**
   TODO: check duplicate package name
   ?use path as identity?

   Basic requirements
     1. cycle detection
     2. avoid duplication
     3. deterministic, since -make-world will also comes with -clean-world

*)

let pp_packages_rev ppf lst = 
  Ext_list.rev_iter lst (fun  s ->  Format.fprintf ppf "%s " s) 

let rec walk_all_deps_aux 
  (visited : string Hash_string.t) 
  (paths : string list) 
  (top : bool) 
  (dir : string) 
  (cb : package_context -> unit) =
  let bsconfig_json =  dir // Literals.bsconfig_json in
  match Ext_json_parse.parse_json_from_file bsconfig_json with
  | Obj {map; loc} ->
    let cur_package_name = 
      match Map_string.find_opt map Bsb_build_schemas.name with 
      | Some (Str {str }) -> str
      | Some _ 
      | None -> Bsb_exception.errorf ~loc "package name missing in %s/bsconfig.json" dir 
    in 
    let package_stacks = cur_package_name :: paths in 
    Bsb_log.info "@{<info>Package stack:@} %a @." pp_packages_rev
      package_stacks ;    
    if Ext_list.mem_string paths cur_package_name  then
      begin
        Bsb_log.error "@{<error>Cyclic dependencies in package stack@}@.";
        exit 2 
      end;
    if Hash_string.mem visited cur_package_name then 
      Bsb_log.info
        "@{<info>Visited before@} %s@." cur_package_name
    else 
      let explore_deps (deps : string) =   
        map
        |?
        (deps,
         `Arr (fun (new_packages : Ext_json_types.t array) ->             
             Ext_array.iter new_packages (fun js ->
                 match js with
                 | Str {str = new_package} ->
                   if not (Bsb_default.filter_otherlibs new_package) then begin
                    let package_dir = 
                      Bsb_pkg.resolve_bs_package ~cwd:dir 
                        (Bsb_pkg_types.string_as_package   new_package) in 
                    walk_all_deps_aux visited package_stacks  false package_dir cb 
                  end
                 | _ -> 
                   Bsb_exception.errorf ~loc 
                     "%s expect an array"
                     deps
               )))
        |> ignore in
      begin 
        explore_deps Bsb_build_schemas.bs_dependencies;          
        if top then explore_deps Bsb_build_schemas.bs_dev_dependencies;
        cb {top ; proj_dir = dir};
        Hash_string.add visited cur_package_name dir;
      end
  | _ -> ()
  | exception _ -> 
    Bsb_exception.invalid_json bsconfig_json
    

let walk_all_deps dir cb = 
  let visited = Hash_string.create 0 in 
  walk_all_deps_aux visited [] true dir cb 

end
module Bsb_global_backend : sig 
#1 "bsb_global_backend.mli"
(* Copyright (C) 2019 - Authors of BuckleScript
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

val backend : Bsb_config_types.compilation_kind_t ref

val lib_artifacts_dir : string ref

val lib_ocaml_dir : string ref

val backend_string: string ref

val set_backend : Bsb_config_types.compilation_kind_t -> unit

end = struct
#1 "bsb_global_backend.ml"
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


end
module Bsb_db_util : sig 
#1 "bsb_db_util.mli"

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



val conflict_module_info:
  string ->
  Bsb_db.module_info -> 
  Bsb_db.module_info -> 
  'a 


val merge : Bsb_db.t -> Bsb_db.t -> Bsb_db.t   

val sanity_check : Bsb_db.t -> unit

(** 
  Currently it is okay to have duplicated module, 
  In the future, we may emit a warning 
*)

val add_basename:
  dir:string -> 
  Bsb_db.t ->  
  ?error_on_invalid_suffix:Ext_position.t-> 
  string -> 
  Bsb_db.t
end = struct
#1 "bsb_db_util.ml"

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
type module_info = Bsb_db.module_info
type t = Bsb_db.t
(* type case = Bsb_db.case *)


     
let conflict_module_info modname (a : module_info) (b : module_info) = 
  Bsb_exception.conflict_module
    modname
    a.dir
    b.dir

(* merge data info from two directories*)    
let merge (acc : t) (sources : t) : t =
  Map_string.merge acc sources (fun modname k1 k2 ->
      match k1 , k2 with
      | None , None ->
        assert false
      | Some a, Some b  ->
        conflict_module_info modname 
          a
          b
      | Some v, None  -> Some v
      | None, Some v ->  Some v
    )

let sanity_check (map : t) = 
  Map_string.iter map (fun m module_info -> 
      if module_info.info = Mli then
        Bsb_exception.no_implementation m 
    )    

(* invariant check:
  ml and mli should have the same case, same path
*)  
let check (x : module_info) 
  name_sans_extension 
  case 
  is_re 
  (module_info : Bsb_db.info)
  =  
  let x_ml_info = x.info in  
  (if x.name_sans_extension <> name_sans_extension 
   || x.case <> case 
   || x.is_re <> is_re 
   || x_ml_info = module_info 
   || x_ml_info = Ml_mli
   then 
     Bsb_exception.invalid_spec 
       (Printf.sprintf 
          "implementation and interface have different path names or different cases %s vs %s"
          x.name_sans_extension name_sans_extension));
  x.info <- Ml_mli;      
  x


let warning_unused_file : _ format = 
  "@{<warning>IGNORED@}: file %s under %s is ignored because it can't be turned into a valid module name. The build system transforms a file name into a module name by upper-casing the first letter@."

let add_basename
    ~(dir:string) 
    (map : t)  
    ?(error_on_invalid_suffix)
    basename : t =   
  let info = ref Bsb_db.Ml in   
  let is_re = ref false in 
  let invalid_suffix = ref false in
  (match Ext_filename.get_extension_maybe basename with 
   | ".ml" -> 
     () 
   | ".re" ->
     is_re := true
   | ".mli" -> 
     info := Mli
   | ".rei" -> 
     info := Mli;
     is_re := true 
   | _ -> 
     invalid_suffix := true

  );   
  let info= !info in 
  let is_re = !is_re in 
  let invalid_suffix = !invalid_suffix in 
  if invalid_suffix then 
    match error_on_invalid_suffix with
    | None -> map 
    | Some loc -> 
      Bsb_exception.errorf ~loc:loc
        "invalid suffix %s" basename
  else  
    match Ext_filename.as_module ~basename:(Filename.basename basename) with 
    | None -> 
      Bsb_log.warn warning_unused_file basename dir; 
      map 
    | Some {module_name; case} ->     
      let name_sans_extension = 
        Filename.concat dir (Ext_filename.chop_extension_maybe basename) in 
      let dir = Filename.dirname name_sans_extension in                
      Map_string.adjust 
        map
        module_name 
        (fun  opt_module_info -> 
           match opt_module_info with 
           | None -> 
             {dir ; name_sans_extension ; info ; is_re ; case }
           | Some x -> 
             check x name_sans_extension case is_re info      
        )

end
module Ext_option : sig 
#1 "ext_option.mli"
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








(** Utilities for [option] type *)

val map : 'a option -> ('a -> 'b) -> 'b option

val iter : 'a option -> ('a -> unit) -> unit

val exists : 'a option -> ('a -> bool) -> bool
end = struct
#1 "ext_option.ml"
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








let map v f = 
  match v with 
  | None -> None
  | Some x -> Some (f x )

let iter v f =   
  match v with 
  | None -> ()
  | Some x -> f x 

let exists v f =    
  match v with 
  | None -> false
  | Some x -> f x 
end
module Bsb_parse_sources : sig 
#1 "bsb_parse_sources.mli"
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



(** [scan .. cxt json]
    entry is to the [sources] in the schema    
    given a root, return an object which is
    all relative paths, this function will do the IO
*)
val scan :
  toplevel: bool -> 
  root: string ->  
  cut_generators: bool -> 
  namespace : string option -> 
  bs_suffix:bool -> 
  ignored_dirs:Set_string.t ->
  Ext_json_types.t ->   
  Bsb_file_groups.t * int 

(** This function has some duplication 
  from [scan],
  the parsing assuming the format is 
  already valid
*) 
val clean_re_js:  
  string -> unit 
end = struct
#1 "bsb_parse_sources.ml"
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



type build_generator = Bsb_file_groups.build_generator



(* type file_group = Bsb_file_groups.file_group *)

type t = Bsb_file_groups.t 

let is_input_or_output (xs : build_generator list) (x : string)  = 
  Ext_list.exists xs (fun  {input; output} -> 
      let it_is = fun y -> y = x  in
      Ext_list.exists input it_is ||
      Ext_list.exists output it_is
    ) 


let errorf x fmt = 
  Bsb_exception.errorf ~loc:(Ext_json.loc_of x) fmt 

type cxt = {
  toplevel : bool ;
  dir_index : Bsb_dir_index.t ; 
  cwd : string ;
  root : string;
  cut_generators : bool;
  traverse : bool;
  namespace : string option;
  bs_suffix: bool;
  ignored_dirs : Set_string.t
}

(** [public] has a list of modules, we do a sanity check to see if all the listed 
  modules are indeed valid module components
*)
let collect_pub_modules 
    (xs : Ext_json_types.t array)
    (cache : Bsb_db.t) : Set_string.t = 
  let set = ref Set_string.empty in 
  for i = 0 to Array.length xs - 1 do 
    let v = Array.unsafe_get xs i in 
    match v with 
    | Str { str}
      -> 
      if Map_string.mem cache str then 
        set := Set_string.add !set str
      else 
        Bsb_log.warn
          "@{<warning>IGNORED@} %S in public is ignored since it is not\
           an existing module@." str
    | _ -> 
      Bsb_exception.errorf 
        ~loc:(Ext_json.loc_of v)
        "public excpect a list of strings"
  done  ;
  !set

let extract_pub (input : Ext_json_types.t Map_string.t) (cur_sources : Bsb_db.t) : Bsb_file_groups.public =   
  match Map_string.find_opt input  Bsb_build_schemas.public with 
  | Some ((Str({str = s}) as x)) ->  
    if s = Bsb_build_schemas.export_all then Export_all  else 
    if s = Bsb_build_schemas.export_none then Export_none else 
      errorf x "invalid str for %s "  s 
  | Some (Arr {content = s}) ->         
    Export_set (collect_pub_modules s cur_sources)
  | Some config -> 
    Bsb_exception.config_error config "expect array or string"
  | None ->
    Export_all 

let extract_resources (input : Ext_json_types.t Map_string.t) : string list =   
  match Map_string.find_opt input  Bsb_build_schemas.resources with 
  | Some (Arr x) ->
    Bsb_build_util.get_list_string x.content
  | Some config -> 
    Bsb_exception.config_error config 
      "expect array "  
  | None -> [] 


let extract_input_output (edge : Ext_json_types.t) : string list * string list = 
  let error () = 
    errorf edge {| invalid edge format, expect  ["output" , ":", "input" ]|}
  in  
  match edge with 
  | Arr {content} -> 
  (match Ext_array.find_and_split content 
          (fun x () -> match x with Str { str =":"} -> true | _ -> false )
          () with 
  | `No_split -> error ()
  | `Split (  output, input) -> 
    (Ext_array.to_list_map (fun (x : Ext_json_types.t) -> 
        match x with
        | Str {str = ":"} -> 
          error ()
        | Str {str } ->           
          Some str 
        | _ -> None) output
    ,
    Ext_array.to_list_map (fun (x : Ext_json_types.t) -> 
        match x with
        | Str {str = ":"} -> 
          error () 
        | Str {str} -> 
          Some str (* More rigirous error checking: It would trigger a ninja syntax error *)
        | _ -> None) input))
    | _ -> error ()    
type json_map = Ext_json_types.t Map_string.t

let extract_generators (input : json_map) : build_generator list  =
  match Map_string.find_opt input  Bsb_build_schemas.generators with
  | Some (Arr { content ; loc_start= _}) ->
    (* Need check is dev build or not *)
    Ext_array.fold_left content [] (fun acc x ->
        match x with
        | Obj { map } ->
          (match Map_string.find_opt map Bsb_build_schemas.name ,
                 Map_string.find_opt map Bsb_build_schemas.edge
           with
           | Some (Str command), Some edge ->
             let output, input = extract_input_output edge in 
             {Bsb_file_groups.input ; output ; command = command.str } :: acc
           | _ ->
             errorf x "Invalid generator format")
        | _ -> errorf x "Invalid generator format"
      )  
  | Some x  -> errorf x "Invalid generator format"
  | None -> []

let extract_predicate (m : json_map)  : string -> bool =
  let excludes = 
    match Map_string.find_opt m  Bsb_build_schemas.excludes with 
    | None -> []   
    | Some (Arr {content = arr}) -> Bsb_build_util.get_list_string arr 
    | Some x -> Bsb_exception.config_error x  "excludes expect array "in 
  let slow_re = Map_string.find_opt m Bsb_build_schemas.slow_re in 
  match slow_re, excludes with 
  | Some (Str {str = s}), [] -> 
    let re = Str.regexp s  in 
    fun name -> Str.string_match re name 0 
  | Some (Str {str = s}) , _::_ -> 
    let re = Str.regexp s in   
    fun name -> Str.string_match re name 0 && not (Ext_list.mem_string excludes name)
  | Some config, _ -> Bsb_exception.config_error config (Bsb_build_schemas.slow_re ^ " expect a string literal")
  | None , _ -> 
    fun name -> not (Ext_list.mem_string excludes name)

(** [parsing_source_dir_map cxt input]
    Major work done in this function, 
    assume [not toplevel && not (Bsb_dir_index.is_lib_dir dir_index)]      
    is already checked, so we don't need check it again    
*)
let try_unlink s = 
  try Unix.unlink s  
  with _ -> 
    Bsb_log.info "@{<info>Failed to remove %s}@." s 

let bs_cmt_post_process_cmd = 
  lazy (try Sys.getenv "BS_CMT_POST_PROCESS_CMD" with _ -> "")

type suffix_kind =   
   | Cmi of int | Cmt of int  | Cmj of int | Cmti of int
   | Not_any 

let classify_suffix (x : string) : suffix_kind =   
  let i =  
    Ext_string.ends_with_index x Literals.suffix_cmi in 
  if i >=0 then Cmi i
  else 
    let i =  
      Ext_string.ends_with_index x Literals.suffix_cmj in 
    if i >= 0 then Cmj i    
    else 
      let i =  
        Ext_string.ends_with_index x Literals.suffix_cmt in 
      if i >= 0 then Cmt i   
      else 
        let i =  
          Ext_string.ends_with_index x Literals.suffix_cmti in 
        if i >= 0 then Cmti i 
        else Not_any

(** This is the only place where we do some removal during scanning,
  configurabl
*)    
let prune_staled_bs_js_files 
    (context : cxt) 
    (cur_sources : _ Map_string.t ) 
     : unit =     
     (* Doesn't need to use Bsb_global_backend.lib_artifacts_dir because this is only for JS. *)
  let lib_parent = 
    Filename.concat (Filename.concat context.root Bsb_config.lib_bs) 
      context.cwd in 
  if Sys.file_exists lib_parent then
    let artifacts = Sys.readdir lib_parent in 
    Ext_array.iter artifacts (fun x ->       
        let kind = classify_suffix x  in
        match kind with 
        | Not_any -> ()
        | Cmi i | Cmt i | Cmj i | Cmti i -> 
          let j = 
            if context.namespace = None then i              
            else
              Ext_string.rindex_neg x '-' 
          in 
          if j >= 0 then
            let cmp = Ext_string.capitalize_sub x  j  in
            if not (Map_string.mem cur_sources cmp) then 
            begin (* prune action *)
              let filepath = Filename.concat lib_parent x in 
              (match kind with 
               | Cmt _ -> 
                 let lazy cmd =  bs_cmt_post_process_cmd in 

                 if cmd <> "" then
                   (try ignore (
                       Sys.command (
                         cmd ^ 
                         " -cmt-rm " ^ filepath)                   
                     : int ) with _ -> ())
                | Cmj _ ->        
                  (* remove .bs.js *)
                  if context.bs_suffix then
                    try_unlink 
                      (Filename.concat context.cwd
                         (String.sub x 0 j ^ Literals.suffix_bs_js)
                      )
               | _ -> ());
              try_unlink filepath
            end
            else () (* assert false *)
      )





(********************************************************************)  
(* starts parsing *)
let rec 
  parsing_source_dir_map 
    ({ cwd =  dir;} as cxt )
    (input : Ext_json_types.t Map_string.t) : Bsb_file_groups.t     
  = 
  if Set_string.mem cxt.ignored_dirs dir then Bsb_file_groups.empty
  else 
    let cur_globbed_dirs = ref false in 
    let has_generators = not (cxt.cut_generators || not cxt.toplevel) in          
    let scanned_generators = extract_generators input in        
    let sub_dirs_field = Map_string.find_opt input  Bsb_build_schemas.subdirs in 
    let base_name_array = 
        lazy (cur_globbed_dirs := true ; Sys.readdir (Filename.concat cxt.root dir)) in 
    let output_sources = 
      Ext_list.fold_left (Ext_list.flat_map scanned_generators (fun x -> x.output))
        Map_string.empty (fun acc o -> 
            Bsb_db_util.add_basename ~dir acc o) in 
    let sources = 
      match Map_string.find_opt input Bsb_build_schemas.files with 
      | None ->  
        (** We should avoid temporary files *)
        Ext_array.fold_left (Lazy.force base_name_array) output_sources (fun acc basename -> 
            if is_input_or_output scanned_generators basename then acc 
            else 
              Bsb_db_util.add_basename ~dir acc basename 
          ) 
      | Some (Arr basenames ) ->         
        Ext_array.fold_left basenames.content output_sources (fun acc basename ->
            match basename with 
            | Str {str = basename;loc} -> 
              Bsb_db_util.add_basename ~dir acc basename ~error_on_invalid_suffix:loc
            | _ -> acc
          ) 
      | Some (Obj {map = map; loc = _} ) -> (* { excludes : [], slow_re : "" }*)
        let predicate = extract_predicate map in 
        Ext_array.fold_left (Lazy.force base_name_array) output_sources (fun acc basename -> 
            if is_input_or_output scanned_generators basename || not (predicate basename) then acc 
            else 
              Bsb_db_util.add_basename  ~dir acc basename 
          ) 
      | Some x -> Bsb_exception.config_error x "files field expect array or object "
    in 
    let resources = extract_resources input in
    let public = extract_pub input sources in 
    (** Doing recursive stuff *)  
    let children =     
      match sub_dirs_field, 
            cxt.traverse with 
      | None , true
      | Some (True _), _ -> 
        let root = cxt.root in 
        let parent = Filename.concat root dir in
        Ext_array.fold_left (Lazy.force base_name_array) Bsb_file_groups.empty (fun origin x -> 
            if  not (Set_string.mem cxt.ignored_dirs x) && 
                Sys.is_directory (Filename.concat parent x) then 
              Bsb_file_groups.merge
                (
                  parsing_source_dir_map
                    {cxt with 
                     cwd = Ext_path.concat cxt.cwd 
                         (Ext_path.simple_convert_node_path_to_os_path x);
                     traverse = true
                    } Map_string.empty)  origin               
            else origin  
          ) 
      (* readdir parent avoiding scanning twice *)        
      | None, false  
      | Some (False _), _  -> Bsb_file_groups.empty
      | Some s, _  -> parse_sources cxt s 
    in 
    (** Do some clean up *)  
    prune_staled_bs_js_files cxt sources ;
    Bsb_file_groups.cons 
      ~file_group:{ dir ; 
                    sources = sources; 
                    resources ;
                    public ;
                    dir_index = cxt.dir_index ;
                    generators = if has_generators then scanned_generators else []  } 
      ?globbed_dir:(
        if !cur_globbed_dirs then Some dir else None)
      children


and parsing_single_source ({toplevel; dir_index ; cwd} as cxt ) (x : Ext_json_types.t )
  : t  =
  match x with 
  | Str  { str = dir }  -> 
    if not toplevel && not (Bsb_dir_index.is_lib_dir dir_index) then 
      Bsb_file_groups.empty
    else 
      parsing_source_dir_map 
        {cxt with 
         cwd = Ext_path.concat cwd (Ext_path.simple_convert_node_path_to_os_path dir)}
        Map_string.empty  
  | Obj {map} ->
    let current_dir_index = 
      match Map_string.find_opt map Bsb_build_schemas.type_ with 
      | Some (Str {str="dev"}) -> 
        Bsb_dir_index.get_dev_index ()
      | Some _ -> Bsb_exception.config_error x {|type field expect "dev" literal |}
      | None -> dir_index in 
    if not toplevel && not (Bsb_dir_index.is_lib_dir current_dir_index) then 
      Bsb_file_groups.empty 
    else 
      let dir = 
        match Map_string.find_opt map Bsb_build_schemas.dir with 
        | Some (Str{str}) -> 
          Ext_path.simple_convert_node_path_to_os_path str 
        | Some x -> Bsb_exception.config_error x "dir expected to be a string"
        | None -> 
          Bsb_exception.config_error x
            (
              "required field :" ^ Bsb_build_schemas.dir ^ " missing" )

      in
      parsing_source_dir_map 
        {cxt with dir_index = current_dir_index; 
                  cwd= Ext_path.concat cwd dir} map
  | _ -> Bsb_file_groups.empty
and  parsing_arr_sources cxt (file_groups : Ext_json_types.t array)  = 
  Ext_array.fold_left file_groups Bsb_file_groups.empty (fun  origin x ->
      Bsb_file_groups.merge (parsing_single_source cxt x) origin 
    ) 
and  parse_sources ( cxt : cxt) (sources : Ext_json_types.t )  = 
  match sources with   
  | Arr file_groups -> 
    parsing_arr_sources cxt file_groups.content
  | _ -> parsing_single_source cxt sources



let scan 
  ~toplevel 
  ~root 
  ~cut_generators 
  ~namespace 
  ~bs_suffix 
  ~ignored_dirs
  x : t * int = 
  Bsb_dir_index.reset ();
  let output = 
    parse_sources {
      ignored_dirs;
      toplevel;
      dir_index = Bsb_dir_index.lib_dir_index;
      cwd = Filename.current_dir_name;
      root ;
      cut_generators;
      namespace;
      bs_suffix;
      traverse = false
    } x in 
  output, Bsb_dir_index.get_current_number_of_dev_groups ()



(* Walk through to do some work *) 
type walk_cxt = {
    cwd : string ;
    root : string;
    traverse : bool;
    ignored_dirs : Set_string.t;
  }
  
let rec walk_sources (cxt : walk_cxt) (sources : Ext_json_types.t) = 
  match sources with 
  | Arr {content} -> 
    Ext_array.iter content (fun x -> walk_single_source cxt x) 
  | x -> walk_single_source  cxt x    
and walk_single_source cxt (x : Ext_json_types.t) =      
  match x with 
  | Str {str = dir} 
    -> 
    let dir = Ext_path.simple_convert_node_path_to_os_path dir in
    walk_source_dir_map 
    {cxt with cwd = Ext_path.concat cxt.cwd dir } None 
  | Obj {map} ->       
    begin match Map_string.find_opt map Bsb_build_schemas.dir with 
    | Some (Str{str}) -> 
      let dir = Ext_path.simple_convert_node_path_to_os_path str  in 
      walk_source_dir_map 
      {cxt with cwd = Ext_path.concat cxt.cwd dir} (Map_string.find_opt map Bsb_build_schemas.subdirs)
    | _ -> ()
    end
  | _ -> ()  
and walk_source_dir_map (cxt : walk_cxt)  sub_dirs_field =   
    let working_dir = Filename.concat cxt.root cxt.cwd in 
    if not (Set_string.mem cxt.ignored_dirs cxt.cwd) then begin 
      let file_array = Sys.readdir working_dir in 
      (* Remove .re.js when clean up *)
      Ext_array.iter file_array begin fun file -> 
        if Ext_string.ends_with file Literals.suffix_gen_js 
        || Ext_string.ends_with file Literals.suffix_gen_tsx 
        then 
          Sys.remove (Filename.concat working_dir file)
      end; 
      let cxt_traverse = cxt.traverse in     
      match sub_dirs_field, cxt_traverse with     
      | None, true 
      | Some(True _), _ -> 
        Ext_array.iter file_array begin fun f -> 
          if not (Set_string.mem cxt.ignored_dirs f) && 
             Sys.is_directory (Filename.concat working_dir f ) then 
            walk_source_dir_map 
              {cxt with 
               cwd = 
                 Ext_path.concat cxt.cwd
                   (Ext_path.simple_convert_node_path_to_os_path f);
               traverse = true
              } None 
        end   
      | None, _ 
      | Some (False _), _ -> ()      
      | Some s, _ -> walk_sources cxt s 
    end
(* It makes use of the side effect when [walk_sources], removing suffix_re_js,
   TODO: make it configurable
 *)
let clean_re_js root =     
  match Ext_json_parse.parse_json_from_file 
      (Filename.concat root Literals.bsconfig_json) with 
  | Obj { map } -> 
    let ignored_dirs = 
      match Map_string.find_opt map Bsb_build_schemas.ignored_dirs with       
      | Some (Arr {content = x}) -> Set_string.of_list (Bsb_build_util.get_list_string x )
      | Some _
      | None -> Set_string.empty
    in  
    Ext_option.iter (Map_string.find_opt map Bsb_build_schemas.sources) begin fun config -> 
      try (
          walk_sources { root ;                           
                         traverse = true; 
                         cwd = Filename.current_dir_name;
                         ignored_dirs
                         } config
        ) with _ -> ()      
    end
  | _  -> () 
  | exception _ -> ()    
  
end
module Bsb_config_parse : sig 
#1 "bsb_config_parse.mli"
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

val package_specs_from_bsconfig : 
    unit -> Bsb_package_specs.t




val interpret_json : 
    toplevel_package_specs:Bsb_package_specs.t option -> 
    per_proj_dir:string -> 
    Bsb_config_types.t






end = struct
#1 "bsb_config_parse.ml"
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


(* let get_list_string = Bsb_build_util.get_list_string *)
let (//) = Ext_path.combine
let current_package : Bsb_pkg_types.t = Global Bs_version.package_name
let resolve_package cwd  package_name = 
  let x =  Bsb_pkg.resolve_bs_package ~cwd package_name  in
  {
    Bsb_config_types.package_name ;
    package_install_path = x // !Bsb_global_backend.lib_ocaml_dir
  }

type json_map = Ext_json_types.t Map_string.t
(* Key is the path *)
let (|?)  m (key, cb) =
  m  |> Ext_json.test key cb


 
let extract_main_entries (_ :json_map) = []  


let package_specs_from_bsconfig () = 
  let json = Ext_json_parse.parse_json_from_file Literals.bsconfig_json in
  begin match json with
    | Obj {map} ->
      begin 
        match Map_string.find_opt map  Bsb_build_schemas.package_specs with 
        | Some x ->
          Bsb_package_specs.from_json x
        | None -> 
          Bsb_package_specs.default_package_specs
      end
    | _ -> assert false
  end





(*TODO: it is a little mess that [cwd] and [project dir] are shared*)


let extract_package_name_and_namespace
    (map : json_map) : string * string option =   
  let package_name = 
    match Map_string.find_opt map Bsb_build_schemas.name with 

    | Some (Str { str = "_" } as config)
      -> 
      Bsb_exception.config_error config "_ is a reserved package name"
    | Some (Str {str = name }) -> 
      name 
    | Some config -> 
      Bsb_exception.config_error config 
        "name expect a string field"  
    | None -> 
      Bsb_exception.invalid_spec
        "field name is required"
  in 
  let namespace = 
    match Map_string.find_opt map Bsb_build_schemas.namespace with 
    | None 
    | Some (False _) 
      -> None 
    | Some (True _) -> 
      Some (Ext_namespace.namespace_of_package_name package_name)
    | Some (Str {str}) -> 
      (*TODO : check the validity of namespace *)
      Some (Ext_namespace.namespace_of_package_name str)        
    | Some x ->
      Bsb_exception.config_error x 
      "namespace field expects string or boolean"
  in 
  package_name, namespace


(**
    There are two things to check:
    - the running bsb and vendoring bsb is the same
    - the running bsb need delete stale build artifacts
      (kinda check npm upgrade)
*)
let check_version_exit (map : json_map) stdlib_path =   
  match Map_string.find_exn map Bsb_build_schemas.version with 
  | Str {str } -> 
    if str <> Bs_version.version then 
      begin
        Format.fprintf Format.err_formatter
          "@{<error>bs-platform version mismatch@} Running bsb @{<info>%s@} (%s) vs vendored @{<info>%s@} (%s)@."
          Bs_version.version
          (Filename.dirname (Filename.dirname Sys.executable_name))
          str
          stdlib_path 
        ;
        exit 2
      end
  | _ -> assert false

let check_stdlib (map : json_map) cwd (*built_in_package*) =  
  match Map_string.find_opt map Bsb_build_schemas.use_stdlib with      
  | Some (False _) -> None    
  | None 
  | Some _ ->
    begin
      let stdlib_path = 
        Bsb_pkg.resolve_bs_package ~cwd current_package in 
      let json_spec = 
        Ext_json_parse.parse_json_from_file 
          (Filename.concat stdlib_path Literals.package_json) in 
      match json_spec with 
      | Obj {map}  -> 
        check_version_exit map stdlib_path;
        Some {
            Bsb_config_types.package_name = current_package;
            package_install_path = stdlib_path // !Bsb_global_backend.lib_ocaml_dir;
          }

      | _ -> assert false 

    end
let extract_bs_suffix_exn (map : json_map) =  
  match Map_string.find_opt map Bsb_build_schemas.suffix with 
  | None -> false  
  | Some (Str {str} as config ) -> 
    if str = Literals.suffix_js then false 
    else if str = Literals.suffix_bs_js then true
    else Bsb_exception.config_error config 
        "expect .bs.js or .js string here"
  | Some config -> 
    Bsb_exception.config_error config 
      "expect .bs.js or .js string here"

let extract_gentype_config (map : json_map) cwd 
  : Bsb_config_types.gentype_config option = 
  match Map_string.find_opt map Bsb_build_schemas.gentypeconfig with 
  | None -> None
  | Some (Obj {map = obj}) -> 
    Some { path = 
             match Map_string.find_opt obj Bsb_build_schemas.path with
             | None -> 
               (Bsb_build_util.resolve_bsb_magic_file
                 ~cwd ~desc:"gentype.exe"
                 "gentype/gentype.exe").path
             | Some (Str {str}) ->  
               (Bsb_build_util.resolve_bsb_magic_file
                 ~cwd ~desc:"gentype.exe" str).path 
             | Some config -> 
               Bsb_exception.config_error config
                 "path expect to be a string"
         }

  | Some config -> 
    Bsb_exception.config_error 
      config "gentypeconfig expect an object"  

let extract_refmt (map : json_map) cwd : Bsb_config_types.refmt =      
  match Map_string.find_opt map Bsb_build_schemas.refmt with 
  | Some (Flo {flo} as config) -> 
    begin match flo with 
      | "3" -> None
      | _ -> Bsb_exception.config_error config "expect version 3 only"
    end
  | Some (Str {str}) 
    -> 
    Some
      (Bsb_build_util.resolve_bsb_magic_file 
              ~cwd ~desc:Bsb_build_schemas.refmt str).path
  | Some config  -> 
    Bsb_exception.config_error config "expect version 2 or 3"
  | None ->
    None

let extract_string (map : json_map) (field : string) cb = 
  match Map_string.find_opt map field with 
  | None -> None 
  | Some (Str{str}) -> cb str 
  | Some config -> 
    Bsb_exception.config_error config (field ^ " expect a string" )
  
let extract_boolean (map : json_map) (field : string) (default : bool) : bool = 
  match Map_string.find_opt map field with 
  | None -> default 
  | Some (True _ ) -> true
  | Some (False _) -> false 
  | Some config -> 
    Bsb_exception.config_error config (field ^ " expect a boolean" )
  
let extract_reason_react_jsx (map : json_map) = 
  let default : Bsb_config_types.reason_react_jsx option ref = ref None in 
  map
  |? (Bsb_build_schemas.reason, `Obj begin fun m -> 
      match Map_string.find_opt m Bsb_build_schemas.react_jsx with 
      | Some (Flo{loc; flo}) -> 
        begin match flo with 
          | "2" -> 
            default := Some Jsx_v2
          | "3" -> 
            default := Some Jsx_v3
          | _ -> Bsb_exception.errorf ~loc "Unsupported jsx version %s" flo
        end        
      | Some x -> Bsb_exception.config_error x 
                    "Unexpected input (expect a version number) for jsx, note boolean is no longer allowed"
      | None -> ()
    end)
  |> ignore;
  !default

let extract_warning (map : json_map) = 
  match Map_string.find_opt map Bsb_build_schemas.warnings with 
  | None -> Bsb_warning.use_default 
  | Some (Obj {map }) -> Bsb_warning.from_map map 
  | Some config -> Bsb_exception.config_error config "expect an object"

let extract_ignored_dirs (map : json_map) =   
  match Map_string.find_opt map Bsb_build_schemas.ignored_dirs with 
  | None -> Set_string.empty
  | Some (Arr {content}) -> 
    Set_string.of_list (Bsb_build_util.get_list_string content)
  | Some config -> 
    Bsb_exception.config_error config "expect an array of string"  

let extract_generators (map : json_map) = 
  let generators = ref Map_string.empty in 
  (match Map_string.find_opt map Bsb_build_schemas.generators with 
   | None -> ()
   | Some (Arr {content = s}) -> 
     generators :=
       Ext_array.fold_left s Map_string.empty (fun acc json -> 
           match json with 
           | Obj {map = m ; loc}  -> 
             begin match Map_string.find_opt  m Bsb_build_schemas.name,
                         Map_string.find_opt  m Bsb_build_schemas.command with 
             | Some (Str {str = name}), Some ( Str {str = command}) -> 
               Map_string.add acc name command 
             | _, _ -> 
               Bsb_exception.errorf ~loc {| generators exepect format like { "name" : "cppo",  "command"  : "cppo $in -o $out"} |}
             end
           | _ -> acc )
   | Some config ->
     Bsb_exception.config_error config (Bsb_build_schemas.generators ^ " expect an array field")       
  );
  !generators
  

let extract_dependencies (map : json_map) cwd (field : string )
  : (string list * Bsb_config_types.dependencies) =   
  match Map_string.find_opt map field with 
  | None -> ([], [])
  | Some (Arr ({content = s})) -> 
    let (otherlibs, l) = List.partition (Bsb_default.filter_otherlibs) (Bsb_build_util.get_list_string s) in
    (otherlibs, (Ext_list.map l (fun s -> resolve_package cwd (Bsb_pkg_types.string_as_package s))))
  | Some config -> 
    Bsb_exception.config_error config 
      (field ^ " expect an array")
  
(* return an empty array if not found *)     
let extract_string_list (map : json_map) (field : string) : string list = 
  match Map_string.find_opt map field with 
  | None -> []
  | Some (Arr {content = s}) -> 
    Bsb_build_util.get_list_string s 
  | Some config ->   
    Bsb_exception.config_error config (field ^ " expect an array")

let extract_ppx 
  (map : json_map) 
  (field : string) 
  ~(cwd : string) : Bsb_config_types.ppx list =     
  match Map_string.find_opt map field with 
  | None -> []
  | Some (Arr {content }) -> 
    let resolve s = 
      if s = "" then Bsb_exception.invalid_spec "invalid ppx, empty string found"
      else 
        (Bsb_build_util.resolve_bsb_magic_file ~cwd ~desc:Bsb_build_schemas.ppx_flags s).path in 
    Ext_array.to_list_f content (fun x -> 
      match x with 
      | Str x ->    
      
        {Bsb_config_types.name = 
          resolve x.str; 
          args = []}
      | Arr {content } -> 

          let xs = Bsb_build_util.get_list_string content in 
          (match xs with 
          | [] -> Bsb_exception.config_error x " empty array is not allowed"
          | name :: args -> 
            {Bsb_config_types.name = resolve name ; args}
          )
      | config -> Bsb_exception.config_error config 
        (field ^ "expect each item to be either string or array")
    )
  | Some config -> 
    Bsb_exception.config_error config (field ^ " expect an array")



let extract_js_post_build (map : json_map) cwd : string option = 
  let js_post_build_cmd = ref None in 
  map    
  |? (Bsb_build_schemas.js_post_build, `Obj begin fun m ->
      m |? (Bsb_build_schemas.cmd , `Str (fun s -> 
          js_post_build_cmd := Some (Bsb_build_util.resolve_bsb_magic_file ~cwd ~desc:Bsb_build_schemas.js_post_build s).path

        )
        )
      |> ignore
    end)

  |> ignore ;
  !js_post_build_cmd

let extract_static_libraries (map: json_map) =
  match Map_string.find_opt map Bsb_build_schemas.static_libraries with 
  | None -> []
  | Some (Arr ({content = s})) -> Bsb_build_util.get_list_string s
  | Some config -> Bsb_exception.config_error config (Bsb_build_schemas.static_libraries ^ " expect an array")

(** ATT: make sure such function is re-entrant. 
    With a given [cwd] it works anywhere*)
let interpret_json 
    ~toplevel_package_specs
    ~per_proj_dir:(per_proj_dir:string)

  : Bsb_config_types.t =

  (** we should not resolve it too early,
      since it is external configuration, no {!Bsb_build_util.convert_and_resolve_path}
  *)
  
  
 
  
  (* When we plan to add more deps here,
     Make sure check it is consistent that for nested deps, we have a 
     quck check by just re-parsing deps 
     Make sure it works with [-make-world] [-clean-world]
  *)
  
  (* Setting ninja is a bit complex
     1. if [build.ninja] does use [ninja] we need set a variable
     2. we need store it so that we can call ninja correctly
  *)
  match  Ext_json_parse.parse_json_from_file (per_proj_dir // Literals.bsconfig_json) with
  | Obj { map } ->
    let package_name, namespace = 
      extract_package_name_and_namespace  map in 
    let refmt = extract_refmt map per_proj_dir in 
    let gentype_config  = extract_gentype_config map per_proj_dir in  
    let bs_suffix = extract_bs_suffix_exn map in   
    (* This line has to be before any calls to Bsb_global_backend.backend, because it'll read the entries 
        array from the bsconfig and set the backend_ref to the first entry, if any. *)
    let entries = extract_main_entries map in
    (* The default situation is empty *)
    let built_in_package = check_stdlib map per_proj_dir in
    let package_specs =     
      match Map_string.find_opt map Bsb_build_schemas.package_specs with 
      | Some x ->
        Bsb_package_specs.from_json x 
      | None ->  Bsb_package_specs.default_package_specs 
    in
    let pp_flags : string option = 
      extract_string map Bsb_build_schemas.pp_flags (fun p -> 
        if p = "" then 
          Bsb_exception.invalid_spec "invalid pp, empty string found"
        else 
          Some (Bsb_build_util.resolve_bsb_magic_file ~cwd:per_proj_dir ~desc:Bsb_build_schemas.pp_flags p).path
      ) in 
    let static_libraries = extract_static_libraries map in
    let c_linker_flags = begin match Map_string.find_opt map Bsb_build_schemas.c_linker_flags with 
    | None -> []
    | Some (Arr ({content = s})) -> Bsb_build_util.get_list_string s
    | Some config -> Bsb_exception.config_error config (Bsb_build_schemas.c_linker_flags ^ " expect an array")
    end in
    let reason_react_jsx = extract_reason_react_jsx map in 
    let (otherlibs, bs_dependencies) = extract_dependencies map per_proj_dir Bsb_build_schemas.bs_dependencies in 
    let toplevel = toplevel_package_specs = None in 
    let (dev_otherlibs, bs_dev_dependencies) = 
      if toplevel then 
        extract_dependencies map per_proj_dir Bsb_build_schemas.bs_dev_dependencies
      else ([], []) in 
    begin match Map_string.find_opt map Bsb_build_schemas.sources with 
      | Some sources -> 
        let cut_generators = 
          extract_boolean map Bsb_build_schemas.cut_generators false in 
        let groups, number_of_dev_groups = Bsb_parse_sources.scan
            ~ignored_dirs:(extract_ignored_dirs map)
            ~toplevel
            ~root: per_proj_dir
            ~cut_generators
            ~bs_suffix
            ~namespace
            sources in         
        {
          gentype_config;
          bs_suffix ;
          package_name ;
          namespace ;    
          warning = extract_warning map;
          external_includes = extract_string_list map Bsb_build_schemas.bs_external_includes;
          bsc_flags = extract_string_list map Bsb_build_schemas.bsc_flags ;
          ppx_files = extract_ppx map ~cwd:per_proj_dir Bsb_build_schemas.ppx_flags;
          pp_file = pp_flags ;          
          bs_dependencies ;
          bs_dev_dependencies ;
          otherlibs;
          dev_otherlibs;
          (*
            reference for quoting
             {[
               let tmpfile = Filename.temp_file "ocamlpp" "" in
               let comm = Printf.sprintf "%s %s > %s"
                   pp (Filename.quote sourcefile) tmpfile
               in
             ]}
          *)          
          refmt;
          js_post_build_cmd = (extract_js_post_build map per_proj_dir);
          package_specs = 
            (match toplevel_package_specs with 
             | None ->  package_specs
             | Some x -> x );          
          file_groups = groups; 
          files_to_install = Hash_set_string.create 96;
          built_in_dependency = built_in_package;
          generate_merlin = 
            extract_boolean map Bsb_build_schemas.generate_merlin true;
          reason_react_jsx  ;  
          entries;
          generators = extract_generators map ; 
          cut_generators ;
          number_of_dev_groups;   
          static_libraries;
          c_linker_flags;
        }
      | None -> 
          Bsb_exception.invalid_spec
            "no sources specified in bsconfig.json"
    end
  | _ -> 
    Bsb_exception.invalid_spec "bsconfig.json expect a json object {}"

end
module Bsb_dependency_info : sig 
#1 "bsb_dependency_info.mli"
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


end = struct
#1 "bsb_dependency_info.ml"
type path = string

type t = {
  mutable static_libraries: path list;
  mutable all_external_deps: path list;
  mutable all_c_linker_flags: string list;
  mutable all_otherlibs: string list;
}

end
module Bsb_global_paths_native
= struct
#1 "bsb_global_paths_native.ml"
let bsb_native_dir  = 
  Filename.dirname (Ext_path.normalize_absolute_path (Ext_path.combine (Sys.getcwd ())  Sys.executable_name))

let vendor_bsdep =     
  Filename.concat bsb_native_dir "bsb_helper.exe"

let ocaml_version = "4.06.1"

let ocaml_dir =
  Filename.(concat (concat (dirname bsb_native_dir) "native") ocaml_version)

let ocaml_lib_dir =
  Filename.(concat (concat ocaml_dir "lib") "ocaml")

end
module Bsb_global_paths : sig 
#1 "bsb_global_paths.mli"
(* Copyright (C) 2019 - Authors of BuckleScript
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


(* val cwd : string  *)

val vendor_bsc : string -> string

val vendor_ninja : string -> string


end = struct
#1 "bsb_global_paths.ml"
(* Copyright (C) 2019 - Authors of BuckleScript
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

(* let cwd = Sys.getcwd () *)


(**
   If [Sys.executable_name] gives an absolute path, 
   nothing needs to be done.
   
   If [Sys.executable_name] is not an absolute path, for example
   (rlwrap ./ocaml)
   it is a relative path, 
   it needs be adapted based on cwd

   if [Sys.executable_name] gives an absolute path, 
   nothing needs to be done
   if it is a relative path 

   there are two cases: 
   - bsb.exe
   - ./bsb.exe 
   The first should also not be touched
   Only the latter need be adapted based on project root  
*)

(* TODO: Is this hacky? *)
(* let bsc_dir  =  Sys.argv.(4) *)

let vendor_bsc bsc_dir =        
  Filename.concat bsc_dir  "bsc.exe"


let vendor_ninja bsc_dir = 
    Filename.concat bsc_dir "ninja.exe"      
  
(* ;; assert (Sys.file_exists bsc_dir)        *)

end
module Ext_io : sig 
#1 "ext_io.mli"
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

val load_file : string -> string

val rev_lines_of_file : string -> string list

val rev_lines_of_chann : in_channel -> string list

val write_file : string -> string -> unit

end = struct
#1 "ext_io.ml"
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


(** on 32 bit , there are 16M limitation *)
let load_file f =
  Ext_pervasives.finally (open_in_bin f) ~clean:close_in begin fun ic ->   
    let n = in_channel_length ic in
    let s = Bytes.create n in
    really_input ic s 0 n;
    Bytes.unsafe_to_string s
  end


let  rev_lines_of_chann chan = 
    let rec loop acc chan = 
      match input_line chan with
      | line -> loop (line :: acc) chan
      | exception End_of_file -> close_in chan ; acc in
    loop [] chan


let rev_lines_of_file file = 
  Ext_pervasives.finally 
    ~clean:close_in 
    (open_in_bin file) rev_lines_of_chann


let write_file f content = 
  Ext_pervasives.finally ~clean:close_out 
    (open_out_bin f)  begin fun oc ->   
    output_string oc content
  end

end
module Bsb_merlin_gen : sig 
#1 "bsb_merlin_gen.mli"
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




val merlin_file_gen : 
  per_proj_dir:string  -> 
  bsc_dir:string ->
  Bsb_config_types.t ->  
  unit 

end = struct
#1 "bsb_merlin_gen.ml"
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


let merlin = ".merlin"
let merlin_header = "####{BSB GENERATED: NO EDIT"
let merlin_trailer = "####BSB GENERATED: NO EDIT}"
let merlin_trailer_length = String.length merlin_trailer
let (//) = Ext_path.combine

(** [new_content] should start end finish with newline *)
let revise_merlin merlin new_content =
  if Sys.file_exists merlin then
    let s = Ext_io.load_file merlin in 
    let header =  Ext_string.find s ~sub:merlin_header  in
    let tail = Ext_string.find s ~sub:merlin_trailer in
    if header < 0  && tail < 0 then (* locked region not added yet *)
      let ochan = open_out_bin merlin in
      output_string ochan s ;
      output_string ochan "\n";
      output_string ochan merlin_header;
      Buffer.output_buffer ochan new_content;
      output_string ochan merlin_trailer ;
      output_string ochan "\n";
      close_out ochan
    else if header >=0 && tail >= 0  then
      (* there is one, hit it everytime,
         should be fixed point
      *)
      let ochan = open_out_bin merlin in
      output_string ochan (String.sub s 0 header) ;
      output_string ochan merlin_header;
      Buffer.output_buffer ochan new_content;
      output_string ochan merlin_trailer ;
      output_string ochan (Ext_string.tail_from s (tail +  merlin_trailer_length));
      close_out ochan
    else failwith ("the .merlin is corrupted, locked region by bsb is not consistent ")
  else
    let ochan = open_out_bin merlin in
    output_string ochan merlin_header ;
    Buffer.output_buffer ochan new_content;
    output_string ochan merlin_trailer ;
    output_string ochan "\n";
    close_out ochan

(* ATTENTION: order matters here, need resolve global properties before
   merlin generation
*)
let merlin_flg_ppx = "\nFLG -ppx " 
let merlin_flg_pp = "\nFLG -pp "
let merlin_s = "\nS "
let merlin_b = "\nB "


let merlin_flg = "\nFLG "
let bs_flg_prefix = "-bs-"

let output_merlin_namespace buffer ns= 
  match ns with 
  | None -> ()
  | Some x -> 
    let lib_artifacts_dir = !Bsb_global_backend.lib_artifacts_dir in
    Buffer.add_string buffer merlin_b ; 
    Buffer.add_string buffer lib_artifacts_dir ; 
    Buffer.add_string buffer merlin_flg ; 
    Buffer.add_string buffer "-open ";
    Buffer.add_string buffer x 

let bsc_flg_to_merlin_ocamlc_flg bsc_flags  =
  merlin_flg ^ 
  String.concat Ext_string.single_space 
    (List.filter (fun x -> not (Ext_string.starts_with x bs_flg_prefix )) @@ bsc_flags)

(* No need for [-warn-error] in merlin  *)     
let warning_to_merlin_flg (warning: Bsb_warning.t ) : string=     
  merlin_flg ^ Bsb_warning.to_merlin_string warning


let merlin_file_gen ~per_proj_dir:(per_proj_dir:string) ~bsc_dir
    ({file_groups = res_files ; 
      generate_merlin;
      ppx_files;
      pp_file;
      bs_dependencies;
      bs_dev_dependencies;
      bsc_flags; 
      built_in_dependency;
      external_includes; 
      reason_react_jsx ; 
      namespace;
      package_name = _;
      warning; 
     } : Bsb_config_types.t)
  =
  if generate_merlin then begin     
    let buffer = Buffer.create 1024 in
    output_merlin_namespace buffer namespace; 
    Ext_list.iter ppx_files (fun ppx ->
        Buffer.add_string buffer merlin_flg_ppx;
        if ppx.args = [] then 
          Buffer.add_string buffer ppx.name
        else   
          let fmt : _ format = 
            if Ext_sys.is_windows_or_cygwin then 
              "\"%s %s\""
            else "'%s %s'" in 
          Buffer.add_string buffer 
            (Printf.sprintf fmt ppx.name (String.concat " " ppx.args))
      );
    Ext_option.iter pp_file (fun x -> 
      Buffer.add_string buffer (merlin_flg_pp ^ x)
    );  
    Buffer.add_string buffer 
      (merlin_flg_ppx  ^ 
       (match reason_react_jsx with 
        | None -> 
          let fmt : _ format = 
            if Ext_sys.is_windows_or_cygwin then
              "\"%s -as-ppx \"" 
            else  "'%s -as-ppx '"  in Printf.sprintf fmt (Bsb_global_paths.vendor_bsc bsc_dir)
        | Some opt ->
          let fmt : _ format = 
            if Ext_sys.is_windows_or_cygwin then
              "\"%s -as-ppx -bs-jsx %d\"" 
            else  "'%s -as-ppx -bs-jsx %d'" 
          in 
          Printf.sprintf fmt  (Bsb_global_paths.vendor_bsc bsc_dir)
            (match opt with Jsx_v2 -> 2 | Jsx_v3 -> 3)
       )
      );    
    Ext_list.iter external_includes (fun path -> 
        Buffer.add_string buffer merlin_s ;
        Buffer.add_string buffer path ;
        Buffer.add_string buffer merlin_b;
        Buffer.add_string buffer path ;
      );      
    Ext_option.iter built_in_dependency (fun package -> 
        let path = package.package_install_path in 
        Buffer.add_string buffer (merlin_s ^ path );
        Buffer.add_string buffer (merlin_b ^ path)                      
      );
    let bsc_string_flag = bsc_flg_to_merlin_ocamlc_flg bsc_flags in 
    Buffer.add_string buffer bsc_string_flag ;
    Buffer.add_string buffer (warning_to_merlin_flg  warning); 
    Ext_list.iter bs_dependencies (fun package ->
        let path = package.package_install_path in
        Buffer.add_string buffer merlin_s ;
        Buffer.add_string buffer path ;
        Buffer.add_string buffer merlin_b;
        Buffer.add_string buffer path ;
      );
    Ext_list.iter bs_dev_dependencies (**TODO: shall we generate .merlin for dev packages ?*)
    (fun package ->    
        let path = package.package_install_path in
        Buffer.add_string buffer merlin_s ;
        Buffer.add_string buffer path ;
        Buffer.add_string buffer merlin_b;
        Buffer.add_string buffer path ;
      );
    let lib_artifacts_dir = !Bsb_global_backend.lib_artifacts_dir in
    Ext_list.iter res_files.files (fun x -> 
        if not (Bsb_file_groups.is_empty x) then 
          begin
            Buffer.add_string buffer merlin_s;
            Buffer.add_string buffer x.dir ;
            Buffer.add_string buffer merlin_b;
            Buffer.add_string buffer (lib_artifacts_dir//x.dir) ;
          end
      ) ;
    Buffer.add_string buffer "\n";
    revise_merlin (per_proj_dir // merlin) buffer 
  end



end
module Bsb_db_encode : sig 
#1 "bsb_db_encode.mli"
(* Copyright (C) 2019 - Present Authors of BuckleScript
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

val write_build_cache : 
  dir:string -> Bsb_db.ts -> string

end = struct
#1 "bsb_db_encode.ml"
(* Copyright (C) 2019 - Present Authors of BuckleScript
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

 
let bsbuild_cache = Literals.bsbuild_cache


let nl buf = 
  Ext_buffer.add_char buf '\n'



(* IDEAS: 
  Pros: 
    - could be even shortened to a single byte
  Cons: 
    - decode would allocate
    - code too verbose
    - not readable 
 *)  

let make_encoding length buf =
  let max_range = length lsl 1 + 1 in 
  if max_range <= 0xff then begin 
    Ext_buffer.add_char buf '1';
    Ext_buffer.add_int_1
  end
  else if max_range <= 0xff_ff then begin 
    Ext_buffer.add_char buf '2';
    Ext_buffer.add_int_2
  end
  else if length <= 0x7f_ff_ff then begin 
    Ext_buffer.add_char buf '3';
    Ext_buffer.add_int_3
  end
  else if length <= 0x7f_ff_ff_ff then begin
    Ext_buffer.add_char buf '4';
    Ext_buffer.add_int_4
  end else assert false 
(* Make sure [tmp_buf1] and [tmp_buf2] is cleared ,
  they are only used to control the order.
  Strictly speaking, [tmp_buf1] is not needed
*)
let encode_single (db : Bsb_db.t) (buf : Ext_buffer.t) =    
  nl buf ; (* module name section *)
  let len = Map_string.cardinal db in 
  Ext_buffer.add_string_char buf (string_of_int len) '\n';
  let mapping = Hash_string.create 50 in 
  Map_string.iter db (fun name {dir} ->  
      Ext_buffer.add_string_char buf name '\n'; 
      if not (Hash_string.mem mapping dir) then
        Hash_string.add mapping dir (Hash_string.length mapping)
    ); 
  let length = Hash_string.length mapping in   
  let rev_mapping = Array.make length "" in 
  Hash_string.iter mapping (fun k i -> Array.unsafe_set rev_mapping i k);
  (* directory name section *)
  Ext_array.iter rev_mapping (fun s -> Ext_buffer.add_string_char buf s '\t');
  nl buf; (* module name info section *)
  let len_encoding = make_encoding length buf in 
  Map_string.iter db (fun _ module_info ->       
      len_encoding buf 
        (Hash_string.find_exn  mapping module_info.dir lsl 1 + Obj.magic module_info.case ))      
    
let encode (dbs : Bsb_db.ts) buf =     
  
  Ext_buffer.add_char_string buf '\n' (string_of_int (Array.length dbs)); 
  Ext_array.iter dbs (fun x ->  encode_single x  buf)
  

(* TODO: shall we avoid writing such file (checking the digest) *)
let write_build_cache ~dir (bs_files : Bsb_db.ts)  : string = 
  let oc = open_out_bin (Filename.concat dir bsbuild_cache) in 
  let buf = Ext_buffer.create 100_000 in 
  encode bs_files buf ; 
  let digest = Ext_buffer.digest buf in 
  let hex_digest = Digest.to_hex digest in
  output_string oc digest;
  Ext_buffer.output_buffer oc buf;
  close_out oc; 
  hex_digest

end
module Ext_digest : sig 
#1 "ext_digest.mli"
(* Copyright (C) 2019- Authors of BuckleScript
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


 val length : int 

 val hex_length : int
end = struct
#1 "ext_digest.ml"
(* Copyright (C) 2019- Authors of BuckleScript
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


 let length = 16

 let hex_length = 32
end
module Bsb_namespace_map_gen : sig 
#1 "bsb_namespace_map_gen.mli"
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

(** [output dir namespace file_groups]
    when [build.ninja] is generated, we output a module map [.mlmap] file 
    such [.mlmap] file will be consumed by [bsc.exe] to generate [.cmi] file
 *)
val output : 
  dir:string ->
  string -> 
  Bsb_file_groups.file_groups ->
  unit 
end = struct
#1 "bsb_namespace_map_gen.ml"
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

let (//) = Ext_path.combine





let write_file fname digest contents = 
  let oc = open_out_bin fname in 
  Digest.output oc digest;
  output_char oc '\n';
  Ext_buffer.output_buffer oc contents;
  close_out oc 
(** 
  TODO:
  sort filegroupts to ensure deterministic behavior
  
  if [.bsbuild] is not changed
  [.mlmap] does not need to be changed too
  
*)
let output 
    ~dir 
    (namespace : string)
    (file_groups : Bsb_file_groups.file_groups )
  = 
  let fname = namespace ^ Literals.suffix_mlmap in 
  let buf = Ext_buffer.create 10000 in   
  Ext_list.iter file_groups 
    (fun  x ->
       Map_string.iter x.sources (fun k _ -> 
           Ext_buffer.add_string_char buf k '\n';
         ) 
    );
  (* let contents = Buffer.contents buf in    *)
  let digest = Ext_buffer.digest buf in 
  let fname = (dir// fname ) in 
  if Sys.file_exists fname then
    let ic = open_in_bin fname in 
    let old_digest = really_input_string ic Ext_digest.length in 
    close_in ic ;
    (if old_digest <> digest then 
      write_file fname digest buf)
  else 
    write_file fname digest buf
    
  
end
module Bsb_ninja_global_vars
= struct
#1 "bsb_ninja_global_vars.ml"
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


 
let g_pkg_flg = "g_pkg_flg"

let bsc = "bsc" 

let src_root_dir = "src_root_dir"
let bsdep = "bsdep"

let bsc_flags = "bsc_flags"

let ppx_flags = "ppx_flags"

let pp_flags = "pp_flags"


let g_dpkg_incls = "g_dpkg_incls"

let refmt = "refmt"

let refmt_flags = "refmt_flags"

let postbuild = "postbuild"

let g_ns = "g_ns" 

let warnings = "warnings"

let gentypeconfig = "gentypeconfig"

let g_dev_incls = "g_dev_incls"



end
module Bsb_ninja_rule : sig 
#1 "bsb_ninja_rule.mli"
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


end = struct
#1 "bsb_ninja_rule.ml"
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






type t = { 
  mutable used : bool; 
  rule_name : string; 
  name : out_channel -> string 
}

let get_name (x : t) oc = x.name oc
let print_rule (oc : out_channel) 
  ~description 
  ?(restat : unit option)  
  ?dyndep 
  ~command   
  name  =
  output_string oc "rule "; output_string oc name ; output_string oc "\n";
  output_string oc "  command = "; output_string oc command; output_string oc "\n";
  Ext_option.iter dyndep (fun f ->
      output_string oc "  dyndep = "; output_string oc f; output_string oc  "\n"
    );
  (if restat <>  None then   
     output_string oc "  restat = 1\n");

  output_string oc "  description = " ; output_string oc description; output_string oc "\n"




(** allocate an unique name for such rule*)
let define
    ~command
    ?dyndep
    ?restat
    ?(description = "\027[34mBuilding\027[39m \027[2m${out}\027[22m") (* blue, dim *)
    rule_name : t 
  =

  let rec self = {
    used  = false;
    rule_name ;
    name = fun oc ->
      if not self.used then
        begin
          print_rule oc ~description  ?dyndep ?restat ~command rule_name;
          self.used <- true
        end ;
      rule_name
  } in 

  self




type command = string

type builtin = {
  build_ast : t;
  (** TODO: Implement it on top of pp_flags *)
  build_ast_from_re : t ;
  (* build_ast_from_rei : t ; *)


  (** platform dependent, on Win32,
      invoking cmd.exe
  *)
  copy_resources : t;
  (** Rules below all need restat *)
  build_bin_deps : t ;



  ml_cmj_js : t;
  ml_cmj_js_dev : t;
  ml_cmj_cmi_js : t ;
  ml_cmj_cmi_js_dev : t ;
  ml_cmi : t;
  ml_cmi_dev : t ;
  
  build_package : t ;
  customs : t Map_string.t
}


;;

let make_custom_rules 
  ~(has_gentype : bool)        
  ~(has_postbuild : bool)
  ~(has_ppx : bool)
  ~(has_pp : bool)
  ~(has_builtin : bool)
  ~(bs_suffix : bool)
  ~(reason_react_jsx : Bsb_config_types.reason_react_jsx option)
  ~(digest : string)
  ~(refmt : string option) (* set refmt path when needed *)
  (custom_rules : command Map_string.t) : 
  builtin = 
  (** FIXME: We don't need set [-o ${out}] when building ast 
      since the default is already good -- it does not*)
  let buf = Buffer.create 100 in     
  let mk_ml_cmj_cmd 
      ~read_cmi 
      ~is_dev 
      ~postbuild : string =     
    Buffer.clear buf;
    Buffer.add_string buf "$bsc $g_pkg_flg -color always";
    if bs_suffix then
      Buffer.add_string buf " -bs-suffix";
    if read_cmi then 
      Buffer.add_string buf " -bs-read-cmi";
    if is_dev then 
      Buffer.add_string buf " $g_dev_incls";      
    Buffer.add_string buf " $g_lib_incls" ;
    if is_dev then
      Buffer.add_string buf " $g_dpkg_incls";
    if not has_builtin then   
      Buffer.add_string buf " -nostdlib";
    Buffer.add_string buf " $warnings $bsc_flags";
    if has_gentype then
      Buffer.add_string buf " $gentypeconfig";
    Buffer.add_string buf " -o $out $in";
    if postbuild then
      Buffer.add_string buf " $postbuild";
    Buffer.contents buf
  in   
  let mk_ast ~(has_pp : bool) ~has_ppx ~has_reason_react_jsx : string =
    Buffer.clear buf ; 
    Buffer.add_string buf "$bsc  $warnings -color always";
    (match refmt with 
    | None -> ()
    | Some x ->
      Buffer.add_string buf " -bs-refmt ";
      Buffer.add_string buf (Ext_filename.maybe_quote x);
    );
    if has_pp then
      Buffer.add_string buf " $pp_flags";
    (match has_reason_react_jsx, reason_react_jsx with
     | false, _ 
     | _, None -> ()
     | _, Some Jsx_v2
       -> Buffer.add_string buf " -bs-jsx 2"
     | _, Some Jsx_v3 
       -> Buffer.add_string buf " -bs-jsx 3"
    );
    if has_ppx then 
      Buffer.add_string buf " $ppx_flags"; 
    Buffer.add_string buf " -bs-simple-binary-ast";
    Buffer.add_string buf " $bsc_flags -o $out -bs-syntax-only -bs-binary-ast $in";   
    Buffer.contents buf
  in  
  let build_ast =
    define
      ~command:(mk_ast ~has_pp ~has_ppx ~has_reason_react_jsx:false)
      "build_ast" in
  let build_ast_from_re =
    define
      ~command:(mk_ast ~has_pp ~has_ppx ~has_reason_react_jsx:true)
      "build_ast_from_re" in 
 
  let copy_resources =    
    define 
      ~command:(
        if Ext_sys.is_windows_or_cygwin then
          "cmd.exe /C copy /Y $in $out > null" 
        else "cp $in $out"
      )
      "copy_resource" in
  let native_arg = match !Bsb_global_backend.backend with 
    | Native -> " -MD-native"
    | Bytecode -> " -MD-bytecode" 
  in
  let build_bin_deps =
    define
      ~restat:()
      ~command:
      ("$bsdep -hash " ^ digest ^" $g_ns -g $bsb_dir_group "^native_arg^" $in")
      "build_deps" in 
  let aux ~name ~read_cmi  ~postbuild =
    let postbuild = has_postbuild && postbuild in 
    define
      ~command:(mk_ml_cmj_cmd 
                  ~read_cmi  ~is_dev:false 
                  ~postbuild)
      ~dyndep:"$in_e.d"
      ~restat:() (* Always restat when having mli *)
      name,
    define
      ~command:(mk_ml_cmj_cmd 
                  ~read_cmi  ~is_dev:true
                  ~postbuild)
      ~dyndep:"$in_e.d"
      ~restat:() (* Always restat when having mli *)
      (name ^ "_dev")
  in 
  (* [g_lib_incls] are fixed for libs *)
  let ml_cmj_js, ml_cmj_js_dev =
    aux ~name:"ml_cmj_only" ~read_cmi:true ~postbuild:true in   
  let ml_cmj_cmi_js, ml_cmj_cmi_js_dev =
    aux
      ~read_cmi:false 
      ~name:"ml_cmj_cmi" ~postbuild:true in  
  let ml_cmi, ml_cmi_dev =
    aux 
       ~read_cmi:false  ~postbuild:false
      ~name:"ml_cmi" in 
  let build_package = 
    define
      ~command:"$bsc -w -49 -color always -no-alias-deps  $in"
      ~restat:()
      "build_package"
  in

  {
    build_ast ;
    build_ast_from_re  ;
    (** platform dependent, on Win32,
        invoking cmd.exe
    *)
    copy_resources;
    (** Rules below all need restat *)
    build_bin_deps ;


    ml_cmj_js ;
    ml_cmj_js_dev ;
    ml_cmj_cmi_js ;
    ml_cmi ;
    
    ml_cmj_cmi_js_dev;
    ml_cmi_dev;
    
    build_package ;
    customs =
      Map_string.mapi custom_rules begin fun name command -> 
        define ~command ("custom_" ^ name)
      end
  }



end
module Bsb_ninja_targets : sig 
#1 "bsb_ninja_targets.mli"
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




type override = 
  | Append of string 
  | AppendList of string list 
  | AppendVar of string
  
  | Overwrite of string 
  
  | OverwriteVar of string 

  | OverwriteVars of string list
  
type shadow = { key : string ; op : override }
(** output should always be marked explicitly,
   otherwise the build system can not figure out clearly
   however, for the command we don't need pass `-o`
*)
val output_build :
  ?order_only_deps:string list ->
  ?implicit_deps:string list ->
  ?implicit_outputs: string list ->    
  ?shadows:shadow list ->  
  outputs:string list ->
  inputs:string list ->
  rule:Bsb_ninja_rule.t -> 
  out_channel -> 
  unit


val phony  :
  ?order_only_deps:string list ->
  inputs:string list -> 
  output:string -> 
  out_channel -> 
  unit

val output_kv : string ->  string -> out_channel -> unit 
val output_kvs : (string * string) array -> out_channel -> unit


end = struct
#1 "bsb_ninja_targets.ml"
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




type override = 
  | Append of string 
  | AppendList of string list
  (* Append s 
     s
  *)
  | AppendVar of string 
  (* AppendVar s 
     $s
  *)
  | Overwrite of string 

  | OverwriteVar of string 
    (*
      OverwriteVar s 
      $s
    *)
  | OverwriteVars of string list

type shadow = 
  { key : string ; op : override }

let output_build
    ?(order_only_deps=[])
    ?(implicit_deps=[])
    ?(implicit_outputs=[])
    ?(shadows=([] : shadow list))
    ~outputs
    ~inputs
    ~rule
    oc =
  let rule = Bsb_ninja_rule.get_name rule  oc in (* Trigger building if not used *)
  output_string oc "build ";
  Ext_list.iter outputs (fun s -> output_string oc Ext_string.single_space ; output_string oc s  );
  if implicit_outputs <> [] then begin 
    output_string oc " | ";
    Ext_list.iter implicit_outputs (fun s -> output_string oc Ext_string.single_space ; output_string oc s)
  end;
  output_string oc " : ";
  output_string oc rule;
  Ext_list.iter inputs (fun s ->   output_string oc Ext_string.single_space ; output_string oc s);
  if implicit_deps <> [] then 
    begin
      output_string oc " | ";
      Ext_list.iter implicit_deps (fun s -> output_string oc Ext_string.single_space; output_string oc s )
    end
  ;
  if order_only_deps <> [] then
    begin
      output_string oc " || ";                
      Ext_list.iter order_only_deps (fun s -> output_string oc Ext_string.single_space ; output_string oc s)
    end
  ;
  output_string oc "\n";
  if shadows <> [] then begin 
    Ext_list.iter shadows (fun {key=k; op= v} ->
        output_string oc "  " ;
        output_string oc k ;
        output_string oc " = ";
        match v with
        | Overwrite s -> 
          output_string oc s ; 
          output_string oc "\n"
        | OverwriteVar s ->
          output_string oc "$";
          output_string oc s ; 
          output_string oc "\n"
        | OverwriteVars s ->  
          Ext_list.iter s (fun s ->
              output_string oc "$";
              output_string oc s ; 
              output_string oc Ext_string.single_space
            );
          output_string oc "\n"
        | AppendList ls -> 
          output_string oc "$" ;
          output_string oc k;
          Ext_list.iter ls
            (fun s ->
               output_string oc Ext_string.single_space;
               output_string oc s 
            ) ;
          output_string oc "\n"
        | Append s ->
          output_string oc "$" ;
          output_string oc k;
          output_string oc Ext_string.single_space;
          output_string oc s ; output_string oc "\n"
        | AppendVar s ->   
          output_string oc "$" ;
          output_string oc k;
          output_string oc Ext_string.single_space;
          output_string oc "$";
          output_string oc s ; 
          output_string oc "\n"
      ) 
  end



let phony ?(order_only_deps=[]) ~inputs ~output oc =
  output_string oc "build ";
  output_string oc output ;
  output_string oc " : ";
  output_string oc "phony";
  output_string oc Ext_string.single_space;
  Ext_list.iter inputs  (fun s ->   output_string oc Ext_string.single_space ; output_string oc s);
  if order_only_deps <> [] then 
    begin
      output_string oc " || ";                
      Ext_list.iter order_only_deps (fun s -> output_string oc Ext_string.single_space ; output_string oc s)
    end;
  output_string oc "\n"

let output_kv key value oc  =
  output_string oc key ;
  output_string oc " = ";
  output_string oc value ;
  output_string oc "\n"

let output_kvs kvs oc =
  Ext_array.iter kvs (fun (k,v) -> output_kv k v oc) 



end
module Ext_namespace_encode : sig 
#1 "ext_namespace_encode.mli"
(* Copyright (C) 2020- Authors of BuckleScript
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

(** [make ~ns:"Ns" "a" ]
    A typical example would return "a-Ns"
    Note the namespace comes from the output of [namespace_of_package_name]
*)
val make : 
  ?ns:string -> string -> string 
 
end = struct
#1 "ext_namespace_encode.ml"
(* Copyright (C) 2020- Authors of BuckleScript
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

 let make ?ns cunit  = 
  match ns with 
  | None -> cunit
  | Some ns -> cunit ^ Literals.ns_sep ^ ns 
end
module Bsb_ninja_native : sig 
#1 "bsb_ninja_native.mli"
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




end = struct
#1 "bsb_ninja_native.ml"
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


type compile_target_t = Native | Bytecode

let (//) = Ext_path.combine

let get_install_path p = ".." // (Filename.basename !Bsb_global_backend.lib_ocaml_dir) // (Filename.basename p)




let handle_generators oc 
    (group : Bsb_file_groups.file_group) 
    custom_rules =   
  let map_to_source_dir = 
    (fun x -> Bsb_config.proj_rel (group.dir //x )) in  
  Ext_list.iter group.generators (fun {output; input; command} -> 
      (*TODO: add a loc for better error message *)
      match Map_string.find_opt custom_rules command with 
      | None -> Ext_fmt.failwithf ~loc:__LOC__ "custom rule %s used but  not defined" command
      | Some rule -> 
        Bsb_ninja_targets.output_build oc 
          ~outputs:(Ext_list.map  output  map_to_source_dir)
          ~inputs:(Ext_list.map input map_to_source_dir) 
          ~rule
    )


let make_common_shadows
    package_specs
    dirname
    dir_index
  : Bsb_ninja_targets.shadow list
  =

    { key = Bsb_ninja_global_vars.g_pkg_flg;
      op =
        Append
          (Bsb_package_specs.package_flag_of_package_specs
             package_specs dirname
          )
    } ::
    (if Bsb_dir_index.is_lib_dir dir_index  then [] else
       [
        { key =  Bsb_ninja_global_vars.g_dev_incls;
          op = OverwriteVar (Bsb_dir_index.string_of_bsb_dev_include dir_index);
        }
       ]
    )



let emit_module_build
    (rules : Bsb_ninja_rule.builtin)
    (package_specs : Bsb_package_specs.t)
    (group_dir_index : Bsb_dir_index.t)
    oc
    ~bs_suffix
    js_post_build_cmd
    ~root_project_dir
    ~compile_target
    namespace
    (module_info : Bsb_db.module_info)
  : unit =
  let has_intf_file = module_info.info = Ml_mli in 
  let is_re = module_info.is_re in 
  let filename_sans_extension = module_info.name_sans_extension in 
  let is_dev = not (Bsb_dir_index.is_lib_dir group_dir_index) in
  let input_impl = 
    Bsb_config.proj_rel 
      (filename_sans_extension ^ if is_re then  Literals.suffix_re else  Literals.suffix_ml  ) in
  let input_intf =      
    Bsb_config.proj_rel 
      (filename_sans_extension ^ if is_re then  Literals.suffix_rei else  Literals.suffix_mli) in
  let output_mlast = 
    filename_sans_extension  ^ if is_re then Literals.suffix_reast else Literals.suffix_mlast in
  let output_mliast = 
    filename_sans_extension  ^ if is_re then Literals.suffix_reiast else Literals.suffix_mliast in
  let output_mlast_simple = filename_sans_extension  ^ Literals.suffix_mlast_simple in
  let output_mliast_simple = filename_sans_extension  ^ Literals.suffix_mliast_simple in
  let output_d = filename_sans_extension ^ Literals.suffix_d in
  let output_filename_sans_extension =
      Ext_namespace_encode.make ?ns:namespace filename_sans_extension
  in
  let output_cmi =  output_filename_sans_extension ^ Literals.suffix_cmi in
  let output_cmx_or_cmo =
     match compile_target with
     | Bytecode -> output_filename_sans_extension ^ Literals.suffix_cmo
     | Native   -> output_filename_sans_extension ^ Literals.suffix_cmx
   in
  let common_shadows =
    make_common_shadows package_specs
      (Filename.dirname output_cmi)
      group_dir_index in
  let ast_rule =     
    if is_re then 
      rules.build_ast_from_re
    else
      rules.build_ast in 
  Bsb_ninja_targets.output_build oc
    ~outputs:[output_mlast_simple]
    ~inputs:[input_impl]
    ~implicit_outputs:[output_mlast]
    ~rule:ast_rule;
  if has_intf_file then begin
    Bsb_ninja_targets.output_build oc
      ~outputs:[output_mliast_simple]
      ~implicit_outputs:[output_mliast]
      (* TODO: we can get rid of absloute path if we fixed the location to be
          [lib/bs], better for testing?
      *)
      ~inputs:[input_intf]
      ~rule:ast_rule
    ;
    let rule = begin match compile_target with
    | Bytecode -> rules.build_cmi_bytecode
    | Native   -> rules.build_cmi_native
    end in
    Bsb_ninja_targets.output_build oc
      ~outputs:[output_cmi]
      ~shadows:common_shadows
      ~order_only_deps:[output_d]
      ~inputs:[output_mliast_simple]
      ~rule
    ;
  end;
  Bsb_ninja_targets.output_build
    oc
    ~outputs:[output_d]
    ~inputs:(if has_intf_file then [output_mlast;output_mliast] else [output_mlast])
    ~rule:rules.build_bin_deps
    ?shadows:(if Bsb_dir_index.is_lib_dir group_dir_index then None
              else Some [{Bsb_ninja_targets.key = Bsb_build_schemas.bsb_dir_group ;
                          op =
                            Overwrite (string_of_int (group_dir_index :> int)) }])
  ;
  let shadows = if is_dev then
    { Bsb_ninja_targets.key = Bsb_ninja_global_vars.dev_includes;
      op =
        Append "$g_dev_incls $g_dpkg_incls"
    } :: common_shadows else common_shadows in
  let rule = begin match compile_target with
   | Bytecode -> rules.build_cmo_cmi_bytecode
   | Native   -> rules.build_cmx_cmi_native
   end in
  let cm_outputs, implicit_deps =
    if has_intf_file then
      []  , [output_cmi]
    else
      [output_cmi], []
  in
  Bsb_ninja_targets.output_build oc
    ~outputs:[output_cmx_or_cmo]
    ~shadows
    ~implicit_outputs:cm_outputs
    ~inputs:[output_mlast_simple]
    ~implicit_deps
    ~order_only_deps:[output_d]
    ~rule;
  (* Copying rules around *)
  List.iter (fun input -> 
    Bsb_ninja_targets.output_build oc
      ~outputs:[get_install_path input]
      ~inputs:[input]
      ~rule:rules.copy_resources
  ) [output_cmx_or_cmo; output_cmi ]
  





let handle_file_group
    oc
    ~bs_suffix
    ~(rules : Bsb_ninja_rule.builtin)
    ~package_specs
    ~js_post_build_cmd
    ~root_project_dir
    ~compile_target
    (files_to_install : Hash_set_string.t)
    (namespace  : string option)
    (group: Bsb_file_groups.file_group )
  : unit =

  handle_generators oc group rules.customs ;
  let installable =
    match group.public with
    | Export_all -> fun _ -> true
    | Export_none -> fun _ -> false
    | Export_set set -> 
      fun module_name ->
      Set_string.mem set module_name in
  Map_string.iter group.sources   (fun  module_name module_info   ->
      if installable module_name then 
        Hash_set_string.add files_to_install 
          module_info.name_sans_extension;
      emit_module_build  rules
        package_specs
        group.dir_index
        oc
        ~bs_suffix
        ~root_project_dir
        ~compile_target
        js_post_build_cmd
        namespace module_info
    )


let link oc
  ~entries
  ~backend
  ~file_groups
  ~namespace
  ~dependency_info:(dependency_info : Bsb_dependency_info.t)
  ~static_libraries
  ~c_linker_flags
  ~rules:(rules : Bsb_ninja_rule.builtin)
  ~root_project_dir =
  let buildable_entries = List.filter (fun entry -> match (backend, entry) with
  | Bsb_config_types.Native, Bsb_config_types.NativeTarget _
  | Bsb_config_types.Bytecode, Bsb_config_types.BytecodeTarget _ -> true
  | _, _ -> false
  ) entries in
  List.iter (fun entry ->
    (* TODO: Maybe we should symlink this instead of outputting the the exe in the root directory

        Ben — August 6th 2019 (It's a special day)
     *)
    let output, rule_name, library_file_name, suffix_cmo_or_cmx, main_module_name =
      begin match entry with
      | Bsb_config_types.JsTarget _ -> 
        (* This target will be filtered out above. *)
        assert false
      | Bsb_config_types.BytecodeTarget main_module_name ->
        root_project_dir // (Ext_string.lowercase_ascii main_module_name) ^ ".exe",
        rules.linking_bytecode,
        Literals.library_file ^ Literals.suffix_cma,
        Literals.suffix_cmo,
        main_module_name
      | Bsb_config_types.NativeTarget main_module_name ->
        root_project_dir // (Ext_string.lowercase_ascii main_module_name) ^ ".exe",
        rules.linking_native,
        Literals.library_file ^ Literals.suffix_cmxa,
        Literals.suffix_cmx,
        main_module_name
      end in

    let rec get_main_module_path (groups : Bsb_file_groups.file_group list) = match groups with
    | [] -> Ext_fmt.failwithf ~loc:__LOC__ "Could not find main module %s in sources." main_module_name
    | group :: rest ->
      begin match Map_string.find_opt group.sources main_module_name with 
      | None -> get_main_module_path rest
      | Some file -> file.name_sans_extension ^ suffix_cmo_or_cmx
      end
    in
    let static_libraries = static_libraries @ dependency_info.static_libraries in

     let shadows = [{
        Bsb_ninja_targets.key = "main_module";
        op = Bsb_ninja_targets.Overwrite main_module_name
      }; {
        key = "static_libraries";
        (* TODO: might need some escaping here. *)
        op = Bsb_ninja_targets.Overwrite (String.concat Ext_string.single_space (Ext_list.flat_map c_linker_flags  (fun x -> ["-add-clib"; "-ccopt"; "-add-clib" ; x]))  ^ " " ^ (Bsb_build_util.flag_concat "-add-clib" static_libraries))
      }] in
      let external_deps_lib =
          List.map
            (fun dep -> (Bytes.unsafe_to_string (Bytes.escaped (Bytes.unsafe_of_string dep))) // library_file_name)
            dependency_info.all_external_deps
      in
      let main_module_path = get_main_module_path file_groups in
      Bsb_ninja_targets.output_build oc
        ~outputs:[output]
        ~inputs:[]
        ~implicit_deps:(main_module_path :: external_deps_lib @ (List.map (fun str -> Bytes.unsafe_to_string (Bytes.escaped (Bytes.unsafe_of_string str))) static_libraries))
        ~shadows
        ~rule:rule_name;
  ) buildable_entries


  let pack oc ~entries ~backend ~file_groups ~namespace ~rules:(rules : Bsb_ninja_rule.builtin) ?build_library () =
     let output_cma_or_cmxa, rule_name, suffix_cmo_or_cmx =
       begin match backend with
       (* These cases could benefit from a better error message. *)
       | Bsb_config_types.Bytecode ->
         [Literals.library_file ^ Literals.suffix_cma] ,
         rules.build_cma_library ,
         Literals.suffix_cmo
       | Bsb_config_types.Native   ->
         [Literals.library_file ^ Literals.suffix_cmxa; Literals.library_file ^ Literals.suffix_a],
         rules.build_cmxa_library,
         Literals.suffix_cmx
     end in
     let all_cmo_or_cmx_files, all_cmi_files =
       List.fold_left (fun acc (group : Bsb_file_groups.file_group) ->
         Map_string.fold group.Bsb_file_groups.sources acc (fun _ (v : Bsb_db.module_info) (all_cmo_or_cmx_files, all_cmi_files) ->
             let input = v.name_sans_extension in
             let name = Ext_namespace_encode.make ?ns:namespace input in
              begin match v.info with
                | Ml
                | Ml_mli ->
                  ((name ^ suffix_cmo_or_cmx)     :: all_cmo_or_cmx_files,
                  (name ^ Literals.suffix_cmi)   :: all_cmi_files)
                 | Mli -> 
                  (all_cmo_or_cmx_files,
                  (name ^ Literals.suffix_cmi)   :: all_cmi_files)
              end
         )
       )
       ([], [])
       file_groups in
    let shadows = match build_library with
    | None -> []
    | Some build_library -> {
        Bsb_ninja_targets.key = "build_library";
        op = Overwrite ("-build-library " ^ build_library)
      } :: []
    in
     (* In the case that a library is just an interface file, we don't do anything *)
     if List.length all_cmo_or_cmx_files > 0 then begin
       Bsb_ninja_targets.output_build oc
         ~outputs:output_cma_or_cmxa
         ~inputs:all_cmo_or_cmx_files
         ~implicit_deps:all_cmi_files
         ~shadows
         ~rule:rule_name;
      List.iter (fun x -> Bsb_ninja_targets.output_build oc
         ~outputs:[get_install_path x]
         ~inputs:[x]
         ~rule:rules.copy_resources) output_cma_or_cmxa
     end

let handle_file_groups
    oc
    ~package_specs
    ~bs_suffix
    ~js_post_build_cmd
    ~files_to_install
    ~rules
    ~toplevel
    ~compile_target
    ~backend
    ~dependency_info
    ~root_project_dir
    ~build_library
    ~config:(config: Bsb_config_types.t)
    (file_groups  :  Bsb_file_groups.file_groups)
    namespace   =
  Ext_list.iter file_groups
    (handle_file_group
       oc
       ~bs_suffix
       ~package_specs
       ~rules
       ~js_post_build_cmd
       ~compile_target
       ~root_project_dir
       files_to_install
       namespace
    );
  if toplevel then
    match build_library with
    | None -> 
      let c_linker_flags = dependency_info.Bsb_dependency_info.all_c_linker_flags @ config.c_linker_flags in
      link oc
        ~entries:config.entries
        ~backend
        ~file_groups
        ~namespace
        ~dependency_info
        ~rules
        ~root_project_dir
        ~static_libraries:config.static_libraries
        ~c_linker_flags
    | Some build_library ->
      pack oc ~entries:config.entries ~backend ~file_groups ~namespace ~rules ~build_library ()
  else
    pack oc ~entries:config.entries ~backend ~file_groups ~namespace ~rules ()



end
module Bsb_ninja_gen : sig 
#1 "bsb_ninja_gen.mli"
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

(** 
  generate ninja file based on [cwd] 
*)
val output_ninja_and_namespace_map :
  per_proj_dir:string ->  
  toplevel:bool -> 

  Bsb_config_types.t -> unit 

end = struct
#1 "bsb_ninja_gen.ml"
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

let (//) = Ext_path.combine

(* we need copy package.json into [_build] since it does affect build output
   it is a bad idea to copy package.json which requires to copy js files
*)




(* let dash_i = "-I" *)



let get_bsc_flags 
    (bsc_flags : string list)
  : string =       
  String.concat Ext_string.single_space bsc_flags



let emit_bsc_lib_includes 
    (bs_dependencies : Bsb_config_types.dependencies)
  (source_dirs : string list) 
  (external_includes) 
  (namespace : _ option)
  (oc : out_channel): unit = 
  (* TODO: bsc_flags contain stdlib path which is in the latter position currently *)
  let all_includes source_dirs  = 
    source_dirs @
    Ext_list.map bs_dependencies (fun x -> x.package_install_path) @ 
    (
      (* for external includes, if it is absolute path, leave it as is 
         for relative path './xx', we need '../.././x' since we are in 
         [lib/bs], [build] is different from merlin though
      *)
      Ext_list.map
        external_includes

        (fun x -> if Filename.is_relative x then Bsb_config.rev_lib_bs_prefix  x else x) 
    )
  in 
  Bsb_ninja_targets.output_kv
    Bsb_build_schemas.g_lib_incls 
    (Bsb_build_util.include_dirs 
       (all_includes 
          (if namespace = None then source_dirs 
           else Filename.current_dir_name :: source_dirs
           (*working dir is [lib/bs] we include this path to have namespace mapping*)
          )))  oc 


let output_static_resources 
    (static_resources : string list) 
    copy_rule 
    oc
  = 
  Ext_list.iter static_resources (fun output -> 
      Bsb_ninja_targets.output_build
        oc
        ~outputs:[output]
        ~inputs:[Bsb_config.proj_rel output]
        ~rule:copy_rule);
  if static_resources <> [] then
    Bsb_ninja_targets.phony
      oc
      ~order_only_deps:static_resources 
      ~inputs:[]
      ~output:Literals.build_ninja         


let output_ninja_and_namespace_map
    ~per_proj_dir 
    ~toplevel           

    ({
      bs_suffix;
      package_name;
      external_includes;
      bsc_flags ; 
      pp_file;
      ppx_files ;

      bs_dependencies;
      bs_dev_dependencies;
      refmt;
      js_post_build_cmd;
      package_specs;
      file_groups = { files = bs_file_groups} ;
      files_to_install;
      built_in_dependency;
      reason_react_jsx;
      generators ;
      namespace ; 
      warning;
      gentype_config; 
      number_of_dev_groups;
      entries;
      otherlibs;
    } as _config : Bsb_config_types.t) : unit 
  =
  let lib_artifacts_dir = !Bsb_global_backend.lib_artifacts_dir in
  let cwd_lib_bs = per_proj_dir // lib_artifacts_dir in 
  
  


  let ppx_flags = Bsb_build_util.ppx_flags ppx_files in
  let oc = open_out_bin (cwd_lib_bs // Literals.build_ninja) in          
  let bsc_flags =
    (Printf.sprintf
      "-bs-D JS=false -bs-D %s=true" (String.uppercase_ascii !Bsb_global_backend.backend_string)) ::
      bsc_flags in
  let g_pkg_flg , g_ns_flg, ns = 
    match namespace with
    | None -> 
      Ext_string.inter2 "-bs-package-name" package_name, Ext_string.empty, Ext_string.empty
    | Some s -> 
      Ext_string.inter4 
        "-bs-package-name" package_name 
        "-bs-ns" s
      ,
      Ext_string.inter2 "-bs-ns" s,
      s in

  let all_sources_mlast =
      List.fold_left (fun acc (group : Bsb_file_groups.file_group) ->
        Map_string.fold group.sources acc (fun _ (v : Bsb_db.module_info) all_sources_mlast ->
          let input = v.name_sans_extension in
          begin match v.info with
            | Ml
            | Ml_mli -> (input ^ Literals.suffix_mlast_simple) :: all_sources_mlast
            | Mli -> all_sources_mlast
          end
        )
      ) [] bs_file_groups
    in
  let () = 
    Ext_option.iter pp_file (fun flag ->
        Bsb_ninja_targets.output_kv Bsb_ninja_global_vars.pp_flags
          (Bsb_build_util.pp_flag flag) oc 
      );
    Ext_option.iter gentype_config (fun x -> 
        (* resolved earlier *)
        Bsb_ninja_targets.output_kv Bsb_ninja_global_vars.gentypeconfig
          ("-bs-gentype " ^ x.path) oc
      );    
    Bsb_ninja_targets.output_kvs
      [|
        Bsb_ninja_global_vars.g_pkg_flg, g_pkg_flg ; 
        Bsb_ninja_global_vars.src_root_dir, per_proj_dir (* TODO: need check its integrity -- allow relocate or not? *);
        (* The path to [bsc.exe] independent of config  *)
        Bsb_ninja_global_vars.bsc, (Ext_filename.maybe_quote (Bsb_global_paths.vendor_bsc bsc_dir));
        (* The path to [bsb_heler.exe] *)
        Bsb_ninja_global_vars.bsdep, (Ext_filename.maybe_quote Bsb_global_paths_native.vendor_bsdep) ;
        Bsb_ninja_global_vars.warnings, Bsb_warning.to_bsb_string ~toplevel warning ;
        Bsb_ninja_global_vars.bsc_flags, (get_bsc_flags bsc_flags) ;
        Bsb_ninja_global_vars.ppx_flags, ppx_flags;


        Bsb_ninja_global_vars.g_dpkg_incls, 
        (Bsb_build_util.include_dirs_by
           bs_dev_dependencies
           (fun x -> x.package_install_path));  
        Bsb_ninja_global_vars.g_ns , g_ns_flg ; 
        Bsb_build_schemas.bsb_dir_group, "0"  (*TODO: avoid name conflict in the future *)
      |] oc 
  in        
  let  bs_groups, bsc_lib_dirs, static_resources =    
    if number_of_dev_groups = 0 then
      let bs_group, source_dirs,static_resources  =
        Ext_list.fold_left bs_file_groups (Map_string.empty,[],[]) 
          (fun (acc, dirs,acc_resources) ({sources ; dir; resources } as x)   
            ->
            Bsb_db_util.merge  acc  sources ,  
            (if Bsb_file_groups.is_empty x then dirs else  dir::dirs) , 
            ( if resources = [] then acc_resources
              else Ext_list.map_append resources acc_resources (fun x -> dir // x ) )
          )  in
      Bsb_db_util.sanity_check bs_group;
      [|bs_group|], source_dirs, static_resources
    else
      let bs_groups = Array.init  (number_of_dev_groups + 1 ) (fun _ -> Map_string.empty) in
      let source_dirs = Array.init (number_of_dev_groups + 1 ) (fun _ -> []) in
      let static_resources =
        Ext_list.fold_left bs_file_groups [] (fun (acc_resources : string list) {sources; dir; resources; dir_index} 
           ->
            let dir_index = (dir_index :> int) in 
            bs_groups.(dir_index) <- Bsb_db_util.merge bs_groups.(dir_index) sources ;
            source_dirs.(dir_index) <- dir :: source_dirs.(dir_index);
            Ext_list.map_append resources  acc_resources (fun x -> dir//x) 
          ) in
      let lib = bs_groups.((Bsb_dir_index.lib_dir_index :> int)) in               
      Bsb_db_util.sanity_check lib;
      for i = 1 to number_of_dev_groups  do
        let c = bs_groups.(i) in
        Bsb_db_util.sanity_check c;
        Map_string.iter c 
          (fun k a -> 
            if Map_string.mem lib k  then 
              Bsb_db_util.conflict_module_info k a (Map_string.find_exn lib k)            
            ) ;
        Bsb_ninja_targets.output_kv 
          (Bsb_dir_index.(string_of_bsb_dev_include (of_int i)))
          (Bsb_build_util.include_dirs source_dirs.(i)) oc
      done  ;
      bs_groups,source_dirs.((Bsb_dir_index.lib_dir_index:>int)), static_resources
  in

  let digest = Bsb_db_encode.write_build_cache ~dir:cwd_lib_bs bs_groups in
  let rules : Bsb_ninja_rule.builtin = 
      Bsb_ninja_rule.make_custom_rules 
      ~refmt
      ~has_gentype:(gentype_config <> None)
      ~has_postbuild:(js_post_build_cmd <> None)
      ~has_ppx:(ppx_files <> [])
      ~has_pp:(pp_file <> None)
      ~has_builtin:(built_in_dependency <> None)
      ~reason_react_jsx
      ~bs_suffix
      ~digest
      generators in 
  
  emit_bsc_lib_includes bs_dependencies bsc_lib_dirs external_includes namespace oc;
  output_static_resources static_resources rules.copy_resources oc ;
  
  (* Add custom rules for compiling C and stuff *)
  Bsb_ninja_rule.print_rule oc ~description:"Building C code" ~command:"gcc -c $in -I $g_stdlib_incl_ocaml $extra_args -o $out" "cc";
  Bsb_ninja_rule.print_rule oc ~description:"Packing C code" ~command:"ar rcs $out $extra_args $in" "ar";

    let (compile_target, rule) =
      if backend = Bsb_config_types.Bytecode then
        Bsb_ninja_native.Bytecode, rules.build_package_build_cmi_bytecode
      else
        Bsb_ninja_native.Native, rules.build_package_build_cmi_native
    in
    Bsb_ninja_native.handle_file_groups oc
      ~bs_suffix
      ~rules
      ~js_post_build_cmd
      ~package_specs
      ~files_to_install
      ~toplevel
      ~compile_target
      ~backend
      ~dependency_info
      ~root_project_dir
      ~build_library
      ~config:_config
      bs_file_groups
      namespace;
    Ext_option.iter  namespace (fun ns ->
      let namespace_dir =
        per_proj_dir // lib_artifacts_dir  in
      Bsb_namespace_map_gen.output
        ~dir:namespace_dir ns
        bs_file_groups;
      Bsb_ninja_targets.output_build oc
        ~outputs:[ns ^ Literals.suffix_mlast_simple]
        ~inputs:[ns ^ Literals.suffix_mlmap]
        ~rule:rules.build_package_gen_mlast_simple;
      Bsb_ninja_targets.output_build oc
        ~outputs:[ns ^ Literals.suffix_cmi]
        ~inputs:[ns ^ Literals.suffix_mlast_simple]
        (* When compiling the namespace file, we might not have the cmis ready yet, so we hide that warning. *)
        ~shadows:[{ Bsb_ninja_targets.key = Bsb_ninja_global_vars.warnings; op = Append ("-w -49") }]
        ~rule;
    );
  if root_project_dir = per_proj_dir then begin
    let entries_to_build = begin match build_library with
    | None -> 
      Ext_list.filter_map entries (function
        | Bsb_config_types.NativeTarget   main_module_name when backend = Bsb_config_types.Native ->
          Some (root_project_dir // (Ext_string.lowercase_ascii main_module_name) ^ ".exe")
        | Bsb_config_types.BytecodeTarget main_module_name when backend = Bsb_config_types.Bytecode ->
          Some (root_project_dir // (Ext_string.lowercase_ascii main_module_name) ^ ".exe")
        | _ -> None
      )
    | Some _index ->  []
    end in
    if List.length entries_to_build > 0 then begin
      output_string oc "default ";
      output_string oc (String.concat Ext_string.single_space entries_to_build);
      output_string oc "\n";
    end
  end;
  let user_defined_build_ninja = per_proj_dir // "build.ninja" in
  if Sys.file_exists user_defined_build_ninja then begin
    output_string oc "subninja ";
    output_string oc user_defined_build_ninja;
    output_string oc "\n";
  end;
  close_out oc

end
module Bsb_main : sig 
#1 "bsb_main.mli"
(* *)

end = struct
#1 "bsb_main.ml"
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

end

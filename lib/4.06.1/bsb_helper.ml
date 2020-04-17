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
module Bsb_helper_arg : sig 
#1 "bsb_helper_arg.mli"



type spec =
  | Unit of (unit -> unit)       
  | Set of bool ref            
  | String of (string -> unit) 
  | Set_string of string ref   
  | Int of (int -> unit)       
  | Set_int of int ref         

type key = string
type doc = string
type usage_msg = string
type anon_fun = (string -> unit)

val parse_exn :
  (key * spec * doc) list -> anon_fun -> usage_msg -> unit




end = struct
#1 "bsb_helper_arg.ml"

type key = string
type doc = string
type usage_msg = string
type anon_fun = (string -> unit)

type spec =
  | Unit of (unit -> unit)     
  | Set of bool ref            
  | String of (string -> unit) 
  | Set_string of string ref   
  | Int of (int -> unit)       
  | Set_int of int ref         

exception Bad of string
(* exception Help of string *)

type error =
  | Unknown of string
  | Wrong of string * string * string  (* option, actual, expected *)
  | Missing of string
  | Message of string

exception Stop of error


type t = (string * spec * string) list 

let rec assoc3 (x : string) (l : t) =
  match l with
  | [] -> None
  | (y1, y2, _y3) :: _t when y1 = x -> Some y2
  | _ :: t -> assoc3 x t
;;



let usage_b (buf : Ext_buffer.t) speclist errmsg =
  let print_spec buf (key, _spec, doc) =
    if  doc <> "" then begin 
      Ext_buffer.add_string buf "  ";
      Ext_buffer.add_string_char buf key ' ';  
      Ext_buffer.add_string_char buf doc '\n'  
    end 
  in 

  Ext_buffer.add_string_char buf errmsg '\n';
  Ext_list.iter speclist (print_spec buf) 
;;


  
let stop_raise progname (error : error) speclist errmsg  =
  let b = Ext_buffer.create 200 in  
  begin match error with
    | Unknown ("-help" | "--help" | "-h") -> 
      usage_b b speclist errmsg;
      output_string stdout (Ext_buffer.contents b);
      exit 0
      
    | Unknown s ->
      Ext_buffer.add_string_char b progname ':';
      Ext_buffer.add_string b  " unknown option '";
      Ext_buffer.add_string b s ;
      Ext_buffer.add_string b "'.\n"
    | Missing s ->
      Ext_buffer.add_string_char b progname ':';
      Ext_buffer.add_string b " option '";
      Ext_buffer.add_string b s;
      Ext_buffer.add_string b "' needs an argument.\n"      
    | Wrong (opt, arg, expected) ->
      Ext_buffer.add_string_char b progname ':';
      Ext_buffer.add_string b " wrong argument '";
      Ext_buffer.add_string b arg; 
      Ext_buffer.add_string b "'; option '";
      Ext_buffer.add_string b opt;
      Ext_buffer.add_string b "' expects ";
      Ext_buffer.add_string b expected;
      Ext_buffer.add_string b ".\n"      
    | Message s ->
      Ext_buffer.add_string_char b progname ':';
      Ext_buffer.add_char_string b ' ' s;
      Ext_buffer.add_string b ".\n"
  end;
  usage_b b speclist errmsg;
  raise (Bad (Ext_buffer.contents b))


let parse_exn  (speclist : t) anonfun errmsg =    
  let argv = Sys.argv in 
  let stop_raise error = stop_raise argv.(0) error speclist errmsg in 
  let l = Array.length argv in
  let current = ref 1 in (* 0 is progname*)
  while !current < l do
    let s = argv.(!current) in
    if s <> "" && s.[0] = '-' then begin
      let action =
        match assoc3 s speclist with 
        | Some action -> action 
        | None -> stop_raise (Unknown s)
      in
      begin try
        let treat_action = function
        | Unit f -> f ();
        | Set r -> r := true;
        | String f when !current + 1 < l ->
            f argv.(!current + 1);
            incr current;
        | Set_string r when !current + 1 < l ->
            r := argv.(!current + 1);
            incr current;
        | Int f when !current + 1 < l ->
            let arg = argv.(!current + 1) in
            begin match int_of_string arg with 
              | i -> f i 
              | exception _ 
                ->
                raise (Stop (Wrong (s, arg, "an integer")))
            end;
            incr current;
        | Set_int r when !current + 1 < l ->
            let arg = argv.(!current + 1) in
            r := (try int_of_string arg
                  with _ ->
                    raise (Stop (Wrong (s, arg, "an integer")))
                 );
            incr current;
        | _ -> raise (Stop (Missing s))
        in
        treat_action action
      with Bad m -> stop_raise (Message m);
         | Stop e -> stop_raise e;
      end;
      incr current;
    end else begin
      (try anonfun s with Bad m -> stop_raise (Message m));
      incr current;
    end;
  done;
;;



(* let parse l f msg =
  try
    parse_exn l f msg;
  with
  | Bad msg -> 
    output_string stderr msg ; exit 2;
  | Help msg -> 
    output_string stdout  msg; exit 0;
;;
 *)

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
module Ext_string_array : sig 
#1 "ext_string_array.mli"
(* Copyright (C) 2020 - Present Authors of BuckleScript
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

val cmp : string -> string -> int 

val find_sorted : 
  string array -> string -> int option

val find_sorted_assoc : 
  (string * 'a ) array -> 
  string -> 
  'a option
end = struct
#1 "ext_string_array.ml"
(* Copyright (C) 2020 - Present Authors of BuckleScript
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


(* Invariant: the same as encoding Map_string.compare_key  *)  
let cmp  =  Ext_string.compare
    

let rec binarySearchAux (arr : string array) (lo : int) (hi : int) (key : string)  : _ option = 
  let mid = (lo + hi)/2 in 
  let midVal = Array.unsafe_get arr mid in 
  let c = cmp key midVal in 
  if c = 0 then Some (mid)
  else if c < 0 then  (*  a[lo] =< key < a[mid] <= a[hi] *)
    if hi = mid then  
      let loVal = (Array.unsafe_get arr lo) in 
      if  loVal = key then Some lo
      else None
    else binarySearchAux arr lo mid key 
  else  (*  a[lo] =< a[mid] < key <= a[hi] *)
  if lo = mid then 
    let hiVal = (Array.unsafe_get arr hi) in 
    if  hiVal = key then Some hi
    else None
  else binarySearchAux arr mid hi key 

let find_sorted sorted key  : int option =  
  let len = Array.length sorted in 
  if len = 0 then None
  else 
    let lo = Array.unsafe_get sorted 0 in 
    let c = cmp key lo in 
    if c < 0 then None
    else
      let hi = Array.unsafe_get sorted (len - 1) in 
      let c2 = cmp key hi in 
      if c2 > 0 then None
      else binarySearchAux sorted 0 (len - 1) key

let rec binarySearchAssoc  (arr : (string * _) array) (lo : int) (hi : int) (key : string)  : _ option = 
  let mid = (lo + hi)/2 in 
  let midVal = Array.unsafe_get arr mid in 
  let c = cmp key (fst midVal) in 
  if c = 0 then Some (snd midVal)
  else if c < 0 then  (*  a[lo] =< key < a[mid] <= a[hi] *)
    if hi = mid then  
      let loVal = (Array.unsafe_get arr lo) in 
      if  fst loVal = key then Some (snd loVal)
      else None
    else binarySearchAssoc arr lo mid key 
  else  (*  a[lo] =< a[mid] < key <= a[hi] *)
  if lo = mid then 
    let hiVal = (Array.unsafe_get arr hi) in 
    if  fst hiVal = key then Some (snd hiVal)
    else None
  else binarySearchAssoc arr mid hi key 

let find_sorted_assoc (type a) (sorted : (string * a) array) (key : string)  : a option =  
  let len = Array.length sorted in 
  if len = 0 then None
  else 
    let lo = Array.unsafe_get sorted 0 in 
    let c = cmp key (fst lo) in 
    if c < 0 then None
    else
      let hi = Array.unsafe_get sorted (len - 1) in 
      let c2 = cmp key (fst hi) in 
      if c2 > 0 then None
      else binarySearchAssoc sorted 0 (len - 1) key

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
module Bsb_db_decode : sig 
#1 "bsb_db_decode.mli"
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


 
  
type t

type group = {
   modules : string array ; 
   dir_length : int ;
   dir_info_offset : int ; 
   module_info_offset : int ;
 }

(* exposed only for testing *)
val decode_internal : 
  string -> 
  int ref ->
  group array 



val read_build_cache : 
  dir:string -> t



type module_info = {
  case : bool (* Bsb_db.case*);
  dir_name : string
} 

val find_opt :
  t -> (* contains global info *)
  int -> (* more likely to be zero *)
  string -> (* module name *)
  module_info option 
end = struct
#1 "bsb_db_decode.ml"
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


 type group = {
   modules : string array ; 
   dir_length : int;
   dir_info_offset : int ; 
   module_info_offset : int;
 }

type t = group array * string (* string is whole content*)


type cursor = int ref 


(*TODO: special case when module_count is zero *)
let rec decode_internal (x : string) (offset : cursor) =   
  let len = Ext_pervasives.parse_nat_of_string x offset in  
  incr offset;
  let first = decode_single x offset in 
  if len = 1 then [|first|]
  else 
    let result = Array.make len first in 
    for i = 1 to len - 1 do 
      Array.unsafe_set result i (decode_single x offset)
    done ;
    result
  
and decode_single (x : string) (offset : cursor) : group = 
  let module_number = Ext_pervasives.parse_nat_of_string x offset in 
  incr offset;
  let modules = decode_modules x offset module_number in 
  let dir_info_offset = !offset in 
  let module_info_offset = 
    String.index_from x dir_info_offset '\n'  + 1 in
  let dir_length = Char.code x.[module_info_offset] - 48 (* Char.code '0'*) in
  offset := 
    module_info_offset +
    1 +
    dir_length * module_number +
    1 
    ;
  { modules ; dir_info_offset; module_info_offset ; dir_length}
and decode_modules (x : string) (offset : cursor) module_number : string array =   
  let result = Array.make module_number "" in 
  let last = ref !offset in 
  let cur = ref !offset in 
  let tasks = ref 0 in 
  while !tasks <> module_number do 
    if String.unsafe_get x !cur = '\n' then 
      begin 
        let offs = !last in 
        let len = (!cur - !last) in         
        Array.unsafe_set result !tasks
        (Ext_string.unsafe_sub x offs len);
        incr tasks;
        last := !cur + 1;
      end;
    incr cur
  done ;
  offset := !cur;
  result
  

(* TODO: shall we check the consistency of digest *)
let read_build_cache ~dir  : t =   
  let all_content = 
    Ext_io.load_file (Filename.concat dir bsbuild_cache) in   
  decode_internal all_content (ref (Ext_digest.length + 1)), all_content



type module_info =  {
  case : bool ; (* which is Bsb_db.case*)
  dir_name : string
} 


let find_opt 
  ((sorteds,whole) : t )  i (key : string) 
    : module_info option = 
  let group = sorteds.(i) in 
  let i = Ext_string_array.find_sorted  group.modules key in 
  match i with 
  | None -> None 
  | Some count ->     
    let encode_len = group.dir_length in 
    let index = 
      Ext_string.get_1_2_3_4 whole 
      ~off:(group.module_info_offset + 1 + count * encode_len)
      encode_len
    in 
    let case = not (index mod 2 = 0) in 
    let ith = index lsr 1 in 
    let dir_name_start = 
      if ith = 0 then group.dir_info_offset 
      else 
        Ext_string.index_count 
          whole group.dir_info_offset '\t'
          ith + 1
    in 
    let dir_name_finish = 
      String.index_from
        whole dir_name_start '\t' 
    in    
    Some {case ; dir_name = String.sub whole dir_name_start (dir_name_finish - dir_name_start)}
  
        
      
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
module Bsb_helper_depfile_gen : sig 
#1 "bsb_helper_depfile_gen.mli"
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

type kind = Js | Bytecode | Native

(** [deps_of_channel ic]
    given an input_channel dumps all modules it depend on, only used for debugging 
*)
val deps_of_channel : in_channel -> string list


val emit_d: 
  kind -> 
  Bsb_dir_index.t ->  
  string  option ->
  string ->
  string -> (* empty string means no mliast *)
  unit

end = struct
#1 "bsb_helper_depfile_gen.ml"
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



let dep_lit = " : "
let write_buf name buf  =     
  let oc = open_out_bin name in 
  Ext_buffer.output_buffer oc buf ;
  close_out oc 

(* should be good for small file *)
let load_file name (buf : Ext_buffer.t): unit  = 
  let len = Ext_buffer.length buf in 
  let ic = open_in_bin name in 
  let n = in_channel_length ic in   
  if n <> len then begin close_in ic ; write_buf name buf  end 
  else
    let holder = really_input_string ic  n in 
    close_in ic ; 
    if Ext_buffer.not_equal buf holder then 
      write_buf name buf 
;;
let write_file name  (buf : Ext_buffer.t) = 
  if Sys.file_exists name then 
    load_file name buf 
  else 
    write_buf name buf 
    
(* return an non-decoded string *)
let extract_dep_raw_string (fn : string) : string =   
  let ic = open_in_bin fn in 
  let size = input_binary_int ic in 
  let s = really_input_string ic size in
  close_in ic;
  s

(* Make sure it is the same as {!Binary_ast.magic_sep_char}*)
let magic_sep_char = '\n'

let deps_of_channel (ic : in_channel) : string list = 
  let size = input_binary_int ic in 
  let s = really_input_string ic size in   
  let rec aux (s : string) acc (offset : int) size : string list = 
    if offset < size then
      let next_tab = String.index_from s offset magic_sep_char in        
      aux s 
        (String.sub s offset (next_tab - offset)::acc) (next_tab + 1) 
        size
    else acc    
  in 
  aux s [] 1 size 

  



(** Please refer to {!Binary_ast} for encoding format, we move it here 
    mostly for cutting the dependency so that [bsb_helper.exe] does
    not depend on compler-libs
*)
(* let read_deps (fn : string) : string list = 
  let ic = open_in_bin fn in 
  let v = deps_of_channel ic in 
  close_in ic;
  v
 *)

type kind = Js | Bytecode | Native

let output_file (buf : Ext_buffer.t) source namespace = 
  Ext_buffer.add_string buf 
    (Ext_namespace_encode.make ?ns:namespace source)

(** for bucklescript artifacts 
    [lhs_suffix] is [.cmj]
    [rhs_suffix] 
    is [.cmj] if it has [ml] (in this case does not care about mli or not)
    is [.cmi] if it has [mli]
*)
let oc_cmi buf namespace source = 
  Ext_buffer.add_char buf ' ';  
  output_file buf source namespace;
  Ext_buffer.add_string buf Literals.suffix_cmi 


(* For cases with self cycle
    e.g, in b.ml
    {[
      include B
    ]}
    When ns is not turned on, it makes sense that b may come from third party package.
    Hoever, this case is wont supported. 
    It complicates when it has interface file or not.
    - if it has interface file, the current interface will have priority, failed to build?
    - if it does not have interface file, the build will not open this module at all(-bs-read-cmi)

    When ns is turned on, `B` is interprted as `Ns-B` which is a cyclic dependency,
    it can be errored out earlier
*)
let find_module db dependent_module is_not_lib_dir (index : Bsb_dir_index.t) = 
  let opt = Bsb_db_decode.find_opt db 0 dependent_module in 
  match opt with 
  | Some _ -> opt
  | None -> 
    if is_not_lib_dir then 
      Bsb_db_decode.find_opt db (index :> int) dependent_module 
    else None 
let oc_impl 
    (mlast : string)
    (index : Bsb_dir_index.t)
    (db : Bsb_db_decode.t)
    (namespace : string option)
    (buf : Ext_buffer.t)
    (lhs_suffix : string)
    (rhs_suffix : string)
  = 
  (* TODO: move namespace upper, it is better to resolve ealier *)  
  let has_deps = ref false in 
  let cur_module_name = Ext_filename.module_name mlast  in
  let at_most_once : unit lazy_t  = lazy (
    has_deps := true ;
    output_file buf (Ext_filename.chop_extension_maybe mlast) namespace ; 
    Ext_buffer.add_string buf lhs_suffix; 
    Ext_buffer.add_string buf dep_lit ) in  
  (match namespace with None -> () | Some ns -> 
      Lazy.force at_most_once;
      Ext_buffer.add_string buf ns;
      Ext_buffer.add_string buf Literals.suffix_cmi;
  ) ; (* TODO: moved into static files*)
  let is_not_lib_dir = not (Bsb_dir_index.is_lib_dir index) in 
  let s = extract_dep_raw_string mlast in 
  let offset = ref 1 in 
  let size = String.length s in 
  while !offset < size do 
    let next_tab = String.index_from s !offset magic_sep_char in
    let dependent_module = String.sub s !offset (next_tab - !offset) in 
    (if dependent_module = cur_module_name then 
      begin
        prerr_endline ("FAILED: " ^ cur_module_name ^ " has a self cycle");
        exit 2
      end
    );
    (match  
      find_module db dependent_module is_not_lib_dir index  
    with      
    | None -> ()
    | Some ({dir_name; case }) -> 
      begin 
        Lazy.force at_most_once;
        let source = 
          Filename.concat dir_name
          (if case then 
            dependent_module
          else 
            Ext_string.uncapitalize_ascii dependent_module) in 
        Ext_buffer.add_char buf ' ';  
        output_file buf source namespace;
        Ext_buffer.add_string buf rhs_suffix;
        
        (* #3260 cmj changes does not imply cmi change anymore *)
        oc_cmi buf namespace source

      end);     
    offset := next_tab + 1  
  done ;
  if !has_deps then  
    Ext_buffer.add_char buf '\n'



(** Note since dependent file is [mli], it only depends on 
    [.cmi] file
*)
let oc_intf
    mliast    
    (index : Bsb_dir_index.t)
    (db : Bsb_db_decode.t)
    (namespace : string option)
    (buf : Ext_buffer.t) : unit =     
  
  let has_deps = ref false in  
  let at_most_once : unit lazy_t = lazy (  
    has_deps := true;
    output_file buf (Ext_filename.chop_all_extensions_maybe mliast) namespace ;   
    Ext_buffer.add_string buf Literals.suffix_cmi ; 
    Ext_buffer.add_string buf dep_lit) in 
  (match namespace with None -> () | Some  ns -> 
      Lazy.force at_most_once;  
      Ext_buffer.add_string buf ns;
      Ext_buffer.add_string buf Literals.suffix_cmi;
  ) ; 
  let cur_module_name = Ext_filename.module_name mliast in
  let is_not_lib_dir = not (Bsb_dir_index.is_lib_dir index)  in  
  let s = extract_dep_raw_string mliast in 
  let offset = ref 1 in 
  let size = String.length s in 
  while !offset < size do 
    let next_tab = String.index_from s !offset magic_sep_char in
    let dependent_module = String.sub s !offset (next_tab - !offset) in 
    (if dependent_module = cur_module_name then 
       begin
         prerr_endline ("FAILED: " ^ cur_module_name ^ " has a self cycle");
         exit 2
       end
    );
    (match  find_module db dependent_module is_not_lib_dir index 
     with     
     | None -> ()
     | Some {dir_name; case} ->       
       Lazy.force at_most_once; 
       oc_cmi buf namespace 
         (Filename.concat dir_name 
            (if case then dependent_module else
               Ext_string.uncapitalize_ascii dependent_module
            ))
    );
    offset := next_tab + 1   
  done;  
  if !has_deps then
    Ext_buffer.add_char buf '\n'


let emit_d 
  compilation_kind
  (index : Bsb_dir_index.t) 
  (namespace : string option) (mlast : string) (mliast : string) = 
  let data  =
    Bsb_db_decode.read_build_cache 
      ~dir:Filename.current_dir_name in   
  let buf = Ext_buffer.create 2048 in 
  let filename = 
      Ext_filename.new_extension mlast Literals.suffix_d in   
  let lhs_suffix, rhs_suffix =
    match compilation_kind with
    | Js       -> Literals.suffix_cmj, Literals.suffix_cmj
    | Bytecode -> Literals.suffix_cmo, Literals.suffix_cmo
    | Native   -> Literals.suffix_cmx, Literals.suffix_cmx 
  in   
  oc_impl 
    mlast
    index 
    data
    namespace
    buf 
    lhs_suffix 
    rhs_suffix ;      
  if mliast <> "" then begin
    oc_intf 
      mliast
      index 
      data 
      namespace 
      buf        
  end;          
  write_file filename buf 

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
let version = "0.0.5"
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
module Bsb_helper_dep_graph : sig 
#1 "bsb_helper_dep_graph.mli"
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

(* Will do a topological sort of the tree given [Set_string.t Map_string.t] while ignoring anything not in the given [domain]. Returns a queue of modules topologically sorted. *)
val sort_files_by_dependencies : domain:Set_string.t -> Set_string.t Map_string.t -> string Queue.t

(* Returns a topologically sorted Queue of module names found from the given main module. *)
val simple_collect_from_main :
           ?alias_map:string Hash_string.t ->
           Set_string.t Map_string.t ->
           string -> string Queue.t

(* Returns a list of extra modules which are part of the "otherlibs" stdlib to link in. *)
val get_otherlibs_dependencies : Set_string.t Map_string.t -> string -> string list

end = struct
#1 "bsb_helper_dep_graph.ml"
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

(* TODO: This function is a duplicate of `Ast_extract.sort_files_by_dependencies`
         without the dependency on `bs_exception`. 
         We should combine them at some point to avoid the duplicated logic. *)
let sort_files_by_dependencies ~domain dependency_graph =
  let next current =
    Map_string.find_exn  dependency_graph current in
  let worklist = ref domain in
  let result = Queue.create () in
  let rec visit visiting path current =
    if Set_string.mem visiting current  then
      Bsb_log.error "@{<error>Cyclic depends@} : @[%a@]"
        (Format.pp_print_list ~pp_sep:Format.pp_print_space
           Format.pp_print_string)
        (current::path)
    else if Set_string.mem !worklist current then
      begin        
        Set_string.iter (next current)
          (fun node ->
             if  Map_string.mem dependency_graph node then
               visit (Set_string.add visiting current) (current::path) node)
        ;
        worklist := Set_string.remove !worklist  current;
        Queue.push current result ;
      end in
  while not (Set_string.is_empty !worklist) do
    visit Set_string.empty []  (Set_string.choose !worklist)
  done;
  result
;;

(* TODO: The core of the logic in this function is the exact same as 
         `Ast_extract.collect_from_main` but we removed the dep on bs_exception
         and made it return a Queue. It also doesn't create the ast_table itself.
         We should probably refactor the two to work together at some point. *)
let simple_collect_from_main ?alias_map ast_table main_module =
  let visited = Hash_string.create 31 in
  let result = Queue.create () in
  let next module_name : Set_string.t =
    let module_set =
      match Map_string.find_exn ast_table module_name with
      | exception _ -> Set_string.empty
      | x -> x
    in
    match alias_map with
    | None -> module_set
    | Some map ->
      Set_string.fold module_set Set_string.empty (fun x acc -> Set_string.add acc (Hash_string.find_default map x x) ) 
  in
  let rec visit visiting path current =
    if Set_string.mem visiting current then
      Bsb_log.error "@{<error>Cyclic depends@} : @[%a@]"
        (Format.pp_print_list ~pp_sep:Format.pp_print_space
           Format.pp_print_string)
        (current::path)
    else
    if not (Hash_string.mem visited current)
    && Map_string.mem ast_table current then
      begin
        Set_string.iter (next current)
          (visit
             (Set_string.add visiting current)
             (current::path))
          ;
        Queue.push current result;
        Hash_string.add visited current ();
      end in
  visit (Set_string.empty) [] main_module ;
  result

let get_otherlibs_dependencies dependency_graph file_extension =
  let addIfPresentInSet v moduleName fileName acc = 
    if Set_string.mem v moduleName then
      Set_string.add acc (fileName ^ file_extension)
    else
      acc
  in
  let set_of_otherlib_deps = Map_string.fold dependency_graph Set_string.empty (fun _k v acc ->
    let addIfPresent = addIfPresentInSet v in
    acc
      |> addIfPresent "Unix"     "unix"
      |> addIfPresent "Bigarray" "bigarray"
      |> addIfPresent "Str"      "str"
      |> addIfPresent "Num"      "nums"
      |> addIfPresent "Threads"  "threads"
      |> addIfPresent "Dynlink"  "dynlink"
      |> addIfPresent "Graphics" "graphics"
  )  in
  Set_string.fold set_of_otherlib_deps [] (fun v acc -> v :: acc) 

end
module Bsb_helper_extract : sig 
#1 "bsb_helper_extract.mli"
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

(* This reads the header part of the mlast file, which simply encodes a set that indicates all of the deps of the current library. *)
val read_dependency_graph_from_mlast_file : string -> Set_string.t

end = struct
#1 "bsb_helper_extract.ml"
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

let read_dependency_graph_from_mlast_file fn  =
  let ic = open_in_bin fn in
  try
    let dep_size = input_binary_int ic in
    let dep_data = really_input_string ic dep_size in
    let splitted_data = Ext_string.split dep_data '\n' in
    let set = Set_string.of_list splitted_data in
    close_in ic;
    set
  with exn ->
    close_in ic;
    raise exn

end
module Bsb_helper_linker : sig 
#1 "bsb_helper_linker.mli"
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

type link_t = LinkBytecode of string | LinkNative of string

val link : link_t ->
  main_module:string ->
  batch_files:string list ->
  includes:string list ->
  ocaml_dependencies:string list ->
  namespace:string option ->
  warnings:string ->
  warn_error:string ->
  verbose:bool ->
  cwd: string ->
  clibs: string list ->
  unit


end = struct
#1 "bsb_helper_linker.ml"
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

type link_t = LinkBytecode of string | LinkNative of string

let ( // ) = Ext_path.combine

(* The linker is called with object files (.cmo / .cmx) which will be namespaced and we're using
   those names to read-in the mlast files which are not namespaced. So we strip the namespace
   before reading them in. *)
let module_of_filename filename = 
  let str = Ext_filename.chop_extension_maybe filename in
  match (String.rindex str '-') with 
  | exception Not_found -> str
  | len -> String.sub str 0 len

let link link_byte_or_native ~main_module ~batch_files ~includes ~ocaml_dependencies ~namespace ~warnings ~warn_error ~verbose ~cwd ~clibs =
  let suffix_object_files, suffix_library_files, compiler, output_file, add_custom = begin match link_byte_or_native with
  | LinkBytecode output_file -> Literals.suffix_cmo, Literals.suffix_cma , "ocamlc"  , output_file, true
  | LinkNative output_file   -> Literals.suffix_cmx, Literals.suffix_cmxa, "ocamlopt", output_file, false
  end in
  (* Map used to track the path to the files as the dependency_graph that we're going to read from the mlast file only contains module names *)
  let module_to_filepath = Ext_list.fold_left batch_files Map_string.empty
    (fun m v ->
      Map_string.add m
      (Ext_filename.module_name (module_of_filename v))
      (Ext_filename.chop_extension_maybe v)
      )    
  in
  let rec get_dep_graph (stack_of_things: string list) dependency_graph =
    match stack_of_things with 
    | [] -> dependency_graph
    | module_name :: rest_of_things ->
    if Map_string.mem dependency_graph module_name then
      get_dep_graph rest_of_things dependency_graph
    else
      begin match Map_string.find_opt module_to_filepath module_name with
      | Some file -> 
        let suffix = if Sys.file_exists (file ^ Literals.suffix_mlast)
          then Literals.suffix_mlast
          else Literals.suffix_reast in
        let new_dependencies = Bsb_helper_extract.read_dependency_graph_from_mlast_file (file ^ suffix) in
        let dependency_graph = Map_string.add dependency_graph
          (Ext_filename.module_name module_name)
          new_dependencies in
        get_dep_graph (Set_string.elements new_dependencies @ rest_of_things) dependency_graph
      | None -> get_dep_graph rest_of_things dependency_graph
      end
    in
    let dependency_graph = get_dep_graph [main_module]  Map_string.empty in
  (* let dependency_graph = Ext_list.fold_left batch_files Map_string.empty
    (fun m file ->
      let module_name = module_of_filename file in
      let suffix = if Sys.file_exists (module_name ^ Literals.suffix_mlast) then Literals.suffix_mlast
        else Literals.suffix_reast in
      Map_string.add m
        (Ext_filename.module_name module_name)
        (Bsb_helper_extract.read_dependency_graph_from_mlast_file (module_name ^ suffix))
        )    
  in *)
  let ocaml_dependencies =
    List.fold_left (fun acc v -> 
      match v with
      | "threads" -> 
      "-thread" :: (Bsb_global_paths_native.ocaml_dir // "lib" // "ocaml" // "threads" // "threads" ^ suffix_library_files) :: acc
      | v -> (Bsb_global_paths_native.ocaml_dir // "lib" // "ocaml" // v ^ suffix_library_files) :: acc
      ) [] ocaml_dependencies in
  let warning_command = if String.length warnings > 0 then
  "-w" :: warnings :: []
     else [] in 
  let warning_command = if String.length warn_error > 0 then
  "-warn-error" :: warn_error :: warning_command
  else warning_command in
  
  let tasks = Bsb_helper_dep_graph.simple_collect_from_main dependency_graph main_module in
  let namespace = match namespace with 
     | None -> ""
     | Some namespace -> "-" ^ namespace
   in
  let list_of_object_files = Queue.fold
    (fun acc v -> match Map_string.find_opt module_to_filepath v with
      | Some file -> (file ^ namespace ^ suffix_object_files) :: acc
      | None -> Bsb_exception.missing_object_file v
      )
    []
    tasks in
  if list_of_object_files <> [] then begin
    let library_files = Ext_list.fold_left includes [] 
      (fun acc dir ->
        (Ext_path.combine dir (Literals.library_file ^ suffix_library_files)) :: acc)
    in
    let clibs = if add_custom && clibs <> [] then
      "-custom" :: clibs
    else
      clibs
    in
    (* This list will be reversed so we append the otherlibs object files at the end, and they'll end at the beginning. *)
    (* let otherlibs = Bsb_helper_dep_graph.get_otherlibs_dependencies dependency_graph suffix_library_files in *)
    let all_object_files = ocaml_dependencies @ library_files @ List.rev (list_of_object_files) @ clibs in
    (* let compiler_extension = if Ext_sys.is_windows_or_cygwin then ".opt.exe" else ".opt" in *)
    let compiler_extension = ".opt.exe" in
    let local_compiler = Bsb_global_paths_native.ocaml_dir // "bin" // compiler ^ compiler_extension in
    let super_errors = if false then ["-bs-super-errors"] else [] in
    let list_of_args = (local_compiler :: "-g" ::
      warning_command) @ super_errors @ "-o" :: output_file :: all_object_files in
    if verbose then
      print_endline("Bsb_helper link command:\n" ^ (String.concat "  " list_of_args) ^ "\n");

    Unix.execvp local_compiler (Array.of_list (list_of_args))
  end else
    failwith @@ "No " ^ suffix_object_files ^ " to link. Hint: is the main module in the entries array right?"

end
module Bsb_helper_packer : sig 
#1 "bsb_helper_packer.mli"
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

type pack_t = PackBytecode | PackNative

val pack : pack_t -> 
  batch_files:string list ->
  includes:string list ->
  namespace:string option ->
  warnings: string -> 
  warn_error: string ->
  verbose: bool ->
  build_library: string option ->
  cwd:string ->
  unit

end = struct
#1 "bsb_helper_packer.ml"
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

type pack_t = PackBytecode | PackNative

let ( // ) = Ext_path.combine

(* The packer is called with object files (.cmo / .cmx) which will be namespaced and we're using
   those names to read-in the mlast files which are not namespaced. So we strip the namespace
   before reading them in. *)
let module_of_filename filename = 
  let str = Ext_filename.chop_extension_maybe filename in
  match (String.rindex str '-') with 
  | exception Not_found -> str
  | len -> String.sub str 0 len

let pack pack_byte_or_native ~batch_files ~includes ~namespace ~warnings ~warn_error ~verbose ~build_library ~cwd =
  let suffix_object_files, suffix_library_files, compiler, nested = begin match pack_byte_or_native with
  | PackBytecode -> Literals.suffix_cmo, Literals.suffix_cma , "ocamlc", "bytecode"
  | PackNative   -> Literals.suffix_cmx, Literals.suffix_cmxa, "ocamlopt", "native"
  end in
  let module_to_filepath = Ext_list.fold_left batch_files  Map_string.empty
    (fun m v ->
      Map_string.add m
      (Ext_filename.module_name (module_of_filename v))
      (Ext_filename.chop_extension_maybe v)
      )  
  in
  let dependency_graph = Ext_list.fold_left batch_files Map_string.empty
    (fun m file ->
      let module_name = module_of_filename file in
      let suffix = if Sys.file_exists (module_name ^ Literals.suffix_mlast) then Literals.suffix_mlast
        else Literals.suffix_reast in
      Map_string.add m
        (Ext_filename.module_name module_name)
        (Bsb_helper_extract.read_dependency_graph_from_mlast_file (module_name ^ suffix))
        )    
  in
  let all_object_files = match build_library with
  | None -> 
    let domain =
      Map_string.fold dependency_graph Set_string.empty 
        (fun k _ acc -> Set_string.add acc k)
        in
    let sorted_tasks = Bsb_helper_dep_graph.sort_files_by_dependencies ~domain dependency_graph in
    List.rev (Queue.fold
      (fun acc v -> match Map_string.find_opt module_to_filepath v with
        | Some file -> (file ^ suffix_object_files) :: acc
        | None -> failwith @@ "build.ninja is missing the file '" ^ v ^ "' that was used in the project. Try force-regenerating but this shouldn't happen."
        )
      []
      sorted_tasks)
  | Some build_library -> 
    let tasks = Bsb_helper_dep_graph.simple_collect_from_main dependency_graph build_library in
    let namespace = match namespace with 
      | None -> ""
      | Some namespace -> "-" ^ namespace
    in
    List.rev (Queue.fold
        (fun acc v -> match Map_string.find_opt module_to_filepath v with
          | Some file -> (file ^ namespace ^ suffix_object_files) :: acc
          | None -> Bsb_exception.missing_object_file v
          )
        []
        tasks)
  in
  let warning_command = if String.length warnings > 0 then
    "-w" :: warnings :: []
  else [] in 
  let warning_command = if String.length warn_error > 0 then
    "-warn-error" :: warn_error :: warning_command
  else warning_command in

  if all_object_files <> [] then
    let includes = Ext_list.fold_left includes [] (fun acc dir -> "-I" :: dir :: acc)  in
    let all_object_files = match namespace with
      | None -> all_object_files
      | Some namespace -> (namespace ^ suffix_object_files) :: all_object_files 
    in
    let compiler_extension = ".opt.exe" in
    let local_compiler = Bsb_global_paths_native.ocaml_dir // "bin" // compiler ^ compiler_extension in
    
    let super_errors = if false then ["-bs-super-errors"] else [] in
    let list_of_args = (local_compiler :: "-a" :: "-g" ::
      warning_command) @ super_errors @ "-o" :: (cwd // Literals.library_file ^ suffix_library_files) :: includes 
      @ all_object_files in

    if verbose then
      print_endline("Bsb_helper pack command:\n" ^ (String.concat "  " list_of_args) ^ "\n");

    Unix.execvp
      local_compiler
        (Array.of_list list_of_args)
  else
    Bsb_exception.no_files_to_pack suffix_object_files

end
module Bsb_helper_main : sig 
#1 "bsb_helper_main.mli"
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


 (** Used to generate .d file, for example 
  {[
    bsb_helper.exe -g 0 -MD  src/hi/hello.ml
  ]}
  It will read the cache file and generate the corresponding
     [.d] file. This [.d] file will be used as attribute [depfile]
  whether we use namespace or not, the filename of [.mlast], [.d] 
  should be kept the same, we only need change the name of [.cm*]
  and the contents of filename in [.d]
 *)

end = struct
#1 "bsb_helper_main.ml"
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

let verbose = ref false

let warnings = ref ""

let warn_error = ref ""

let ocaml_dependencies = ref []

let add_ocaml_dependencies s = 
  ocaml_dependencies := s :: !ocaml_dependencies

let main_module = ref None

let clibs = ref []
let add_clib file = clibs := file :: !clibs 

let build_library = ref None

let set_main_module modulename =
  main_module := Some (Ext_string.capitalize_ascii modulename)

let includes :  _ list ref = ref []

let add_include =
  let normalize cwd s =
    Ext_path.normalize_absolute_path (Ext_path.combine cwd s) in
  fun dir ->
    includes := (normalize (Sys.getcwd ()) dir) :: !includes

let compilation_kind = ref Bsb_helper_depfile_gen.Js

let hash : string ref = ref ""
let batch_files = ref []
let collect_file name =
  batch_files := name :: !batch_files

(* let output_prefix = ref None *)
let dev_group = ref 0
let namespace = ref None


let anonymous filename =
  collect_file filename
let usage = "Usage: bsb_helper.exe [options] \nOptions are:"

let link link_byte_or_native = 
  begin match !main_module with
    | None -> failwith "Linking needs a main module. Please add -bs-main MyMainModule to the invocation."
    | Some main_module ->
      Bsb_helper_linker.link 
        link_byte_or_native
        ~main_module:main_module
        (* `includes` is not reversed here because it gets reversed inside when we fold_list and 
         ~batch_files:!batch_files		           prepend a new list. *)
       ~includes:!includes
       ~batch_files:!batch_files
       ~namespace:!namespace
       ~ocaml_dependencies:(List.rev !ocaml_dependencies)
       ~warnings:!warnings
       ~warn_error:!warn_error
       ~verbose:!verbose
       ~cwd:(Sys.getcwd ())
       ~clibs:(List.rev !clibs)
  end
let pack link_byte_or_native =
   Bsb_helper_packer.pack
     link_byte_or_native
     ~includes:!includes
     ~batch_files:!batch_files
     ~namespace:!namespace
     ~warnings:!warnings
     ~warn_error:!warn_error
     ~verbose:!verbose
     ~build_library:!build_library
     ~cwd:(Sys.getcwd ())
  
let () =
  Bsb_helper_arg.parse_exn [
    "-g",  Set_int dev_group ,
    " Set the dev group (default to be 0)"
    ;
    "-bs-ns",  String (fun s -> namespace := Some s),
    " Set namespace";
    "-hash",  Set_string hash,
    " Set hash(internal)";
    
    "-MD-bytecode", Unit (fun () -> 
      compilation_kind := Bsb_helper_depfile_gen.Bytecode
    ),          
    " (internal)Generate dep file for ninja format(from .ml[i]deps)";
    "-MD-native", Unit (fun () -> 
        compilation_kind := Bsb_helper_depfile_gen.Native
           ),
    " (internal)Generate dep file for ninja format(from .ml[i]deps)";

    (**
       The args below are used for packing/linking.

       This makes bsb_helper act as an ocaml linker where we automatically figure
       out the dependencies graph to do a topological sort before calling 
       ocamlc/ocamlopt.
    *)
    "-bs-main", ( String set_main_module),
    " set the main entry module. Only used in conjunction with -link-bytecode and -link-native";

    (* This is a way to add a directory to the search path. This is used for the 
       compiler to look for cmi files. It's also used to look for a file called `lib.cma` to 
       link with the current executable.

       For example if called like so

          bsb_helper -I theExtLib myMainFile.cmo -link-bytecode

       Then we'll go look for `theExtLib/lib.cma` to link with the final exec.
    *)
    "-I",  ( String add_include),
    " add dir to search path for the linker and packer";

    (* Both linking and packing arguments must come _after_ all of the other args and files have been listed.
       For example:

          bsb_helper -bs-main MyModule myFile.cmo myOtherFile.cmo -link-bytecode 

       In the following example, the file called `myIgnoredFile.cmo` is not linked nor is `myLibFolder/lib.cma`

          bsb_helper -bs-main MyModule myFile.cmo myOtherFile.cmo -link-bytecode -I myLibFolder myIgnoredFile.cmo

    *)
    "-link-bytecode", ( String (fun x -> link (Bsb_helper_linker.LinkBytecode x))),
    " link bytecode files into an executable";

    "-link-native", ( String (fun x -> link (Bsb_helper_linker.LinkNative x))),
    " link native files into an executable";

    "-pack-native-library", ( Unit (fun () -> 
        pack Bsb_helper_packer.PackNative
      )),
    " pack native files (cmx) into a library file (cmxa)";

    "-pack-bytecode-library", ( Unit (fun () -> 
        pack Bsb_helper_packer.PackBytecode
      )),
    " pack bytecode files (cmo) into a library file (cma)";

    "-add-ocaml-dependency", ( String add_ocaml_dependencies),
    " Add a dependency on otherlibs or compiler-libs.";

    "-w", ( String (fun w -> warnings := w )),
    " Use warnings for packer/linker.";

    "-warn-error", ( String (fun w -> warn_error := w )),
    " Turn warnings into errors for packer/linker.";

    "-verbose", ( Unit (fun v -> verbose := true)),
    " Turn on verbose Maude.";

    "-add-clib", (String add_clib),
    " adds a .a library file to be linked into the final executable";

    "-build-library", (String (fun v -> build_library := Some v)),
    " Create a library file with all the object files from the given entry point."
    
  ] anonymous usage;
  (* arrange with mlast comes first *)
  match !batch_files with
  | [x]
    ->  Bsb_helper_depfile_gen.emit_d
          !compilation_kind
          (Bsb_dir_index.of_int !dev_group )          
          !namespace x ""
  | [y; x] (* reverse order *)
    -> 
    Bsb_helper_depfile_gen.emit_d
      !compilation_kind
      (Bsb_dir_index.of_int !dev_group)
      !namespace x y
  | _ -> 
    ()

end

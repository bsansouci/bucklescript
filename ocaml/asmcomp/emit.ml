# 1 "amd64/emit.mlp"
# 2 "asmcomp/amd64/emit.mlp"
(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Emission of Intel x86_64 assembly code *)

open Misc
open Cmm
open Arch
open Proc
open Reg
open Mach
open Linearize
open Emitaux

open X86_ast
open X86_proc
open X86_dsl

(* [Branch_relaxation] is not used in this file, but is required by
   emit.mlp files for certain other targets; the reference here ensures
   that when releases are being prepared the .depend files are correct
   for all targets. *)
open! Branch_relaxation

let _label s = D.label ~typ:QWORD s

(* Override proc.ml *)

let int_reg_name =
  [| RAX; RBX; RDI; RSI; RDX; RCX; R8; R9;
     R12; R13; R10; R11; RBP; |]

let float_reg_name = Array.init 16 (fun i -> XMM i)

let register_name r =
  if r < 100 then Reg64 (int_reg_name.(r))
  else Regf (float_reg_name.(r - 100))

(* CFI directives *)

let cfi_startproc () =
  if Config.asm_cfi_supported then D.cfi_startproc ()

let cfi_endproc () =
  if Config.asm_cfi_supported then D.cfi_endproc ()

let cfi_adjust_cfa_offset n =
  if Config.asm_cfi_supported then D.cfi_adjust_cfa_offset n

let emit_debug_info dbg =
  emit_debug_info_gen dbg D.file D.loc

let fp = Config.with_frame_pointers

(* Tradeoff between code size and code speed *)

let fastcode_flag = ref true

let stack_offset = ref 0

(* Layout of the stack frame *)

let frame_required () =
  fp || !contains_calls || num_stack_slots.(0) > 0 || num_stack_slots.(1) > 0

let frame_size () =                     (* includes return address *)
  if frame_required() then begin
    let sz =
      (!stack_offset + 8 * (num_stack_slots.(0) + num_stack_slots.(1)) + 8
       + (if fp then 8 else 0))
    in Misc.align sz 16
  end else
    !stack_offset + 8

let slot_offset loc cl =
  match loc with
  | Incoming n -> frame_size() + n
  | Local n ->
      if cl = 0
      then !stack_offset + n * 8
      else !stack_offset + (num_stack_slots.(0) + n) * 8
  | Outgoing n -> n

(* Symbols *)

let symbol_prefix = if system = S_macosx then "_" else ""

let emit_symbol s = string_of_symbol symbol_prefix s

(* Record symbols used and defined - at the end generate extern for those
   used but not defined *)

let symbols_defined = ref StringSet.empty
let symbols_used = ref StringSet.empty

let add_def_symbol s = symbols_defined := StringSet.add s !symbols_defined
let add_used_symbol s = symbols_used := StringSet.add s !symbols_used

let imp_table = Hashtbl.create 16

let reset_imp_table () = Hashtbl.clear imp_table

let get_imp_symbol s =
  match Hashtbl.find imp_table s with
  | exception Not_found ->
      let imps = "__caml_imp_" ^ s in
      Hashtbl.add imp_table s imps;
      imps
  | imps -> imps

let emit_imp_table () =
  let f s imps =
    _label (emit_symbol imps);
    D.qword (ConstLabel (emit_symbol s))
  in
  D.data();
  D.comment "relocation table start";
  D.align 8;
  Hashtbl.iter f imp_table;
  D.comment "relocation table end"

let mem__imp s =
  let imp_s = get_imp_symbol s in
  mem64_rip QWORD (emit_symbol imp_s)

let rel_plt s =
  if windows && !Clflags.dlcode then mem__imp s
  else
    sym (if use_plt then emit_symbol s ^ "@PLT" else emit_symbol s)

let emit_call s = I.call (rel_plt s)

let emit_jump s = I.jmp (rel_plt s)

let load_symbol_addr s arg =
  if !Clflags.dlcode then
    if windows then begin
      (* I.mov (mem__imp s) arg (\* mov __caml_imp_foo(%rip), ... *\) *)
      I.mov (sym (emit_symbol s)) arg (* movabsq $foo, ... *)
    end else I.mov (mem64_rip QWORD (emit_symbol s ^ "@GOTPCREL")) arg
  else if !Clflags.pic_code then
    I.lea (mem64_rip NONE (emit_symbol s)) arg
  else
    I.mov (sym (emit_symbol s)) arg

(* Output a label *)

let emit_label lbl =
  match system with
  | S_macosx | S_win64 -> "L" ^ string_of_int lbl
  | _ -> ".L" ^ string_of_int lbl

let label s = sym (emit_label s)

let def_label s = D.label (emit_label s)

let emit_Llabel fallthrough lbl =
  if not fallthrough && !fastcode_flag then D.align 4;
  def_label lbl

(* Output a pseudo-register *)

let reg = function
  | { loc = Reg.Reg r } -> register_name r
  | { loc = Stack s; typ = Float } as r ->
      let ofs = slot_offset s (register_class r) in
      mem64 REAL8 ofs RSP
  | { loc = Stack s } as r ->
      let ofs = slot_offset s (register_class r) in
      mem64 QWORD ofs RSP
  | { loc = Unknown } ->
      assert false

let reg64 = function
  | { loc = Reg.Reg r } -> int_reg_name.(r)
  | _ -> assert false


let res i n = reg i.res.(n)

let arg i n = reg i.arg.(n)

(* Output a reference to the lower 8, 16 or 32 bits of a register *)

let reg_low_8_name  = Array.map (fun r -> Reg8L r) int_reg_name
let reg_low_16_name = Array.map (fun r -> Reg16 r) int_reg_name
let reg_low_32_name = Array.map (fun r -> Reg32 r) int_reg_name

let emit_subreg tbl typ r =
  match r.loc with
  | Reg.Reg r when r < 13 -> tbl.(r)
  | Stack s -> mem64 typ (slot_offset s (register_class r)) RSP
  | _ -> assert false

let arg8 i n = emit_subreg reg_low_8_name BYTE i.arg.(n)
let arg16 i n = emit_subreg reg_low_16_name WORD i.arg.(n)
let arg32 i n = emit_subreg reg_low_32_name DWORD i.arg.(n)
let arg64 i n = reg64 i.arg.(n)

let res16 i n = emit_subreg reg_low_16_name WORD i.res.(n)
let res32 i n = emit_subreg reg_low_32_name DWORD i.res.(n)

(* Output an addressing mode *)

let addressing addr typ i n =
  match addr with
  | Ibased(s, ofs) ->
      add_used_symbol s;
      mem64_rip typ (emit_symbol s) ~ofs
  | Iindexed d ->
      mem64 typ d (arg64 i n)
  | Iindexed2 d ->
      mem64 typ ~base:(arg64 i n) d (arg64 i (n+1))
  | Iscaled(2, d) ->
      mem64 typ ~base:(arg64 i n) d (arg64 i n)
  | Iscaled(scale, d) ->
      mem64 typ ~scale d (arg64 i n)
  | Iindexed2scaled(scale, d) ->
      mem64 typ ~scale ~base:(arg64 i n) d (arg64 i (n+1))

(* Record live pointers at call points -- see Emitaux *)

let record_frame_label ?label live raise_ dbg =
  let lbl =
    match label with
    | None -> new_label()
    | Some label -> label
  in
  let live_offset = ref [] in
  Reg.Set.iter
    (function
      | {typ = Val; loc = Reg r} ->
          live_offset := ((r lsl 1) + 1) :: !live_offset
      | {typ = Val; loc = Stack s} as reg ->
          live_offset := slot_offset s (register_class reg) :: !live_offset
      | {typ = Addr} as r ->
          Misc.fatal_error ("bad GC root " ^ Reg.name r)
      | _ -> ()
    )
    live;
  record_frame_descr ~label:lbl ~frame_size:(frame_size())
    ~live_offset:!live_offset ~raise_frame:raise_ dbg;
  lbl

let record_frame ?label live raise_ dbg =
  let lbl = record_frame_label ?label live raise_ dbg in
  def_label lbl

(* Spacetime instrumentation *)

let spacetime_before_uninstrumented_call ~node_ptr ~index =
  (* At the moment, [node_ptr] is pointing at the node for the current
     OCaml function.  Get hold of the node itself and move the pointer
     forwards, saving it into the distinguished register.  This is used
     for instrumentation of function calls (e.g. caml_call_gc and bounds
     check failures) not inserted until this stage of the compiler
     pipeline. *)
  I.mov node_ptr (reg Proc.loc_spacetime_node_hole);
  assert (index >= 2);
  I.add (int (index * 8)) (reg Proc.loc_spacetime_node_hole)

(* Record calls to the GC -- we've moved them out of the way *)

type gc_call =
  { gc_lbl: label;                      (* Entry label *)
    gc_return_lbl: label;               (* Where to branch after GC *)
    gc_frame: label;                    (* Label of frame descriptor *)
    gc_spacetime : (X86_ast.arg * int) option;
    (* Spacetime node hole pointer and index *)
  }

let call_gc_sites = ref ([] : gc_call list)

let emit_call_gc gc =
  def_label gc.gc_lbl;
  begin match gc.gc_spacetime with
  | None -> assert (not Config.spacetime)
  | Some (node_ptr, index) ->
    assert Config.spacetime;
    spacetime_before_uninstrumented_call ~node_ptr ~index
  end;
  emit_call "caml_call_gc";
  def_label gc.gc_frame;
  I.jmp (label gc.gc_return_lbl)

(* Record calls to caml_ml_array_bound_error.
   In -g mode, or when using Spacetime profiling, we maintain one call to
   caml_ml_array_bound_error per bound check site.  Without -g, we can share
   a single call. *)

type bound_error_call =
  { bd_lbl: label;                      (* Entry label *)
    bd_frame: label;                    (* Label of frame descriptor *)
    bd_spacetime : (X86_ast.arg * int) option;
    (* As for [gc_call]. *)
  }

let bound_error_sites = ref ([] : bound_error_call list)
let bound_error_call = ref 0

let bound_error_label ?label dbg ~spacetime =
  if !Clflags.debug || Config.spacetime then begin
    let lbl_bound_error = new_label() in
    let lbl_frame = record_frame_label ?label Reg.Set.empty false dbg in
    bound_error_sites :=
      { bd_lbl = lbl_bound_error; bd_frame = lbl_frame;
        bd_spacetime = spacetime; } :: !bound_error_sites;
    lbl_bound_error
  end else begin
    if !bound_error_call = 0 then bound_error_call := new_label();
    !bound_error_call
  end

let emit_call_bound_error bd =
  def_label bd.bd_lbl;
  begin match bd.bd_spacetime with
  | None -> ()
  | Some (node_ptr, index) ->
    spacetime_before_uninstrumented_call ~node_ptr ~index
  end;
  emit_call "caml_ml_array_bound_error";
  def_label bd.bd_frame

let emit_call_bound_errors () =
  List.iter emit_call_bound_error !bound_error_sites;
  if !bound_error_call > 0 then begin
    def_label !bound_error_call;
    emit_call "caml_ml_array_bound_error"
  end

(* Names for instructions *)

let instr_for_intop = function
  | Iadd -> I.add
  | Isub -> I.sub
  | Imul -> (fun arg1 arg2 -> I.imul arg1 (Some arg2))
  | Iand -> I.and_
  | Ior -> I.or_
  | Ixor -> I.xor
  | Ilsl -> I.sal
  | Ilsr -> I.shr
  | Iasr -> I.sar
  | _ -> assert false

let instr_for_floatop = function
  | Iaddf -> I.addsd
  | Isubf -> I.subsd
  | Imulf -> I.mulsd
  | Idivf -> I.divsd
  | _ -> assert false

let instr_for_floatarithmem = function
  | Ifloatadd -> I.addsd
  | Ifloatsub -> I.subsd
  | Ifloatmul -> I.mulsd
  | Ifloatdiv -> I.divsd

let cond = function
  | Isigned Ceq   -> E   | Isigned Cne   -> NE
  | Isigned Cle   -> LE  | Isigned Cgt   -> G
  | Isigned Clt   -> L   | Isigned Cge   -> GE
  | Iunsigned Ceq -> E   | Iunsigned Cne -> NE
  | Iunsigned Cle -> BE  | Iunsigned Cgt -> A
  | Iunsigned Clt -> B   | Iunsigned Cge -> AE

(* Output an = 0 or <> 0 test. *)

let output_test_zero arg =
  match arg.loc with
  | Reg.Reg _ -> I.test (reg arg) (reg arg)
  | _  -> I.cmp (int 0) (reg arg)

(* Output a floating-point compare and branch *)

let emit_float_test cmp neg i lbl =
  (* Effect of comisd on flags and conditional branches:
                     ZF PF CF  cond. branches taken
        unordered     1  1  1  je, jb, jbe, jp
        >             0  0  0  jne, jae, ja
        <             0  0  1  jne, jbe, jb
        =             1  0  0  je, jae, jbe.
     If FP traps are on (they are off by default),
     comisd traps on QNaN and SNaN but ucomisd traps on SNaN only.
  *)
  match (cmp, neg) with
  | (Ceq, false) | (Cne, true) ->
      let next = new_label() in
      I.ucomisd (arg i 1) (arg i 0);
      I.jp (label next);          (* skip if unordered *)
      I.je lbl;                   (* branch taken if x=y *)
      def_label next
  | (Cne, false) | (Ceq, true) ->
      I.ucomisd (arg i 1) (arg i 0);
      I.jp lbl;                   (* branch taken if unordered *)
      I.jne lbl                   (* branch taken if x<y or x>y *)
  | (Clt, _) ->
      I.comisd (arg i 0) (arg i 1);
      if not neg then I.ja lbl    (* branch taken if y>x i.e. x<y *)
      else            I.jbe lbl   (* taken if unordered or y<=x i.e. !(x<y) *)
  | (Cle, _) ->
      I.comisd (arg i 0) (arg i 1);(* swap compare *)
      if not neg then I.jae lbl   (* branch taken if y>=x i.e. x<=y *)
      else            I.jb lbl    (* taken if unordered or y<x i.e. !(x<=y) *)
  | (Cgt, _) ->
      I.comisd (arg i 1) (arg i 0);
      if not neg then I.ja lbl    (* branch taken if x>y *)
      else            I.jbe lbl   (* taken if unordered or x<=y i.e. !(x>y) *)
  | (Cge, _) ->
      I.comisd (arg i 1) (arg i 0);(* swap compare *)
      if not neg then I.jae lbl   (* branch taken if x>=y *)
      else            I.jb lbl    (* taken if unordered or x<y i.e. !(x>=y) *)

(* Deallocate the stack frame before a return or tail call *)

let output_epilogue f =
  if frame_required() then begin
    let n = frame_size() - 8 - (if fp then 8 else 0) in
    if n <> 0
    then begin
      I.add (int n) rsp;
      cfi_adjust_cfa_offset (-n);
    end;
    if fp then I.pop rbp;
    f ();
    (* reset CFA back cause function body may continue *)
    if n <> 0
    then cfi_adjust_cfa_offset n
  end
  else
    f ()

(* Floating-point constants *)

let float_constants = ref ([] : (int64 * int) list)

let add_float_constant cst =
  try
    List.assoc cst !float_constants
  with Not_found ->
    let lbl = new_label() in
    float_constants := (cst, lbl) :: !float_constants;
    lbl

let emit_float_constant f lbl =
  _label (emit_label lbl);
  D.qword (Const f)

let emit_global_label s =
  let lbl = Compilenv.make_symbol (Some s) in
  add_def_symbol lbl;
  let lbl = emit_symbol lbl in
  D.global lbl;
  _label lbl


(* Output the assembly code for an instruction *)

(* Name of current function *)
let function_name = ref ""
(* Entry point for tail recursive calls *)
let tailrec_entry_point = ref 0

(* Emit an instruction *)
let emit_instr fallthrough i =
  emit_debug_info i.dbg;
  match i.desc with
  | Lend -> ()
  | Lop(Imove | Ispill | Ireload) ->
      let src = i.arg.(0) and dst = i.res.(0) in
      if src.loc <> dst.loc then
        begin match src.typ, src.loc, dst.loc with
        | Float, Reg.Reg _, Reg.Reg _ -> I.movapd (reg src) (reg dst)
        | Float, _, _ -> I.movsd (reg src) (reg dst)
        | _ -> I.mov (reg src) (reg dst)
        end
  | Lop(Iconst_int n) ->
      if n = 0n then begin
        match i.res.(0).loc with
        | Reg _ -> I.xor (res i 0) (res i 0)
        | _     -> I.mov (int 0) (res i 0)
      end
      else
        I.mov (nat n) (res i 0)
  | Lop(Iconst_float f) ->
      begin match f with
      | 0x0000_0000_0000_0000L ->       (* +0.0 *)
          I.xorpd (res i 0) (res i 0)
      | _ ->
          let lbl = add_float_constant f in
          I.movsd (mem64_rip NONE (emit_label lbl)) (res i 0)
      end
  | Lop(Iconst_symbol s) ->
      add_used_symbol s;
      load_symbol_addr s (res i 0)
  | Lop(Icall_ind { label_after; }) ->
      I.call (arg i 0);
      record_frame i.live false i.dbg ~label:label_after
  | Lop(Icall_imm { func; label_after; }) ->
      add_used_symbol func;
      emit_call func;
      record_frame i.live false i.dbg ~label:label_after
  | Lop(Itailcall_ind { label_after; }) ->
      output_epilogue begin fun () ->
        I.jmp (arg i 0);
        if Config.spacetime then begin
          record_frame Reg.Set.empty false i.dbg ~label:label_after
        end
      end
  | Lop(Itailcall_imm { func; label_after; }) ->
      begin
        if func = !function_name then
          I.jmp (label !tailrec_entry_point)
        else begin
          output_epilogue begin fun () ->
            add_used_symbol func;
            emit_jump func
          end
        end
      end;
      if Config.spacetime then begin
        record_frame Reg.Set.empty false i.dbg ~label:label_after
      end
  | Lop(Iextcall { func; alloc; label_after; }) ->
      add_used_symbol func;
      if alloc then begin
        load_symbol_addr func rax;
        emit_call "caml_c_call";
        record_frame i.live false i.dbg ~label:label_after;
        if system <> S_win64 then begin
          (* TODO: investigate why such a diff.
             This comes from:
            http://caml.inria.fr/cgi-bin/viewvc.cgi?view=revision&revision=12664

             If we do the same for Win64, we probably need to change
             amd64nt.asm accordingly.
          *)
          load_symbol_addr "caml_young_ptr" r11;
          I.mov (mem64 QWORD 0 R11) r15
        end
      end else begin
        emit_call func;
        if Config.spacetime then begin
          record_frame Reg.Set.empty false i.dbg ~label:label_after
        end
      end
  | Lop(Istackoffset n) ->
      if n < 0
      then I.add (int (-n)) rsp
      else if n > 0
      then I.sub (int n) rsp;
      if n <> 0
      then cfi_adjust_cfa_offset n;
      stack_offset := !stack_offset + n
  | Lop(Iload(chunk, addr)) ->
      let dest = res i 0 in
      begin match chunk with
      | Word_int | Word_val ->
          I.mov (addressing addr QWORD i 0) dest
      | Byte_unsigned ->
          I.movzx (addressing addr BYTE i 0) dest
      | Byte_signed ->
          I.movsx (addressing addr BYTE i 0) dest
      | Sixteen_unsigned ->
          I.movzx (addressing addr WORD i 0) dest
      | Sixteen_signed ->
          I.movsx (addressing addr WORD i 0) dest;
      | Thirtytwo_unsigned ->
          I.mov (addressing addr DWORD i 0) (res32 i 0)
      | Thirtytwo_signed ->
          I.movsxd (addressing addr DWORD i 0) dest
      | Single ->
          I.cvtss2sd (addressing addr REAL4 i 0) dest
      | Double | Double_u ->
          I.movsd (addressing addr REAL8 i 0) dest
      end
  | Lop(Istore(chunk, addr, _)) ->
      begin match chunk with
      | Word_int | Word_val ->
          I.mov (arg i 0) (addressing addr QWORD i 1)
      | Byte_unsigned | Byte_signed ->
          I.mov (arg8 i 0) (addressing addr BYTE i 1)
      | Sixteen_unsigned | Sixteen_signed ->
          I.mov (arg16 i 0) (addressing addr WORD i 1)
      | Thirtytwo_signed | Thirtytwo_unsigned ->
          I.mov (arg32 i 0) (addressing addr DWORD i 1)
      | Single ->
          I.cvtsd2ss (arg i 0) xmm15;
          I.movss xmm15 (addressing addr REAL4 i 1)
      | Double | Double_u ->
          I.movsd (arg i 0) (addressing addr REAL8 i 1)
      end
  | Lop(Ialloc { words = n; label_after_call_gc; spacetime_index; }) ->
      if !fastcode_flag then begin
        let lbl_redo = new_label() in
        def_label lbl_redo;
        I.sub (int n) r15;
        let spacetime_node_hole_ptr_is_in_rax =
          Config.spacetime && (i.arg.(0).loc = Reg 0)
        in
        if !Clflags.dlcode then begin
          (* When using Spacetime, %rax might be the node pointer, so we
             must take care not to clobber it.  (Whilst we can tell the
             register allocator that %rax is destroyed by Ialloc, we can't
             force that the argument (the node pointer) is not in %rax.) *)
          if spacetime_node_hole_ptr_is_in_rax then begin
            I.push rax
          end;
          load_symbol_addr "caml_young_limit" rax;
          I.cmp (mem64 QWORD 0 RAX) r15;
          if spacetime_node_hole_ptr_is_in_rax then begin
            I.pop rax  (* this does not affect the flags *)
          end
        end else
          I.cmp (mem64_rip QWORD (emit_symbol "caml_young_limit")) r15;
        let lbl_call_gc = new_label() in
        let dbg =
          if not Config.spacetime then Debuginfo.none
          else i.dbg
        in
        let lbl_frame =
          record_frame_label ?label:label_after_call_gc i.live false dbg
        in
        I.jb (label lbl_call_gc);
        I.lea (mem64 NONE 8 R15) (res i 0);
        let gc_spacetime =
          if not Config.spacetime then None
          else Some (arg i 0, spacetime_index)
        in
        call_gc_sites :=
          { gc_lbl = lbl_call_gc;
            gc_return_lbl = lbl_redo;
            gc_frame = lbl_frame;
            gc_spacetime; } :: !call_gc_sites
      end else begin
        if Config.spacetime then begin
          spacetime_before_uninstrumented_call ~node_ptr:(arg i 0)
            ~index:spacetime_index;
        end;
        begin match n with
        | 16 -> emit_call "caml_alloc1"
        | 24 -> emit_call "caml_alloc2"
        | 32 -> emit_call "caml_alloc3"
        | _  ->
            I.mov (int n) rax;
            emit_call "caml_allocN"
        end;
        let label =
          record_frame_label ?label:label_after_call_gc i.live false
            Debuginfo.none
        in
        def_label label;
        I.lea (mem64 NONE 8 R15) (res i 0)
      end
  | Lop(Iintop(Icomp cmp)) ->
      I.cmp (arg i 1) (arg i 0);
      I.set (cond cmp) al;
      I.movzx al (res i 0)
  | Lop(Iintop_imm(Icomp cmp, n)) ->
      I.cmp (int n) (arg i 0);
      I.set (cond cmp) al;
      I.movzx al (res i 0)
  | Lop(Iintop (Icheckbound { label_after_error; spacetime_index; } )) ->
      let spacetime =
        if not Config.spacetime then None
        else Some (arg i 2, spacetime_index)
      in
      let lbl = bound_error_label ?label:label_after_error i.dbg ~spacetime in
      I.cmp (arg i 1) (arg i 0);
      I.jbe (label lbl)
  | Lop(Iintop_imm(Icheckbound { label_after_error; spacetime_index; }, n)) ->
      let spacetime =
        if not Config.spacetime then None
        else Some (arg i 1, spacetime_index)
      in
      let lbl = bound_error_label ?label:label_after_error i.dbg ~spacetime in
      I.cmp (int n) (arg i 0);
      I.jbe (label lbl)
  | Lop(Iintop(Idiv | Imod)) ->
      I.cqo ();
      I.idiv (arg i 1)
  | Lop(Iintop(Ilsl | Ilsr | Iasr as op)) ->
      (* We have i.arg.(0) = i.res.(0) and i.arg.(1) = %rcx *)
      instr_for_intop op cl (res i 0)
  | Lop(Iintop Imulh) ->
      I.imul (arg i 1) None
  | Lop(Iintop op) ->
      (* We have i.arg.(0) = i.res.(0) *)
      instr_for_intop op (arg i 1) (res i 0)
  | Lop(Iintop_imm(Iadd, n)) when i.arg.(0).loc <> i.res.(0).loc ->
      I.lea (mem64 NONE n (arg64 i 0)) (res i 0)
  | Lop(Iintop_imm(Iadd, 1) | Iintop_imm(Isub, -1)) ->
      I.inc (res i 0)
  | Lop(Iintop_imm(Iadd, -1) | Iintop_imm(Isub, 1)) ->
      I.dec (res i 0)
  | Lop(Iintop_imm(op, n)) ->
      (* We have i.arg.(0) = i.res.(0) *)
      instr_for_intop op (int n) (res i 0)
  | Lop(Inegf) ->
      I.xorpd (mem64_rip OWORD (emit_symbol "caml_negf_mask")) (res i 0)
  | Lop(Iabsf) ->
      I.andpd (mem64_rip OWORD (emit_symbol "caml_absf_mask")) (res i 0)
  | Lop(Iaddf | Isubf | Imulf | Idivf as floatop) ->
      instr_for_floatop floatop (arg i 1) (res i 0)
  | Lop(Ifloatofint) ->
      I.cvtsi2sd  (arg i 0)  (res i 0)
  | Lop(Iintoffloat) ->
      I.cvttsd2si (arg i 0) (res i 0)
  | Lop(Ispecific(Ilea addr)) ->
      I.lea (addressing addr NONE i 0) (res i 0)
  | Lop(Ispecific(Istore_int(n, addr, _))) ->
      I.mov (nat n) (addressing addr QWORD i 0)
  | Lop(Ispecific(Ioffset_loc(n, addr))) ->
      I.add (int n) (addressing addr QWORD i 0)
  | Lop(Ispecific(Ifloatarithmem(op, addr))) ->
      instr_for_floatarithmem op (addressing addr REAL8 i 1) (res i 0)
  | Lop(Ispecific(Ibswap 16)) ->
      I.xchg ah al;
      I.movzx (res16 i 0) (res i 0)
  | Lop(Ispecific(Ibswap 32)) ->
      I.bswap (res32 i 0);
      I.movsxd (res32 i 0) (res i 0)
  | Lop(Ispecific(Ibswap 64)) ->
      I.bswap (res i 0)
  | Lop(Ispecific(Ibswap _)) ->
      assert false
  | Lop(Ispecific Isqrtf) ->
      I.sqrtsd (arg i 0) (res i 0)
  | Lop(Ispecific(Ifloatsqrtf addr)) ->
      I.sqrtsd (addressing addr REAL8 i 0) (res i 0)
  | Lop (Iname_for_debugger _) -> ()
  | Lreloadretaddr ->
      ()
  | Lreturn ->
      output_epilogue begin fun () ->
        I.ret ()
      end
  | Llabel lbl ->
      emit_Llabel fallthrough lbl
  | Lbranch lbl ->
      I.jmp (label lbl)
  | Lcondbranch(tst, lbl) ->
      let lbl = label lbl in
      begin match tst with
      | Itruetest ->
          output_test_zero i.arg.(0);
          I.jne lbl
      | Ifalsetest ->
          output_test_zero i.arg.(0);
          I.je lbl
      | Iinttest cmp ->
          I.cmp (arg i 1) (arg i 0);
          I.j (cond cmp) lbl
      | Iinttest_imm((Isigned Ceq | Isigned Cne |
                      Iunsigned Ceq | Iunsigned Cne) as cmp, 0) ->
          output_test_zero i.arg.(0);
          I.j (cond cmp) lbl
      | Iinttest_imm(cmp, n) ->
          I.cmp (int n) (arg i 0);
          I.j (cond cmp) lbl
      | Ifloattest(cmp, neg) ->
          emit_float_test cmp neg i lbl
      | Ioddtest ->
          I.test (int 1) (arg8 i 0);
          I.jne lbl
      | Ieventest ->
          I.test (int 1) (arg8 i 0);
          I.je lbl
      end
  | Lcondbranch3(lbl0, lbl1, lbl2) ->
      I.cmp (int 1) (arg i 0);
      begin match lbl0 with
      | None -> ()
      | Some lbl -> I.jb (label lbl)
      end;
      begin match lbl1 with
      | None -> ()
      | Some lbl -> I.je (label lbl)
      end;
      begin match lbl2 with
      | None -> ()
      | Some lbl -> I.jg (label lbl)
      end
  | Lswitch jumptbl ->
      let lbl = emit_label (new_label()) in
      (* rax and rdx are clobbered by the Lswitch,
         meaning that no variable that is live across the Lswitch
         is assigned to rax or rdx.  However, the argument to Lswitch
         can still be assigned to one of these two registers, so
         we must be careful not to clobber it before use. *)
      let (tmp1, tmp2) =
        if i.arg.(0).loc = Reg 0 (* rax *)
        then (phys_reg 4 (*rdx*), phys_reg 0 (*rax*))
        else (phys_reg 0 (*rax*), phys_reg 4 (*rdx*)) in

      I.lea (mem64_rip NONE lbl) (reg tmp1);
      I.movsxd (mem64 DWORD 0 (arg64 i 0) ~scale:4 ~base:(reg64 tmp1))
               (reg tmp2);
      I.add (reg tmp2) (reg tmp1);
      I.jmp (reg tmp1);

      begin match system with
      | S_mingw64 | S_cygwin -> D.section [".rdata"] (Some "dr") []
      | S_macosx | S_win64 -> () (* with LLVM/OS X and MASM, use the text segment *)
      | _ -> D.section [".rodata"] None []
      end;
      D.align 4;
      _label lbl;
      for i = 0 to Array.length jumptbl - 1 do
        D.long (ConstSub (ConstLabel(emit_label jumptbl.(i)),
                         ConstLabel lbl))
      done;
      D.text ()
  | Lsetuptrap lbl ->
      I.call (label lbl)
  | Lpushtrap ->
      cfi_adjust_cfa_offset 8;
      I.push r14;
      cfi_adjust_cfa_offset 8;
      I.mov rsp r14;
      stack_offset := !stack_offset + 16
  | Lpoptrap ->
      I.pop r14;
      cfi_adjust_cfa_offset (-8);
      I.add (int 8) rsp;
      cfi_adjust_cfa_offset (-8);
      stack_offset := !stack_offset - 16
  | Lraise k ->
      (* No Spacetime instrumentation is required for [caml_raise_exn] and
         [caml_reraise_exn].  The only function called that might affect the
         trie is [caml_stash_backtrace], and it does not. *)
      begin match k with
      | Cmm.Raise_withtrace ->
          emit_call "caml_raise_exn";
          record_frame Reg.Set.empty true i.dbg
      | Cmm.Raise_notrace ->
          I.mov r14 rsp;
          I.pop r14;
          I.ret ()
      end

let rec emit_all fallthrough i =
  match i.desc with
  | Lend -> ()
  | _ ->
      emit_instr fallthrough i;
      emit_all (Linearize.has_fallthrough i.desc) i.next

(* Emission of the profiling prelude *)

let emit_profile () =
  if system = S_gnu || system = S_linux then begin
    (* mcount preserves rax, rcx, rdx, rsi, rdi, r8, r9 explicitly
       and rbx, rbp, r12-r15 like all C functions.  This includes
       all the registers used for argument passing, so we don't
       need to preserve other regs.  We do need to initialize rbp
       like mcount expects it, though. *)
    I.push r10;
    if not fp then I.mov rsp rbp;
    (* No Spacetime instrumentation needed: [mcount] cannot call anything
       OCaml-related. *)
    emit_call "mcount";
    I.pop r10
  end

let all_functions = ref []

(* Emission of a function declaration *)

let fundecl fundecl =
  function_name := fundecl.fun_name;
  fastcode_flag := fundecl.fun_fast;
  tailrec_entry_point := new_label();
  stack_offset := 0;
  call_gc_sites := [];
  bound_error_sites := [];
  bound_error_call := 0;
  all_functions := fundecl :: !all_functions;
  D.text ();
  D.align 16;
  add_def_symbol fundecl.fun_name;
  if system = S_macosx
  && not !Clflags.output_c_object
  && is_generic_function fundecl.fun_name
  then (* PR#4690 *)
    D.private_extern (emit_symbol fundecl.fun_name)
  else
    D.global (emit_symbol fundecl.fun_name);
  D.label (emit_symbol fundecl.fun_name);
  emit_debug_info fundecl.fun_dbg;
  cfi_startproc ();
  if fp then begin
    I.push rbp;
    cfi_adjust_cfa_offset 8;
    I.mov rsp rbp;
  end;
  if !Clflags.gprofile then emit_profile();
  if frame_required() then begin
    let n = frame_size() - 8 - (if fp then 8 else 0) in
    if n <> 0
    then begin
      I.sub (int n) rsp;
      cfi_adjust_cfa_offset n;
    end;
  end;
  def_label !tailrec_entry_point;
  emit_all true fundecl.fun_body;
  List.iter emit_call_gc !call_gc_sites;
  emit_call_bound_errors ();
  if frame_required() then begin
    let n = frame_size() - 8 - (if fp then 8 else 0) in
    if n <> 0
    then begin
      cfi_adjust_cfa_offset (-n);
    end;
  end;
  cfi_endproc ();
  begin match system with
  | S_gnu | S_linux ->
      D.type_ (emit_symbol fundecl.fun_name) "@function";
      D.size (emit_symbol fundecl.fun_name)
        (ConstSub (
            ConstThis,
            ConstLabel (emit_symbol fundecl.fun_name)))
  | _ -> ()
  end

(* Emission of data *)

let emit_item = function
  | Cglobal_symbol s -> D.global (emit_symbol s)
  | Cdefine_symbol s -> add_def_symbol s; _label (emit_symbol s)
  | Cint8 n -> D.byte (const n)
  | Cint16 n -> D.word (const n)
  | Cint32 n -> D.long (const_nat n)
  | Cint n -> D.qword (const_nat n)
  | Csingle f -> D.long  (Const (Int64.of_int32 (Int32.bits_of_float f)))
  | Cdouble f -> D.qword (Const (Int64.bits_of_float f))
  | Csymbol_address s -> add_used_symbol s; D.qword (ConstLabel (emit_symbol s))
  | Cstring s -> D.bytes s
  | Cskip n -> if n > 0 then D.space n
  | Calign n -> D.align n

let data l =
  D.data ();
  List.iter emit_item l

(* Beginning / end of an assembly file *)

let begin_assembly() =
  X86_proc.reset_asm_code ();
  reset_debug_info();                   (* PR#5603 *)
  reset_imp_table();
  float_constants := [];
  all_functions := [];
  if system = S_win64 then begin
    D.extrn "caml_young_ptr" QWORD;
    D.extrn "caml_young_limit" QWORD;
    D.extrn "caml_exception_pointer" QWORD;
    D.extrn "caml_call_gc" NEAR;
    D.extrn "caml_c_call" NEAR;
    D.extrn "caml_allocN" NEAR;
    D.extrn "caml_alloc1" NEAR;
    D.extrn "caml_alloc2" NEAR;
    D.extrn "caml_alloc3" NEAR;
    D.extrn "caml_ml_array_bound_error" NEAR;
    D.extrn "caml_raise_exn" NEAR;
  end;


  if !Clflags.dlcode || Arch.win64 then begin
    (* from amd64.S; could emit these constants on demand *)
    begin match system with
    | S_macosx -> D.section ["__TEXT";"__literal16"] None ["16byte_literals"]
    | S_mingw64 | S_cygwin -> D.section [".rdata"] (Some "dr") []
    | S_win64 -> D.data ()
    | _ -> D.section [".rodata.cst8"] (Some "a") ["@progbits"]
    end;
    D.align 16;
    _label (emit_symbol "caml_negf_mask");
    D.qword (Const 0x8000000000000000L);
    D.qword (Const 0L);
    D.align 16;
    _label (emit_symbol "caml_absf_mask");
    D.qword (Const 0x7FFFFFFFFFFFFFFFL);
    D.qword (Const 0xFFFFFFFFFFFFFFFFL);
  end;

  D.data ();
  emit_global_label "data_begin";

  D.text ();
  emit_global_label "code_begin";
  if system = S_macosx then I.nop (); (* PR#4690 *)
  ()

let emit_spacetime_shapes () =
  D.data ();
  D.align 8;
  emit_global_label "spacetime_shapes";
  List.iter (fun fundecl ->
      (* CR-someday mshinwell: some of this should be platform independent *)
      begin match fundecl.fun_spacetime_shape with
      | None -> ()
      | Some shape ->
        let funsym = emit_symbol fundecl.fun_name in
        D.comment ("Shape for " ^ funsym ^ ":");
        D.qword (ConstLabel funsym);
        List.iter (fun (part_of_shape, label) ->
            let tag =
              match part_of_shape with
              | Direct_call_point _ -> 1
              | Indirect_call_point -> 2
              | Allocation_point -> 3
            in
            D.qword (Const (Int64.of_int tag));
            D.qword (ConstLabel (emit_label label));
            begin match part_of_shape with
            | Direct_call_point { callee; } ->
              D.qword (ConstLabel (emit_symbol callee))
            | Indirect_call_point -> ()
            | Allocation_point -> ()
            end)
          shape;
          D.qword (Const 0L)
      end)
    !all_functions;
  D.qword (Const 0L);
  D.comment "End of Spacetime shapes."

let end_assembly() =
  if !float_constants <> [] then begin
    begin match system with
    | S_macosx -> D.section ["__TEXT";"__literal8"] None ["8byte_literals"]
    | S_mingw64 | S_cygwin -> D.section [".rdata"] (Some "dr") []
    | S_win64 -> D.data ()
    | _ -> D.section [".rodata.cst8"] (Some "a") ["@progbits"]
    end;
    List.iter (fun (cst,lbl) -> emit_float_constant cst lbl) !float_constants
  end;

  D.text ();
  if system = S_macosx then I.nop ();
  (* suppress "ld warning: atom sorting error" *)

  emit_global_label "code_end";

  emit_imp_table();

  D.data ();
  D.qword (const 0);  (* PR#6329 *)
  emit_global_label "data_end";
  D.qword (const 0);

  D.align 8;                            (* PR#7591 *)
  emit_global_label "frametable";

  let setcnt = ref 0 in
  emit_frames
    { efa_code_label = (fun l -> D.qword (ConstLabel (emit_label l)));
      efa_data_label = (fun l -> D.qword (ConstLabel (emit_label l)));
      efa_16 = (fun n -> D.word (const n));
      efa_32 = (fun n -> D.long (const_32 n));
      efa_word = (fun n -> D.qword (const n));
      efa_align = D.align;
      efa_label_rel =
        (fun lbl ofs ->
           let c =
             ConstAdd (
               ConstSub(ConstLabel(emit_label lbl), ConstThis),
               const_32 ofs
             ) in
           if system = S_macosx then begin
             incr setcnt;
             let s = Printf.sprintf "L$set$%d" !setcnt in
             D.setvar (s, c);
             D.long (ConstLabel s)
           end else
             D.long c
        );
      efa_def_label = (fun l -> _label (emit_label l));
      efa_string = (fun s -> D.bytes (s ^ "\000"))
    };

  if Config.spacetime then begin
    emit_spacetime_shapes ()
  end;

  if system = S_linux then
    (* Mark stack as non-executable, PR#4564 *)
    D.section [".note.GNU-stack"] (Some "") [ "%progbits" ];

  if system = S_win64 then begin
    D.comment "External functions";
    StringSet.iter
      (fun s ->
         if not (StringSet.mem s !symbols_defined) then
           D.extrn (emit_symbol s) NEAR)
      !symbols_used;
    symbols_used := StringSet.empty;
    symbols_defined := StringSet.empty;
  end;

  let asm =
    if !Emitaux.create_asm_file then
      Some
        (
         (if X86_proc.masm then X86_masm.generate_asm
          else X86_gas.generate_asm) !Emitaux.output_channel
        )
    else
      None
  in
  X86_proc.generate_code asm

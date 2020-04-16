type token =
  | Tident of (string)
  | Tchar of (int)
  | Tstring of (string)
  | Taction of (Syntax.location)
  | Trule
  | Tparse
  | Tparse_shortest
  | Tand
  | Tequal
  | Tend
  | Tor
  | Tunderscore
  | Teof
  | Tlbracket
  | Trbracket
  | Trefill
  | Tstar
  | Tmaybe
  | Tplus
  | Tlparen
  | Trparen
  | Tcaret
  | Tdash
  | Tlet
  | Tas
  | Thash

open Parsing;;
let _ = parse_error;;
# 19 "parser.mly"
open Syntax

(* Auxiliaries for the parser. *)

let named_regexps =
  (Hashtbl.create 13 : (string, regular_expression) Hashtbl.t)

let regexp_for_string s =
  let rec re_string n =
    if n >= String.length s then Epsilon
    else if succ n = String.length s then
      Characters (Cset.singleton (Char.code s.[n]))
    else
      Sequence
        (Characters(Cset.singleton (Char.code s.[n])),
         re_string (succ n))
  in re_string 0

let rec remove_as = function
  | Bind (e,_) -> remove_as e
  | Epsilon|Eof|Characters _ as e -> e
  | Sequence (e1, e2) -> Sequence (remove_as e1, remove_as e2)
  | Alternative (e1, e2) -> Alternative (remove_as e1, remove_as e2)
  | Repetition e -> Repetition (remove_as e)

let as_cset = function
  | Characters s -> s
  | _ -> raise Cset.Bad

# 62 "parser.ml"
let yytransl_const = [|
  261 (* Trule *);
  262 (* Tparse *);
  263 (* Tparse_shortest *);
  264 (* Tand *);
  265 (* Tequal *);
  266 (* Tend *);
  267 (* Tor *);
  268 (* Tunderscore *);
  269 (* Teof *);
  270 (* Tlbracket *);
  271 (* Trbracket *);
  272 (* Trefill *);
  273 (* Tstar *);
  274 (* Tmaybe *);
  275 (* Tplus *);
  276 (* Tlparen *);
  277 (* Trparen *);
  278 (* Tcaret *);
  279 (* Tdash *);
  280 (* Tlet *);
  281 (* Tas *);
  282 (* Thash *);
    0|]

let yytransl_block = [|
  257 (* Tident *);
  258 (* Tchar *);
  259 (* Tstring *);
  260 (* Taction *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\003\000\003\000\006\000\006\000\004\000\
\004\000\005\000\005\000\008\000\008\000\009\000\009\000\011\000\
\011\000\010\000\007\000\007\000\007\000\007\000\007\000\007\000\
\007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
\013\000\012\000\012\000\014\000\014\000\014\000\000\000"

let yylen = "\002\000\
\008\000\001\000\000\000\005\000\000\000\003\000\000\000\002\000\
\000\000\005\000\005\000\002\000\000\000\002\000\003\000\003\000\
\000\000\002\000\001\000\001\000\001\000\001\000\003\000\002\000\
\002\000\002\000\003\000\003\000\002\000\003\000\001\000\003\000\
\001\000\002\000\001\000\003\000\001\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\002\000\039\000\005\000\000\000\000\000\000\000\
\000\000\008\000\000\000\000\000\000\000\000\000\007\000\031\000\
\021\000\022\000\019\000\020\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\024\000\025\000\026\000\000\000\000\000\000\000\012\000\000\000\
\000\000\000\000\000\000\000\000\023\000\000\000\030\000\000\000\
\033\000\032\000\000\000\000\000\000\000\006\000\001\000\036\000\
\000\000\000\000\010\000\017\000\011\000\017\000\018\000\000\000\
\000\000\000\000\016\000"

let yydgoto = "\002\000\
\004\000\005\000\006\000\009\000\015\000\026\000\038\000\025\000\
\059\000\060\000\064\000\029\000\050\000\046\000"

let yysindex = "\002\000\
\014\255\000\000\000\000\000\000\000\000\247\254\032\255\037\255\
\036\255\000\000\040\255\053\255\158\255\059\255\000\000\000\000\
\000\000\000\000\000\000\000\000\003\255\158\255\064\255\059\255\
\062\255\029\255\045\255\070\255\058\255\070\255\009\255\158\255\
\000\000\000\000\000\000\073\255\158\255\084\255\000\000\010\255\
\053\255\069\255\078\255\070\255\000\000\070\255\000\000\084\255\
\000\000\000\000\158\255\144\255\144\255\000\000\000\000\000\000\
\158\255\044\255\000\000\000\000\000\000\000\000\000\000\077\255\
\077\255\158\255\000\000"

let yyrindex = "\000\000\
\026\255\000\000\000\000\000\000\000\000\086\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\040\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\027\255\040\255\
\000\000\082\255\255\254\000\000\000\000\079\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\113\255\000\000\000\000\
\000\000\000\000\000\000\080\255\000\000\090\255\000\000\128\255\
\000\000\000\000\095\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\254\254\
\118\255\000\000\000\000"

let yygindex = "\000\000\
\000\000\067\000\000\000\000\000\066\000\000\000\243\255\084\000\
\056\000\203\255\053\000\000\000\000\000\031\000"

let yytablesize = 178
let yytable = "\023\000\
\037\000\014\000\001\000\062\000\027\000\014\000\007\000\014\000\
\031\000\016\000\017\000\018\000\067\000\037\000\008\000\052\000\
\053\000\003\000\048\000\032\000\019\000\020\000\021\000\051\000\
\028\000\033\000\034\000\035\000\022\000\047\000\003\000\004\000\
\003\000\036\000\037\000\010\000\041\000\011\000\058\000\058\000\
\012\000\003\000\004\000\058\000\016\000\017\000\018\000\063\000\
\013\000\003\000\004\000\030\000\058\000\014\000\032\000\019\000\
\020\000\021\000\044\000\024\000\033\000\034\000\035\000\022\000\
\016\000\017\000\018\000\043\000\036\000\037\000\040\000\027\000\
\045\000\049\000\032\000\019\000\020\000\021\000\055\000\056\000\
\033\000\034\000\035\000\022\000\016\000\017\000\018\000\066\000\
\036\000\037\000\009\000\003\000\042\000\035\000\034\000\019\000\
\020\000\021\000\027\000\027\000\033\000\034\000\035\000\022\000\
\038\000\027\000\054\000\039\000\061\000\037\000\027\000\027\000\
\027\000\027\000\065\000\027\000\029\000\029\000\027\000\027\000\
\027\000\015\000\000\000\029\000\000\000\015\000\000\000\015\000\
\029\000\000\000\000\000\028\000\028\000\029\000\000\000\000\000\
\029\000\029\000\028\000\000\000\000\000\000\000\000\000\028\000\
\016\000\017\000\018\000\000\000\028\000\000\000\000\000\028\000\
\028\000\000\000\057\000\019\000\020\000\021\000\016\000\017\000\
\018\000\000\000\000\000\022\000\000\000\000\000\000\000\000\000\
\000\000\019\000\020\000\021\000\000\000\000\000\000\000\000\000\
\000\000\022\000"

let yycheck = "\013\000\
\002\001\004\001\001\000\057\000\002\001\008\001\016\001\010\001\
\022\000\001\001\002\001\003\001\066\000\015\001\024\001\006\001\
\007\001\004\001\032\000\011\001\012\001\013\001\014\001\037\000\
\022\001\017\001\018\001\019\001\020\001\021\001\005\001\005\001\
\004\001\025\001\026\001\004\001\008\001\001\001\052\000\053\000\
\005\001\016\001\016\001\057\000\001\001\002\001\003\001\004\001\
\009\001\024\001\024\001\021\000\066\000\001\001\011\001\012\001\
\013\001\014\001\028\000\001\001\017\001\018\001\019\001\020\001\
\001\001\002\001\003\001\023\001\025\001\026\001\009\001\002\001\
\015\001\001\001\011\001\012\001\013\001\014\001\010\001\002\001\
\017\001\018\001\019\001\020\001\001\001\002\001\003\001\011\001\
\025\001\026\001\005\001\010\001\026\000\015\001\015\001\012\001\
\013\001\014\001\004\001\005\001\017\001\018\001\019\001\020\001\
\015\001\011\001\041\000\024\000\053\000\026\001\016\001\017\001\
\018\001\019\001\062\000\021\001\004\001\005\001\024\001\025\001\
\026\001\004\001\255\255\011\001\255\255\008\001\255\255\010\001\
\016\001\255\255\255\255\004\001\005\001\021\001\255\255\255\255\
\024\001\025\001\011\001\255\255\255\255\255\255\255\255\016\001\
\001\001\002\001\003\001\255\255\021\001\255\255\255\255\024\001\
\025\001\255\255\011\001\012\001\013\001\014\001\001\001\002\001\
\003\001\255\255\255\255\020\001\255\255\255\255\255\255\255\255\
\255\255\012\001\013\001\014\001\255\255\255\255\255\255\255\255\
\255\255\020\001"

let yynames_const = "\
  Trule\000\
  Tparse\000\
  Tparse_shortest\000\
  Tand\000\
  Tequal\000\
  Tend\000\
  Tor\000\
  Tunderscore\000\
  Teof\000\
  Tlbracket\000\
  Trbracket\000\
  Trefill\000\
  Tstar\000\
  Tmaybe\000\
  Tplus\000\
  Tlparen\000\
  Trparen\000\
  Tcaret\000\
  Tdash\000\
  Tlet\000\
  Tas\000\
  Thash\000\
  "

let yynames_block = "\
  Tident\000\
  Tchar\000\
  Tstring\000\
  Taction\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 7 : 'header) in
    let _2 = (Parsing.peek_val __caml_parser_env 6 : 'named_regexps) in
    let _3 = (Parsing.peek_val __caml_parser_env 5 : 'refill_handler) in
    let _5 = (Parsing.peek_val __caml_parser_env 3 : 'definition) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'other_definitions) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : 'header) in
    Obj.repr(
# 73 "parser.mly"
        ( {header = _1;
           refill_handler = _3;
           entrypoints = _5 :: List.rev _6;
           trailer = _7} )
# 248 "parser.ml"
               : Syntax.lexer_definition))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Syntax.location) in
    Obj.repr(
# 80 "parser.mly"
        ( _1 )
# 255 "parser.ml"
               : 'header))
; (fun __caml_parser_env ->
    Obj.repr(
# 82 "parser.mly"
        ( { loc_file = ""; start_pos = 0; end_pos = 0; start_line = 1;
            start_col = 0 } )
# 262 "parser.ml"
               : 'header))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'named_regexps) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'regexp) in
    Obj.repr(
# 87 "parser.mly"
        ( Hashtbl.add named_regexps _3 _5 )
# 271 "parser.ml"
               : 'named_regexps))
; (fun __caml_parser_env ->
    Obj.repr(
# 89 "parser.mly"
        ( () )
# 277 "parser.ml"
               : 'named_regexps))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'other_definitions) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'definition) in
    Obj.repr(
# 93 "parser.mly"
        ( _3::_1 )
# 285 "parser.ml"
               : 'other_definitions))
; (fun __caml_parser_env ->
    Obj.repr(
# 95 "parser.mly"
        ( [] )
# 291 "parser.ml"
               : 'other_definitions))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Syntax.location) in
    Obj.repr(
# 98 "parser.mly"
                    ( Some _2 )
# 298 "parser.ml"
               : 'refill_handler))
; (fun __caml_parser_env ->
    Obj.repr(
# 99 "parser.mly"
              ( None )
# 304 "parser.ml"
               : 'refill_handler))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'arguments) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'entry) in
    Obj.repr(
# 103 "parser.mly"
        ( {name=_1 ; shortest=false ; args=_2 ; clauses=_5} )
# 313 "parser.ml"
               : 'definition))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'arguments) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'entry) in
    Obj.repr(
# 105 "parser.mly"
        ( {name=_1 ; shortest=true ; args=_2 ; clauses=_5} )
# 322 "parser.ml"
               : 'definition))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'arguments) in
    Obj.repr(
# 109 "parser.mly"
                            ( _1::_2 )
# 330 "parser.ml"
               : 'arguments))
; (fun __caml_parser_env ->
    Obj.repr(
# 110 "parser.mly"
                            ( [] )
# 336 "parser.ml"
               : 'arguments))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'case) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'rest_of_entry) in
    Obj.repr(
# 116 "parser.mly"
        ( _1::List.rev _2 )
# 344 "parser.ml"
               : 'entry))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'case) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'rest_of_entry) in
    Obj.repr(
# 118 "parser.mly"
        ( _2::List.rev _3 )
# 352 "parser.ml"
               : 'entry))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'rest_of_entry) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'case) in
    Obj.repr(
# 123 "parser.mly"
        ( _3::_1 )
# 360 "parser.ml"
               : 'rest_of_entry))
; (fun __caml_parser_env ->
    Obj.repr(
# 125 "parser.mly"
        ( [] )
# 366 "parser.ml"
               : 'rest_of_entry))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'regexp) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Syntax.location) in
    Obj.repr(
# 129 "parser.mly"
        ( (_1,_2) )
# 374 "parser.ml"
               : 'case))
; (fun __caml_parser_env ->
    Obj.repr(
# 133 "parser.mly"
        ( Characters Cset.all_chars )
# 380 "parser.ml"
               : 'regexp))
; (fun __caml_parser_env ->
    Obj.repr(
# 135 "parser.mly"
        ( Eof )
# 386 "parser.ml"
               : 'regexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 137 "parser.mly"
        ( Characters (Cset.singleton _1) )
# 393 "parser.ml"
               : 'regexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 139 "parser.mly"
        ( regexp_for_string _1 )
# 400 "parser.ml"
               : 'regexp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'char_class) in
    Obj.repr(
# 141 "parser.mly"
        ( Characters _2 )
# 407 "parser.ml"
               : 'regexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'regexp) in
    Obj.repr(
# 143 "parser.mly"
        ( Repetition _1 )
# 414 "parser.ml"
               : 'regexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'regexp) in
    Obj.repr(
# 145 "parser.mly"
        ( Alternative(Epsilon, _1) )
# 421 "parser.ml"
               : 'regexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'regexp) in
    Obj.repr(
# 147 "parser.mly"
        ( Sequence(Repetition (remove_as _1), _1) )
# 428 "parser.ml"
               : 'regexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'regexp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'regexp) in
    Obj.repr(
# 149 "parser.mly"
        (
          let s1 = as_cset _1
          and s2 = as_cset _3 in
          Characters (Cset.diff s1 s2)
        )
# 440 "parser.ml"
               : 'regexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'regexp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'regexp) in
    Obj.repr(
# 155 "parser.mly"
        ( Alternative(_1,_3) )
# 448 "parser.ml"
               : 'regexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'regexp) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'regexp) in
    Obj.repr(
# 157 "parser.mly"
        ( Sequence(_1,_2) )
# 456 "parser.ml"
               : 'regexp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'regexp) in
    Obj.repr(
# 159 "parser.mly"
        ( _2 )
# 463 "parser.ml"
               : 'regexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 161 "parser.mly"
        ( try
            Hashtbl.find named_regexps _1
          with Not_found ->
            let p = Parsing.symbol_start_pos () in
            Printf.eprintf "File \"%s\", line %d, character %d:\n\
                             Reference to unbound regexp name `%s'.\n"
                           p.Lexing.pos_fname p.Lexing.pos_lnum
                           (p.Lexing.pos_cnum - p.Lexing.pos_bol)
                           _1;
            exit 2 )
# 479 "parser.ml"
               : 'regexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'regexp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'ident) in
    Obj.repr(
# 172 "parser.mly"
        (let p1 = Parsing.rhs_start_pos 3
         and p2 = Parsing.rhs_end_pos 3 in
         let p = {
           loc_file = p1.Lexing.pos_fname ;
           start_pos = p1.Lexing.pos_cnum ;
           end_pos = p2.Lexing.pos_cnum ;
           start_line = p1.Lexing.pos_lnum ;
           start_col = p1.Lexing.pos_cnum - p1.Lexing.pos_bol ; } in
         Bind (_1, (_3, p)))
# 495 "parser.ml"
               : 'regexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 184 "parser.mly"
         (_1)
# 502 "parser.ml"
               : 'ident))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'char_class1) in
    Obj.repr(
# 189 "parser.mly"
        ( Cset.complement _2 )
# 509 "parser.ml"
               : 'char_class))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'char_class1) in
    Obj.repr(
# 191 "parser.mly"
        ( _1 )
# 516 "parser.ml"
               : 'char_class))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 195 "parser.mly"
        ( Cset.interval _1 _3 )
# 524 "parser.ml"
               : 'char_class1))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 197 "parser.mly"
        ( Cset.singleton _1 )
# 531 "parser.ml"
               : 'char_class1))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'char_class1) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'char_class1) in
    Obj.repr(
# 199 "parser.mly"
        ( Cset.union _1 _2 )
# 539 "parser.ml"
               : 'char_class1))
(* Entry lexer_definition *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let lexer_definition (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Syntax.lexer_definition)
;;

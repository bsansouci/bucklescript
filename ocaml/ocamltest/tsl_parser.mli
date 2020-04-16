type token =
  | TSL_BEGIN_C_STYLE
  | TSL_END_C_STYLE
  | TSL_BEGIN_OCAML_STYLE
  | TSL_END_OCAML_STYLE
  | COMA
  | TEST_DEPTH of (int)
  | EQUAL
  | INCLUDE
  | WITH
  | IDENTIFIER of (string)
  | STRING of (string)

val tsl_block :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Tsl_ast.tsl_block

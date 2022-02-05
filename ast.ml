type ident = string
module IdSet = Set.Make(String)
module IdMap = Map.Make(String)

type loc = Lexing.position * Lexing.position

type 'a desc = { loc : loc ; desc : 'a}

type ctype = Int | Struct of ident
type ctype_typed = Typenull | Void | CT of ctype

let str_of_ctype = function
  | Int -> "int" | Struct id -> "Struct " ^ id

let str_of_ctype_typed = function
  | Typenull -> "typenull"
  | Void -> "void*"
  | CT ct -> str_of_ctype ct

type unop = Unot | Uneg 

type binop =
    | Bassign (* = *)
    | Badd | Bsub | Bmul | Bdiv | Bmod    (* + - * / % *)
    | Beq | Bneq | Blt | Ble | Bgt | Bge  (* == != < <= > >= *)
    | Band | Bor      (* && || *)


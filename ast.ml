type ident = string

type loc = Lexing.position * Lexing.position

type 'a desc = { loc : loc ; desc : 'a}

type ctype = Int | Struct of ident
type ctype_typed = Typenull | ctype

type unop = Unot | Uneg 

type binop =
    | Bassign (* = *)
    | Badd | Bsub | Bmul | Bdiv | Bmod    (* + - * / % *)
    | Beq | Bneq | Blt | Ble | Bgt | Bge  (* == != < <= > >= *)
    | Band | Bor      (* && || *)


open Ast

type instr_binop = 
     |Iadd | Isub |Iimul |Idiv|Imod |Imov |Isetl |Isetle
     |Iand |Ior
     (* Les opérateurs binaires de x86-64 *)
type instr_unop =
  |IUadd of int  (* Ajoute l'operande immediate n à son argument *)
  |IUsub of int (* Soustrait n à l'argument *)
  |Inot
  |Ineg
      (* Les opérateurs unaires de x86-64 *)
     

type selec_expr = 
  | Sel_Eint of int
  | Sel_Eident of ident
  | Sel_Eassign of ident * selec_expr
  | Sel_load of int * selec_expr
  | Sel_store of int*selec_expr * selec_expr (* Assigne la valuer du deuxieme au champ int du premier *)
  | Sel_binop of instr_binop * selec_expr * selec_expr
  | Sel_unop of instr_unop * selec_expr
  | Sel_call of ident* selec_expr list

type selec_instruction =
  | Sel_Inil
  | Sel_Iexpr of selec_expr
  | Sel_Iif of selec_expr * selec_instruction * selec_instruction
  | Sel_Iwhile of selec_expr * selec_instruction
  | Sel_Ireturn of selec_expr 
  | Sel_block of selec_instruction list

type selec_stmt =
  | Sel_decl_var of ident
  | Sel_instr of selec_instruction

type selec_decl = 
  |Sel_decl_fun of selec_fun_declaration

and selec_fun_declaration = 
  {
     selec_fun_name : ident;
     selec_fun_args : ident list;
     selec_fun_body : selec_stmt list; 
  }

and selec_program = selec_decl list


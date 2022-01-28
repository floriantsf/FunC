open Ast

type ty_decl_vars =
  | Ty_dv_Int of ident
  | Ty_dv_Sr of ident * ident (* var's ident, type's ident *)

(* We didn't need decl_type anymore *)

type ty_expr = 
  | Ty_Eint of int
  | Ty_Eident of ident
  | Ty_Eunop of unop * ty_expr
  | Ty_Ebinop of binop * ty_expr * ty_expr
  | Ty_Ecall of ident * (ty_expr list)
  | Ty_Ept of expr * int
 
type ty_stmt =
  | Ty_Snil
  | Ty_Sexpr of ty_expr
  | Ty_Sif of ty_expr * ty_stmt * ty_stmt
  | Ty_Swhile of ty_expr * ty_stmt
  | Ty_Sbloc of ty_bloc
  | Ty_Sreturn of ty_expr

and ty_bloc_un = 
  | Ty_Bdv of ty_decl_vars
  | Ty_Bstmt of ty_stmt
and ty_bloc = ty_bloc_un list
   
type ty_decl_fct =
{ id : ident ;
  params : ident list ;
  body : ty_bloc }

type ty_all =
{ sr_size_tab : (ident , int) Hashtbl.t ;
  fcts : ty_decl_fct list 
}
  


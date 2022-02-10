                                                                     
(* types produced during typing and used during ??? *)

open Ast
exception Typing_error of {loc : loc ; msg : string}

(* dv : decl_vars ; dt : decl_typ, ; df : decl_fct *)

(* We didn't need decl_type anymore *)

type ty_expr = 
  | Ty_Eint of int
  | Ty_Eident of ident
  | Ty_Eunop of unop * ty_expr
  | Ty_Ebinop of binop * ty_expr * ty_expr
  | Ty_Eassign_var of ident * ty_expr
  | Ty_Eassign_ch of ty_expr * int * ty_expr
  | Ty_Ecall of ident * (ty_expr list)
  | Ty_Ept of ty_expr * int
 
type ty_stmt =
  | Ty_Sdv of ident
  | Ty_Snil
  | Ty_Sexpr of ty_expr
  | Ty_Sif of ty_expr * ty_stmt * ty_stmt
  | Ty_Swhile of ty_expr * ty_stmt
  | Ty_Sbloc of ty_stmt list
  | Ty_Sreturn of ty_expr

type ty_df =
{ id : ident ;
  params : ident list ;
  body : ty_stmt list }

type ty_file = ty_df list
 


(* === Others things used to type === *)

type tab_champs = (ident , (ctype * int)) Hashtbl.t
type env_sr = (ident , (tab_champs * int)) Hashtbl.t

type statut = Param | Global of int
type ty_info_var = { typ : ctype ; statut : statut}
type env_vars = ty_info_var IdMap.t

type ty_info_fct = 
{ type_r : ctype_typed ; 
  type_params : ctype list }
type env_fct = (ident , ty_info_fct ) Hashtbl.t

type ty_env = { mutable env_v : env_vars ; env_s : env_sr ; env_f : env_fct ; depth : int }

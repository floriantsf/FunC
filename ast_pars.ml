
(* types produced during parsing and used during typing *)

open Ast
exception Parser_error of string

(* dv : decl_vars ; dt : decl_typ, ; df : decl_fct ; dfl : decl_field *)

type par_expr =
  | Par_Eint of int
  | Par_Eident of ident
  | Par_Eunop of unop * (par_expr desc)
  | Par_Ebinop of binop * (par_expr desc) * (par_expr desc)
  | Par_Ecall of (ident desc) * (par_expr desc list)
  | Par_Ept of (par_expr desc) * (ident desc)
  | Par_Esize of ident desc

type par_dv = 
{ typ : ctype desc ; 
  vars_expr : ((ident desc) * (par_expr desc option)) list }
type par_stmt =
  | Par_Sdv of par_dv
  | Par_Snil
  | Par_Sexpr of par_expr desc
  | Par_Sif of (par_expr desc) * par_stmt * par_stmt
  | Par_Swhile of (par_expr desc) * par_stmt
  | Par_Sbloc of par_stmt list
  | Par_Sreturn of par_expr desc

type par_param = {typ : ctype desc ; nom : ident desc}

type par_dfl = {typ : ctype desc ; vars : ident desc list}
type par_dt = {nom : ident desc ; fields : par_dfl list}

type par_df = 
{ type_r : ctype desc ; 
  nom : ident desc ;
  params : par_param list ;
  body : par_stmt list }

type par_decl = 
  | Par_Ddt of par_dt
  | Par_Ddf of par_df

type par_file = par_decl list

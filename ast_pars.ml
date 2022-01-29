
(* types produced during parsing and used during typing *)

open Ast
exception Parser_error of string

(* dv : decl_vars ; dt : decl_typ, ; df : decl_fct *)

type par_expr =
  | Par_Eint of int
  | Par_Eident of ident
  | Par_Eunop of unop * (par_expr desc)
  | Par_Ebinop of binop * (par_expr desc) * (par_expr desc)
  | Par_Ecall of (ident desc) * (par_expr desc list)
  | Par_Ept of (expr desc) * (ident desc)
  | Par_Esize of ident desc

type par_stmt =
  | Par_Snil
  | Par_Sexpr of par_expr
  | Par_Sif of (par_expr desc) * (par_stmt desc) * (par_stmt desc)
  | Par_Swhile of (par_expr desc) * (par_stmt desc)
  | Par_Sbloc of par_bloc
  | Par_Sreturn of par_expr desc

and par_bloc_un =
  | Par_Bdv of par_dv
  | Par_Bstmt of par_stmt
and par_bloc = par_bloc_un desc list

type par_param = {typ : ctype desc ; nom : ident desc}

type par_dv = {typ : ctype desc ; vars : ident desc list}

type par_dt = {nom : ident desc ; fields : par_dv desc list}

type par_df = 
{ type_r : ctype desc ; 
  params : par_param desc list ;
  body : bloc desc }

type par_decl = 
  | Par_Ddt of par_dt
  | Par_Ddf of par_df

type par_file = par_decl desc list

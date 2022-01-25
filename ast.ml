type ident = string

type loc = Lexing.position * Lexing.position

type typ = Typenull | Int | Struct of ident

type unop = 
  |Unot  
  |Uneg 

type binop =
    | Bassign
    | Badd | Bsub | Bmul | Bdiv | Bmod    (* + - * / % *)
    | Beq | Bneq | Blt | Ble | Bgt | Bge  (* == != < <= > >= *)
    | Band | Bor      (* && || *)


type loc_file = {loc_decls : loc_decl list}
and 
  loc_decl = DeclTyp of loc_decl_typ | DeclFct of loc_decl_fct
and
  loc_decl_vars = VarsInt of loc_vars_int 
              | VarsStruct of loc_vars_struct 
and
  loc_vars_int = {
  names : ident list;  
  loc_vars_int : loc;
}
and 
loc_vars_struct = {
  name_struct_vars : ident;
  names_vars : ident list;
  loc_vars_struct : loc;
}
and 
loc_decl_typ = {
  name_struct_decl_typ : ident;
  struct_vars : loc_decl_vars list;  
  loc_decl_typ : loc;
  }
and loc_decl_fct ={
  name_fct : ident;
  typ_name_fct : typ;
  decl_fct_args : loc_param_fct list;
  body : loc_block ;
  loc_decl_fct : loc;
}
and loc_param_fct ={
  typ_loc_param : typ;
  name_loc_param : ident;
  loc_param_fct : loc;
}
and loc_expr_node = 
  | LInt of int
  | LIdent of ident
  | LPoint of loc_expr * ident
  | LCall of ident * ( loc_expr list)
  | LUnop of unop * loc_expr 
  | LBinop of binop * loc_expr * loc_expr
  | LSizeof of ident
and loc_expr = 
  {
    expr_node : loc_expr_node;
    loc_expr : loc;
  }
and loc_stmt_node = 
  | LSvoid 
  | LSeval of loc_expr
  | LSif of loc_expr * loc_stmt * loc_stmt 
  | LSwhile of loc_expr * loc_stmt 
  | LSblock of loc_block
  | LSreturn of loc_expr
and loc_stmt = 
  {
    stmt_node : loc_stmt_node;
    loc_stmt : loc;
  }
and loc_block = 
  {
    loc_block_decl_vars : loc_decl_vars list;
    loc_block_stmt : loc_stmt list;
    loc_block : loc;
  }

type ident = string

type loc = Lexing.position * Lexing.position

type loc_file = {decls : decl_loc list}
and 
  decl = DeclTyp of loc_decl_typ | DeclFct of loc_decl_fct
and
  decl_vars = VarsInt of loc_vars_int 
              | VarsStruct of loc_vars_struct 
and
  loc_vars_int = {
  names : ident list;  
  loc_vars_int : loc;
}
and 
  loc_vars_struct = {
  name_struct : ident;
  names_vars : ident list;
  loc_vars_struct : loc;
}

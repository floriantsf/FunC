open Ast


type typ_file = {typ_decls : typ_decl list}
and typ_decl = TDeclTyp of typ_decl_typ | TDeclFct of typ_decl_fct
and typ_decl_vars = TVarsInt of typ_vars_int
                  | TVarsStruct of typ_vars_struct
and 
typ_vars_int = ident list
and 
typ_vars_struct = 
  {
    name_struct_vars : ident;
  } 
and 
typ_decl_typ = 
  {
    name_decl_typ : ident;
    struct_vars_typ : typ_decl_vars list; 
    typ_decl_typ : typ;
  }

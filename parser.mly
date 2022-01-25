%{
  open Ast
%}

%token ADD SUB MUL DIV NEG LPAR RPAR MOD NOT 
%token EQ NEQ LT LE GT GE AND OR
%token COMMA DOT ASSIGN SEMICOLON LBRACK RBRACK 
%token EOF 
%token INT STRUCT IF ELSE WHILE RETURN SIZEOF
%token STAR SELECT

%token THEN
%token <int> CONST 
%token <string> IDENT
%token <string> STRING

%right ASSIGN
%left OR
%left AND
%left EQ NEQ
%left LT LE GT GE
%left ADD SUB
%left MUL DIV MOD
%right NOT NEG
%left SELECT

%nonassoc THEN
%nonassoc ELSE


%start file 
%type <Ast.loc_file> file

%%

file : 
  l = decl* ; EOF {{loc_decls = l}}  
;
decl : 
  | dt = decl_typ { DeclTyp dt}
  | fct = decl_fct {DeclFct fct}
;
decl_vars : 
  |INT; l = separated_nonempty_list(COMMA, IDENT) ; SEMICOLON
     {VarsInt {names = l; loc_vars_int = $startpos,$endpos }}
  |STRUCT; n= IDENT; l = separated_nonempty_list(COMMA, STAR; s = IDENT {s}); SEMICOLON {VarsStruct {name_struct_vars = n; names_vars = l; loc_vars_struct = $startpos, $endpos}}
;
decl_typ : 
  |STRUCT; name = IDENT; LBRACK; vars = decl_vars*; RBRACK; SEMICOLON { {name_struct_decl_typ = name; struct_vars = vars; loc_decl_typ = $startpos,$endpos}}
;
decl_fct : 
  |INT; f = IDENT; LPAR; args = separated_list(COMMA, param); RPAR;b = block; {{name_fct = f; typ_name_fct = Int; decl_fct_args = args; body = b; loc_decl_fct =$startpos, $endpos}}
  |STRUCT; s_name = IDENT; STAR; f = IDENT; LPAR; args = separated_list(COMMA, param); RPAR;b = block {{name_fct = f; typ_name_fct = Struct s_name; decl_fct_args = args; body = b; loc_decl_fct = $startpos,$endpos}}
;
param :
  |INT; s = IDENT {{typ_loc_param = Int; name_loc_param = s; loc_param_fct = $startpos,$endpos}}
;
expr : 
  |i = CONST {{expr_node = LInt i; loc_expr = $startpos, $endpos}}
  |s = IDENT {{expr_node = LIdent s; loc_expr = $startpos, $endpos}}
  |e = expr ; SELECT; n = IDENT {{expr_node = LPoint (e, n); loc_expr = $startpos, $endpos}}
  |f  = IDENT ; LPAR ; l = separated_list (COMMA, expr) ;RPAR {{expr_node = LCall (f,l); loc_expr = $startpos,$endpos}}
  |NOT; e = expr {{expr_node = LUnop (Unot, e); loc_expr = $startpos,$endpos}}
  |SUB; e = expr %prec NEG {{expr_node = LUnop (Uneg, e); loc_expr = $startpos, $endpos}}
  |e1 = expr; o = operator ; e2 = expr {{expr_node = LBinop (o, e1, e2); loc_expr = $startpos,$endpos}}
  |SIZEOF ; LPAR; STRUCT; n = IDENT; RPAR {{expr_node = LSizeof n; loc_expr = $startpos,$endpos}}
  |LPAR; e = expr; RPAR {e}
  ;
%inline operator:
  |ASSIGN {Bassign}
  |EQ {Beq}
  |NEQ {Bneq}
  |LT {Blt}
  |LE {Ble}
  |GT {Bgt}
  |GE {Bge}
  |ADD {Badd}
  |SUB {Bsub}
  |MUL {Bmul}
  |DIV {Bdiv}
  |MOD {Bmod}
  |AND {Band}
  |OR {Bor}
  ;
stmt : 
    i = stmt_node {{stmt_node = i; loc_stmt = $startpos, $endpos}}
    ;
stmt_node:
 | SEMICOLON {LSvoid}
 | e = expr ; SEMICOLON {LSeval e}
 | IF; LPAR; e = expr ; RPAR; i = stmt %prec THEN {LSif (e, i , {stmt_node = LSvoid; loc_stmt = Lexing.dummy_pos, Lexing.dummy_pos})} 
 | IF; LPAR; e = expr; RPAR; i1 = stmt; ELSE ; i2 = stmt {LSif (e, i1, i2)}
 |WHILE ; LPAR; e = expr; RPAR ; i = stmt {LSwhile (e,i)}
 |b = block {LSblock b}
 |RETURN; e = expr; SEMICOLON {LSreturn e}
 ;
block : 
  LBRACK; l1 = decl_vars*; l2 = stmt*; RBRACK {{loc_block_decl_vars = l1; loc_block_stmt = l2; loc_block = $startpos, $endpos}}
%%



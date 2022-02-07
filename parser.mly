%{
  open Ast
  open Ast_pars

  let w d startpos endpos = {desc = d ; loc = (startpos , endpos)}
  let dummy_loc = (Lexing.dummy_pos,Lexing.dummy_pos)
%}

%token ADD SUB MUL DIV NEG LPAR RPAR MOD NOT 
%token EQ NEQ LT LE GT GE AND OR
%token COMMA ASSIGN SEMICOLON LBRACK RBRACK 
%token EOF 
%token INT STRUCT IF ELSE WHILE RETURN SIZEOF
%token STAR SELECT

%token <int> CONST 
%token <string> IDENT

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
%type <Ast_pars.par_file> file

%%

// ------------------------------------------ //
// AUXILIAIRES : 

%inline loc (X): 
  | x = X { {desc = x ; loc = ($startpos, $endpos)} }

ty_int : 
  | INT { Int }
ty_sr  : 
  | STRUCT sr = IDENT { Struct sr }
ctype  : 
  | INT { Int }
  | STRUCT ; sr = IDENT ; STAR { Struct sr }

// ------------------------------------------ //


// ------------------------------------------ //
// PRINCIPALES :

file : 
  | l = decl* ; EOF { l }  

decl : 
  | dt  = decl_typ  { Par_Ddt dt  }
  | fct = decl_fct  { Par_Ddf fct }

// ------------------------------------------ //


// ------------------------------------------ //
// DECLARATION D'UNE STRUCT :

decl_field : 
  | dty = loc(ty_int) ; l = separated_nonempty_list(COMMA, did = loc(IDENT) {did}) ; SEMICOLON
    { {typ = dty ; vars = l} }
  | dty = loc(ty_sr)  ; l = separated_nonempty_list(COMMA, STAR; did = loc(IDENT) {did}) ; SEMICOLON 
    { {typ = dty ; vars = l} }

decl_typ : 
  | STRUCT ; did = loc(IDENT) ; LBRACK ; l = decl_field* ; RBRACK ; SEMICOLON 
    { { nom = did ; fields = l} }

// ------------------------------------------ //


// ------------------------------------------ //
// DECLARATION D'UNE FONCTION :

param :
  | dty = loc(ctype) ; did = loc(IDENT)
    { {typ = dty ; nom = did} }

decl_fct : 
  | dty = loc(ctype) ; did = loc(IDENT) ; LPAR ; args = separated_list(COMMA, param) ; RPAR ; b = bloc
    { {type_r = dty ; nom = did ; params = args ; body = b} }

// ------------------------------------------ //


// ------------------------------------------ //
// LES EXPRESSIONS :

expr : 
  | n = CONST 
    { Par_Eint n }
  | s = IDENT 
    { Par_Eident s }
  | NOT ; de = loc(expr)
    { Par_Eunop (Unot,de) }
  | NEG ; de = loc(expr)
    { Par_Eunop (Uneg,de) }
  | de1 = loc(expr) ; op = operator ; de2 = loc(expr)
    { Par_Ebinop (op , de1 , de2) }
  | did = loc(IDENT) ; LPAR ; l = separated_list (COMMA, loc(expr)) ; RPAR 
    { Par_Ecall (did , l) }
  | de = loc(expr) ; SELECT ; did = loc(IDENT) 
    { Par_Ept (de , did) }
  | SIZEOF ; LPAR ; STRUCT ; did = loc(IDENT) ; RPAR 
    { Par_Esize did }
  | LPAR ; e = expr ; RPAR 
    { e }

%inline operator:
  |ASSIGN {Bassign}
  |EQ     {Beq}
  |NEQ    {Bneq}
  |LT     {Blt}
  |LE     {Ble}
  |GT     {Bgt}
  |GE     {Bge}
  |ADD    {Badd}
  |SUB    {Bsub}
  |MUL    {Bmul}
  |DIV    {Bdiv}
  |MOD    {Bmod}
  |AND    {Band}
  |OR     {Bor}

// ------------------------------------------ //


// ------------------------------------------ //
// LES INSTRUCTIONS :

init_var :
  | did = loc(IDENT) 
    { (did,None) }
  | did = loc(IDENT) ; ASSIGN ; de = loc(expr)
    { (did , Some de) }

decl_var : 
  | dty = loc(ty_int) ; l = separated_nonempty_list(COMMA, init_var) ; SEMICOLON
    { {typ = dty ; vars = l} }
  | dty = loc(ty_sr)  ; l = separated_nonempty_list(COMMA, STAR; x = init_var {x}) ; SEMICOLON 
    { {typ = dty ; vars = l} }

stmt : 
  | dv = decl_var 
    { Par_Sdv dv }
  | SEMICOLON 
    { Par_Snil }
  | de = loc(expr) ; SEMICOLON
    { Par_Sexpr de }
  | IF ; LPAR ; de = loc(expr) ; RPAR ; ds = loc(stmt) %prec THEN
    { Par_Sif (de , ds , {desc = Par_Snil ; loc = dummy_loc }) }
  | IF ; LPAR ; de = loc(expr) ; RPAR ; ds1 = loc(stmt) ; ELSE ; ds2 = loc(stmt) 
    { Par_Sif (de , ds1 , ds2) }
  | WHILE ; LPAR ; de = loc(expr) ; RPAR ; ds = loc(stmt)
    { Par_Swhile (de , ds) }
  | b = bloc
    { Par_Sbloc b }
  | RETURN ; de = loc(expr) ; SEMICOLON 
    { Par_Sreturn de }

bloc : 
  | LBRACK ; l = list(loc(stmt)) ; RBRACK  { l }

// ------------------------------------------ //
%%


%{
  open Ast
%}

%token ADD SUB MUL DIV NEG LPAR RPAR MOD NOT
%token EQ NEQ LT LE GT GE AND OR
%token COMMA DOT ASSIGN SEMICOLON LBRACK RBRACK 
%token EOF 
%token INT STRUCT IF ELSE WHILE RETURN SIZEOF

%right ASSIGN
%left OR
%left AND
%left EQ NEQ
%left LT LE GT GE
%left ADD SUB
%left MUL DIV
%right NOT NEG
%left SELECT

%nonassoc THEN
%nonassoc ELSE


%start file 
%type <Ast.file_loc> file

%%

file : l = decl* ; EOF {{decls = l}}
   ;
decl : 
         decl_typ 
         | decl_fct
   ;
   decl_vars : INT; l = separated_nonempty_list(COMMA, IDENT) ; SEMICOLON{{

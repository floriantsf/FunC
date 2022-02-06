(* Analyseur lexical pour FunC *)


{
open Parser
(*let p_p = Preparser.defs Preprocess.token lexbuf in*)
exception Lexing_error of string
let keywords = Hashtbl.create 20 

let () = List.iter (fun (s,t) -> Hashtbl.add keywords s t) 
    [
      "int" , INT;
      "struct", STRUCT;
      "if", IF;
      "else", ELSE;
      "while", WHILE;
      "return", RETURN;
      "sizeof", SIZEOF;
    ]
  let string_buffer = ref ""
}
 
let alpha = ['a'-'z' 'A'-'Z']
let chiffre = ['0'-'9']
let ident = (alpha | '_') (alpha |chiffre | '_')*
let chiffre_octal = ['0'-'7']
let chiffre_hexa = ['0'-'9' 'a'-'f'  'A'-'F']
let qqconque = [ ^ '"']
            
let entier = '0'
           | ['1'-'9'] chiffre* 
           | '0' (chiffre_octal)+
           | "0x" (chiffre_hexa)+

rule token = parse 
             |ident as s {try Hashtbl.find keywords s with Not_found -> (try (match Hashtbl.find p_p.definitions s
                with VInt i -> CONST i) 
              with Not_found -> IDENT s)}
             | entier as x {CONST (int_of_string x)}
             | "*" {STAR}
             | "=" {ASSIGN}
             | "||" {OR}
             |"&&" {AND}
             |"==" {EQ}
             |"!=" {NEQ}
             |"<" {LT}
             |">" {GT}
             |"<=" {LE}
             |">=" {GE}
             |"+" {ADD}
             |"-" {SUB}
             |"*" {MUL}
             |"/" {DIV}
             |"%" {MOD}
             |"!" {NOT}
             |"->" {SELECT}
             |"{" {LBRACK}
             |"}" {RBRACK}
             |"(" {LPAR}
             |")" {RPAR}
             |"," {COMMA}
             |"\n" {Lexing.new_line lexbuf; token lexbuf}
             |"/*" {comment1 lexbuf}
             |"//" {comment2 lexbuf}
             |'"' {chaine lexbuf}
             |[' ' '\t'] {token lexbuf}
             |eof {EOF}
             |_ {raise (Lexing_error ("Unknown keyword"))}
(*
and check_for_condition = parse
             |"ifdef" {check_ifdef lexbuf}
             |"ifndef" {check_ifndef lexbuf}
             |'\n' {Lexing.new_line lexbuf; token lexbuf}
             |_ {check_for_condition lexbuf}
and check_ifdef = parse 
             |ident as s {if Hashtbl.mem p_p.definitions then token lexbuf else process_condition_false lexbuf}
             |' ' {check_ifdef lexbuf}
             |_ {raise (Lexing_error "no check for definition")}
and check_ifndef = parse 
             |ident as s {if Hashtbl.mem p_p.definitions then process_condition_false lexbuf else token lexbuf}
             |' ' {check_ifdef lexbuf}
             |_ {raise (Lexing_error "error")}
and process_condition_false = parse
             |'#' {check_for_endif lexbuf}
             |'\n' {Lexing.new_line lexbuf; process_condition_false lexbuf}
             |eof { assert false}
             |_ {process_condition_false lexbuf}
and check_for_endif = parse
             |' ' {check_for_endif lexbuf}
             |'\n' {Lexing.new_line lexbuf; process_condition_false}
             |"endif" {token lexbuf}
             |_ {check_for_endif lexbuf}
   *)
and comment1 = parse 
             | "*/" {token lexbuf}
             | "\n" {Lexing.new_line lexbuf; comment1 lexbuf}
             | eof {raise (Lexing_error "Unfinished comment")}
             | _ {comment1 lexbuf}
and comment2 = parse 
             | "\n" {Lexing.new_line lexbuf; token lexbuf}
             | eof {EOF}
             | _ {comment2 lexbuf}
and chaine = parse 
           |'"' {let res = !string_buffer in string_buffer := ""; STRING res} 
           |"\\\"" {string_buffer := !string_buffer ^ Char.escaped '"'; chaine lexbuf} 
           |"\\n" {string_buffer := !string_buffer ^ "\n"; chaine lexbuf} 
           |"\\\\" {string_buffer :=  !string_buffer ^"\\"; chaine lexbuf} 
           |"\\" {raise (Lexing_error "Illegal escape in string")} 
           |"\n" {raise (Lexing_error "Illegal line jump in string")}
           |qqconque as s {string_buffer := !string_buffer ^ Char.escaped s; chaine lexbuf} 
           |eof {raise (Lexing_error "String not finished")}

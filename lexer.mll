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
  let char_error s = raise (Lexing_error ("Illegal character sequence: " ^s))
  let char_to_int s = match String.length s with
  |1 -> Char.code s.[0]
  |2 |4 when s.[0] = '\\' -> 
      begin match s.[1] with
      |'n' -> 10
      |'t' -> 9
      |'\'' -> 39
      |'\"' -> 34 
      |'x' -> int_of_string ("0x"^ String.sub s 2 2)
      |_ -> char_error s
      end
      |_ -> char_error s
}
 
let alpha = ['a'-'z' 'A'-'Z']
let chiffre = ['0'-'9']
let ident = (alpha | '_') (alpha |chiffre | '_')*
let chiffre_octal = ['0'-'7']
let chiffre_hexa = ['0'-'9' 'a'-'f'  'A'-'F']
let qqconque = [ ^ '"']
let char_ascii =
  [^'\000'-'\x1f' '\\' '\'' '\"']
  | '\\' ('n' | 't' | '\'' |'\"')
  | "\\x" chiffre_hexa chiffre_hexa


rule token = parse 
             |ident as s {try Hashtbl.find keywords s with Not_found -> (*(try (match Hashtbl.find p_p.definitions s
                with VInt i -> CONST i) *)
              IDENT s}
             | "0"
                 { CONST 0 }
             | (['1'-'9'] chiffre*) as s
                 { try
               CONST (int_of_string s)
             with _ ->
               raise (Lexing_error ("invalid integer constant '" ^ s ^ "'")) }
             | "0" (chiffre_octal+ as s)
                 { try
               CONST (int_of_string ("0o" ^ s))
             with _ ->
               raise (Lexing_error ("invalid octal constant '" ^ s ^ "'")) }
             | ("0x" chiffre_hexa+) as s
                 { try
               CONST (int_of_string s)
             with _ ->
               raise (Lexing_error ("invalid hexadecimal constant '" ^ s ^ "'")) }
             | '\'' (char_ascii as s) '\''
                 { CONST  (char_to_int s) }
             | "*" {STAR}
             | "=" {ASSIGN}
             | "||" {OR}
             |"&&" {AND}
             |"==" {EQ}
             |"!=" {NEQ}
             |'<' {LT}
             |'>' {GT}
             |"<=" {LE}
             |">=" {GE}
             |'+' {ADD}
             |'-' {SUB}
             |'*' {MUL}
             |'/' {DIV}
             |'%' {MOD}
             |'!' {NOT}
             |"->" {SELECT}
             |'{' {LBRACK}
             |'}' {RBRACK}
             |'(' {LPAR}
             |')' {RPAR}
             |',' {COMMA}
             |';' {SEMICOLON}
             |'\n' {Lexing.new_line lexbuf; token lexbuf}
             |"/*" {comment1 lexbuf}
             |"//" {comment2 lexbuf}
             (*|'"' {chaine lexbuf}*)
             |[' ' '\t'] {token lexbuf}
             |eof {EOF}
             |_ as s {raise (Lexing_error ("Unknown keyword" ^ Char.escaped(s)))}
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
(*
and chaine = parse 
           |'"' {let res = !string_buffer in string_buffer := ""; STRING res} 
           |"\\\"" {string_buffer := !string_buffer ^ Char.escaped '"'; chaine lexbuf} 
           |"\\n" {string_buffer := !string_buffer ^ "\n"; chaine lexbuf} 
           |"\\\\" {string_buffer :=  !string_buffer ^"\\"; chaine lexbuf} 
           |"\\" {raise (Lexing_error "Illegal escape in string")} 
           |"\n" {raise (Lexing_error "Illegal line jump in string")}
           |qqconque as s {string_buffer := !string_buffer ^ Char.escaped s; chaine lexbuf} 
           |eof {raise (Lexing_error "String not finished")}
*)

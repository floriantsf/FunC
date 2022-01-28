(* Analyseur lexical pour le preprocessing *)


{
  open Preparser
  let keywords = Hashtbl.create 7

  let () = List.iter (fun (s,t) -> Hashtbl.add keywords s t)
      [
        "ifndef", IFNDEF;
        "ifdef" , IFDEF;
        "endif" , ENDIF;
        "define", DEFINE;   
      ]
}

let alpha  = ['a'-'z' 'A'-'Z']
let ident = (alpha | '_') (alpha | chiffre | '_')*

rule token = parse 
  |'#' {process lexbuf}
  |'\n' {Lexing.new_line lexbuf; token lexbuf}
  |eof {EOF}
  | _ {token lexbuf}

and process = parse
  |ident as s {try Hashtbl.find keywords s with Not_found -> P_IDENT s}
  |'\n' {Lexing.new_line lexbuf; token lexbuf}


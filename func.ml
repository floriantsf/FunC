open Format
open Lexing


(* Options inutiles au projet *)
(* Option pour s'arreter à l'issue du parser *)
let parse_only = ref false
let type_only = ref false
(* Le fichier souce *)
let ifile = ref ""
let set_file f s = f := s
(* Les options du compilateur *)
let options = ["--type-only", Arg.Set type_only , "Pour s'arreter à l'issue du typage";
                "--parse-only", Arg.Set parse_only, "Pour faire uniquement la phase d'analyse syntaxique";
                ]
let usage = "usage : ./func [options] file.c"
let localisation pos =
  let l = pos.pos_lnum in
  let c = pos.pos_cnum - pos.pos_bol + 1 in
  eprintf "File \"%s\", line %d, characters %d-%d:\n" !ifile l (c-1) c
let localisation_loc l = (* l de type Ast.loc *)
    let p1, p2 = l in
    let l1 = p1.pos_lnum in
    let c1 = p1.pos_cnum - p1.pos_bol +1 in
    let c2 = p2.pos_cnum - p2.pos_bol +1 in
    eprintf "File \"%s\", line %d, characters %d-%d:\n" !ifile l1 c1 c2
let main () =
    Arg.parse options (set_file ifile) usage;
    if !ifile = "" then begin 
        eprintf "Aucun fichier à compiler \n@?";
        exit 1;
    end;

    let f = open_in !ifile in 
    let buf = Lexing.from_channel f in 
    try

        let p = Parser.file Lexer.token buf in (* Pour l'instant on ne donne pas de nom a ce qui a été créé a partir de l'analyse syntaxique *)
        close_in f;
        if !parse_only then exit 0; (* On s'arrete à l'analyse syntaxique dans ce cas *)
        let _ = Typing.ty_file p in        

        if !type_only then exit 0;

        ()
        (*
        (* On fait l'analyse de types *)
        let p = Typer.type_all p in  (* on récupere un nouvel arbre de syntaxe a l'issue du typage *)
        
        
        if !type_only then exit 0;
        let nom_fichier = (List.hd (String.split_on_char '.' !ifile))^".s"  in

        Assemble.compile_program p nom_fichier
           *)
        
      
        
    with
        | Lexer.Lexing_error c ->
                (* On a détéctés une erreur lexicale on la signale et on ferme le programme *)
                (* On prend sa position absolue et on la transforme en nubéro de linge *)
                localisation (Lexing.lexeme_start_p buf);
                eprintf "Erreur lexicale: %s@." c;
                exit 1
        | Parser.Error ->
                (* On détecte une erreur de syntaxe, on la signale et on ferme le programme *)
                localisation (Lexing.lexeme_start_p buf);
                eprintf "Erreur Syntaxique au mot %s@." (Lexing.lexeme buf);
                exit 1

let () = main () 

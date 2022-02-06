

open Ast_sel

let rec mkAdd e1 e2 = match (e1, e2) with (* mkAdd prends en entrées des expressions de ast_sel *)
  |(Sel_Eint n1, Sel_Eint n2) ->  Sel_Eint (n1 + n2)
  |(Sel_Eint 0, e2) ->  e2
  |(e1, Sel_Eint 0) -> e1
  |(Sel_binop (Iadd, Sel_Eint n1, e) , Sel_Eint n2) -> mkAdd (Sel_Eint (n1+n2)) e  
  |(Sel_Eint n, e) -> Sel_unop (IUadd n, e) 
  |(e, Sel_Eint n) -> Sel_unop (IUadd n ,e)
  |_ -> Sel_binop (Iadd, e1, e2) (* Si pas d'optimisation possible *)

let rec mkSub e1 e2 = match (e1, e2) with

let instr_selec e = match e with (* Fait la selection d'instructions *)
  |Ty_Eint n -> Sel_Eint n 
  |Ty_Eident x -> Sel_Eident x
  |Ty_Eunop (op , e) -> assert false
  |Ty_Ebinop (op, e1, e2) -> 
    begin match op with
      |Badd -> mkAdd (instr_selec e1) (instr_selec e2)
      |_ -> assert false
    end
  |Ty_Eassign_var (s, e1) -> 



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
  |(Sel_Eint n1, Sel_Eint n2) ->  Sel_Eint (n1 - n2)
  |(Sel_Eint 0, e2) ->  mkNeg e2
  |(e1, Sel_Eint 0) -> e1
  |(Sel_binop (Iadd, Sel_Eint n1, e) , Sel_Eint n2) -> mkAdd (Sel_Eint (n1-n2)) e  
  (*|(Sel_Eint n, e) -> Sel_unop (IUadd n, e) *)
  |(e, Sel_Eint n) -> Sel_unop (IUsub n ,e)
  |_ -> Sel_binop (Isub, e1, e2) (* Si pas d'optimisation possible *)

let pure e = match e with (* Determine si l'expression est pure *)
  |Ty_Eint _ |Ty_Eident _ -> true
  |Ty_Eunop (_, e1) -> pure e1
  |Ty_Ebinop (_, e1, e2) -> pure e1 && pure e2
  |Ty_Eassign_var _ |Ty_Eassign_ch -> false
  |Ty_Ecall _ -> false
  |Ty_Ept -> true
let rec mkMul e1 e2 = match (e1, e2) with
  |(Sel_Eint n1, Sel_Eint n2) ->  Sel_Eint (n1*n2)
  |(Sel_Eint 0, e2) ->  mkNeg e2
  |(e1, Sel_Eint 0) -> e1
  |(Sel_binop (Iadd, Sel_Eint n1, e) , Sel_Eint n2) -> mkAdd (Sel_Eint (n1-n2)) e  
  (*|(Sel_Eint n, e) -> Sel_unop (IUadd n, e) *)
  |(e, Sel_Eint n) -> Sel_unop (IUsub n ,e)
  |_ -> Sel_binop (Iimul, e1, e2) (* Si pas d'optimisation possible *)

let instr_selec e = match e with (* Fait la selection d'instructions *)
  |Ty_Eint n -> Sel_Eint n 
  |Ty_Eident x -> Sel_Eident x
  |Ty_Eunop (op , e) -> assert false
  |Ty_Ebinop (op, e1, e2) -> 
    begin match op with
      |Badd -> mkAdd (instr_selec e1) (instr_selec e2)
      |Bsub -> mkSub (instr_selec e1) (instr_selec e2)
      |Bmul -> mkMul (instr_selec e1) (instr_selec e2)
      |_ -> assert false
    end
  |Ty_Eassign_var (s, e1) -> 

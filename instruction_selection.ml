

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
  |Ty_Eint _ |Ty_Eident _ |Ty_Ept-> true
  |Ty_Eunop (_, e1) -> pure e1
  |Ty_Ebinop (_, e1, e2) -> pure e1 && pure e2
  |Ty_Eassign_var _ |Ty_Eassign_ch |Ty_Ecall-> false

let rec mkMul e1 e2 p1 p2 = match (e1, e2) with
  |(Sel_Eint n1, Sel_Eint n2) ->  Sel_Eint (n1*n2)
  |(Sel_Eint 0, e2) when p2 -> Sel_Eint 0
  |(e1, Sel_Eint 0) when p1 -> Sel_Eint 0
  |(e, Sel_Eint n) |(Sel_Eint n, e) -> Sel_unop (IUmul n ,e)
  |_ -> Sel_binop (Iimul, e1, e2) (* Si pas d'optimisation possible *)

let rec mkDiv e1 e2 p2 = match (e1, e2) with 
  |(Sel_Eint n1, Sel_Eint n2) -> Sel_Eint (n1/n2)
  |(Sel_Eint 0, e2) when p2 -> Sel_Eint 0
  |_ -> Sel_binop (Iimul, e1, e2)

let rec mkAnd e1 e2 = match (e1, e2) with
  |(Sel_Eint 0, _) | (_, Sel_Eint 0) -> Sel_Eint 0 (* Eval parresseuse *)
  |_ -> Sel_binop (Iand , e1, e2)

let rec mkOr e1 e2 = match (e1, e2) with
  |(Sel_Eint n, _) where n <> 0 -> Sel_Eint 1
  |(_, Sel_Eint n) where n <> 0 -> Sel_Eint 1
  |_ -> Sel_binop (Ior, e1, e2)


let rec mkEq e1 e2 = match (e1, e2) with 
  |(Sel_Eint n1, Sel_Eint n2) -> Sel_Eint (1 if n1 = n2 else 0)
  |_ -> Sel_binop (Isete, e1, e2)

let rec mkNeq e1 e2 = match (e1, e2) with 
  |(Sel_Eint n1, Sel_Eint n2) -> Sel_Eint (0 if n1 = n2 else 1)
  |_ -> Sel_binop (Isetne, e1, e2)

let rec mkLt e1 e2 = match (e1, e2) with
  |(Sel_Eint n1, Sel_Eint n2) -> Sel_Eint (1 if n1 < n2 else 0)
  |_ -> Sel_binop (Isetlt, e1, e2)

let rec mkLe e1 e2 = match (e1, e2) with
  |(Sel_Eint n1, Sel_Eint n2) -> Sel_Eint (1 if n1 <= n2 else 0)
  |_ -> Sel_binop (Isetle, e1, e2)

let rec mkGt e1 e2 = match (e1, e2) with
  |(Sel_Eint n1, Sel_Eint n2) -> Sel_Eint (1 if n1 > n2 else 0)
  |_ -> Sel_binop (Isetgt, e1, e2)

let rec mkGe e1 e2 = match (e1, e2) with
  |(Sel_Eint n1, Sel_Eint n2) -> Sel_Eint (1 if n1 >= n2 else 0)
  |_ -> Sel_binop (Isetge, e1, e2)

(* Opérations unaires *)

let rec mkNot e1 = match e1 with
  | Sel_Eint n1 -> Sel_Eint (1 if n1 = 0 else 0)
  | _ -> Sel_unop (Inot , e1)

let rec mkNeg e1 = match e1 with
  | Sel_Eint n1 -> Sel_Eint ( -n1 )
  |_ -> Sel_unop (Ineg , e1)

let rec instr_selec e = match e with (* Fait la selection d'instructions *)
  |Ty_Eint n -> Sel_Eint n 
  |Ty_Eident x -> Sel_Eident x
  |Ty_Eunop (op , e) -> 
    begin  match op with
      |Unot -> mkNot (instr_selec e1)
      |Uneg -> mkNeg (instr_selec e1)
    end
  |Ty_Ebinop (op, e1, e2) -> 
    begin match op with
      |Badd -> mkAdd (instr_selec e1) (instr_selec e2)
      |Bsub -> mkSub (instr_selec e1) (instr_selec e2)
      |Bmul -> mkMul (instr_selec e1) (instr_selec e2) (pure e1) (pure e2)
      |Bdiv -> mkDiv (instr_selec e1) (instr_selec e2) (pure e2)
      |Band -> mkAnd (instr_selec e1) (instr_selec e2) 
      |Bor -> mkOr (instr_selec e1) (instr_selec e2)
      |Beq -> mkEq (instr_selec e1) (instr_selec e2)
      |Bneq -> mkNeq (instr_selec e1) (instr_selec e2) 
      |Blt -> mkLt (instr_selec e1) (instr_selec e2)  
      |Ble -> mkLe (instr_selec e1) (instr_selec e2)
      |Bgt -> mkGt (instr_selec e1) (instr_selec e2)
      |Bge -> mkGe (instr_selec e1) (instr_selec e2)
      |_ -> assert false
    end
  |Ty_Eassign_var (s, e1) -> Sel_binop (Imov , e1, Sel_Eident s) 
  |Ty_Eassign_ch (e1, d, e2) -> Sel_store (d, instr_selec e1, instr_selec e2)
  |Ty_Ept (e1, d) -> Sel_load (d, instr_selec e1)
  |Ty_Ecall (x, el) -> Sel_call (x, List.map instr_selec el)

let optIf e s1 s2 = match e with
  |Sel_Eint 0 -> s2
  |Sel_Eint _ -> s1
  |_ -> Sel_Iif (e, s1, s2)

let optWhile e s1 = match e with
  |Sel_Eint 0 -> Sel_Inil (* while(false) *)
  |_ -> Sel_Iwhile (e, s1)

let rec instr_stmt s = match s with
  | Ty_Snil -> Sel_Inil
  | Ty_Sexpr e-> instr_selec e
  | Ty_Sif (e, s1, s2) -> optIf (instr_selec e) (instr_stmt s1) (instr_stmt s2)
  | Ty_Swhile (e, s1)  -> optWhile (instr_selec e) (instr_stmt s1)
  | Ty_Sbloc sl -> Sel_bloc List.map instr_stmt sl
  | Ty_Sreturn e -> Sel_Ireturn (instr_selec e)

let 



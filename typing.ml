open Ast
open Ast_pars
open Ast_ty

let warnings = ref true

(* FONCTIONS AUXILIAIRES POUR VERIFIER SI DEUX TYPES SONT EQUIVALENTS. *)

let ty_equiv (t1 : ctype_typed) (t2 : ctype_typed) : bool =
  (t1 = t2) || ( match t1,t2 with
  | Typenull , CT Int
  | Typenull , CT (Struct _)
  | CT Int , Typenull 
  | CT (Struct _) , Typenull 
  | Void , CT (Struct _)
  | CT (Struct _) , Void -> true
  | _,_ -> false )

let test_ty_equiv t1 t2 =
  if not (ty_equiv t1 t2) 
  then raise (Typing_error {loc = de.loc ;
    msg = "Les deux membres de cette égalité/comparaison ne sont pas de \
    types équivalents. Respectivement : "^(str_of_ctype_typed t1)
    ^" et "^(str_of_ctype_typed t2) } )


(* TRAITEMENT DES EXPRESSIONS *)

let rec ty_expr env de : (ctype_typed * ty_expr) = 
  match de.desc with
  | Par_Eint 0 -> Typenull , Ty_Eint 0
  | Par_Eint n -> CT Int , Ty_Int n
  | Par_Eident id -> 
      begin match IdMap.find_opt id env.env_v with
      | None -> raise (Typing_error {loc = de.loc ; 
        msg = "Variable inconnue dans le contexte"})
      | Some t -> ( CT t , Ty_Eident id)
      end

  | Par_Ept (de',ch) ->
      let type_e',ty_e' = ty_expr env de' in
      begin match type_e' with
      | CT (Struct sr) -> 
        begin match Hashtbl.find_opt env.env_s sr with
        | None -> 
          raise (Typing_error {loc = loc ; msg = "Structure inconnue"})
        | Some (tab_chs,_) -> 
          (match Hashtbl.find_opt tab_chs ch with
          | None -> raise (Typing_error {loc = loc ; 
            msg = ch^" n'est pas un champ de la structure "^sr})
          | Some (t,offset) -> (CT t , Ty_Ept (ty_e',offset)) )
        end
      | _ -> 
        raise (Typing_error {loc = de'.loc ;
        msg = "Devrait être de type une structure, car on en demande un champ"})
      end

  | Par_Eunop (op , de') ->
      let type_e',ty_e' = ty_expr env de' in
      if op = Unot && not (ty_equiv type_e' (CT Int)) 
      then raise (Typing_error {loc = de'.loc ; 
          msg = "Cette expression doit avoir un type équivalent à \
          Int pour lui appliquer un moins unaire."})
      else (CT Int , Ty_Eunop (op,ty_e'))

  | Par_Ebinop (op , de1 , de2) -> 
    let type_e1 , ty_e1 = ty_expr env de1 in
    let type_e2 , ty_e2 = ty_expr env de2 in
    begin match op with
    | Bassign -> begin match ty_e1 with
      | Ty_Eident id ->
        test_ty_equiv type_e1 type_e2 ;
        (type_e1, Ty_Eassign_var (id,ty_e2))
      | Ty_Ept (e1',offset1) ->
        test_ty_equiv type_e1 type_e2 ;
        (type_e1, Ty_Eassign_ch (e1',offset1,ty_e2))
      | _ -> raise (Typing_error { loc = de1.loc ;
        msg = "N'est pas une valeur gauche (ie une variable ou un champ)" })
      end

    | Beq | Bneq | Blt | Ble | Bgt | Bge ->
      test_ty_equiv type_e1 type_e2 ;
      (CT Int , Ty_Ebinop (op , ty_e1 , ty_e2) )

    | Band | Bor ->
      (CT Int , Ty_Ebinop (op , ty_e1 , ty_e2) )

    | Badd | Bsub | Bmul | Bdiv | Bmod ->
      (* TODO simplifier les opérations avec des constantes *)
      test_ty_equiv type_e1 (CT Int) ;
      test_ty_equiv type_e2 (CT Int) ;
      (CT Int , Ty_Ebinop (op , ty_e1 , ty_e2) )
    end

  | Par_Esize did ->
    begin match Hashtbl.find_opt ens_s did.desc with
    | None -> raise (Typing_error {loc = did.loc ; msg = "Structure inconnue"})
    | Some (_,size) -> (CT Int , Ty_Eint size)
    end

  | Par_Ecall (did , lde) ->
    begin match Hashtbl.find_opt ens_f did.desc with
    | None -> raise (Typing_error {loc = did.loc ; msg = "Fonction inconnue"})
    | Some info_f ->
      (* rappel : on jète les fonctions jamais appelées *)
      info_f.usefull <- true ;
      let ltype , lty = List.split (List.map (ty_expr env) lde) in
      begin try
        List.iter2 test_ty_equiv ltype info_f.type_params ;
        (CT info_f.type_r , Ty_Ecall (did.desc , lty) )
      with | Invalid_argument _ ->
        raise (Typing_error {loc = loc ; 
        msg = did.desc ^ " est appelée sur un mauvais nombre d'arguments, "
        ^ (string_of_int (List.length info_f.type_params)) ^ " demandés, "
        ^ (string_of_int (List.length lde)) ^ " donnés." })
      end
    end


(* DECLARATION DE VARIABLES LOCALES *)



(* fct auxiliaire pour vérifier si un type existe *)
let test_welldef env_s (dtyp : ctype desc) = match dtyp.desc with
  | Int -> ()
  | Struct sr -> if not (Hashtbl.mem env_s sr) 
    then raise (Typing_error {loc = dtyp.loc ; msg = "Structure inconnue."})

(* On change int x,y,z ; en int x ; int y ; int z *)
let ty_dvars env_v env_s dvars : (env_vars * par_bloc) = 
  test_welldef env_s dvars.typ ;
  let type_v = dvars.typ.desc in
  List.fold_left 
    (fun (env_v',lty_v) did ->
      if IdMap.mem did.desc env_v' 
      then raise (Typing_error {loc = did.loc ;
        msg = "Nom de variable déjà utilisé, \
        les redéfinitions ne sont pas autorisées." }) ;
      (IdMap.add did.desc type_v env_v' , 
      Par_Bdv {typ = type_v ; var = did.desc} :: lty_v )
    )
    (env_v , [])
   dvars.vars
    


(* TRAITEMENT DES INSTRUCTIONS ET BLOCS *)

(* En Java, le compilateur préfère s'assurer que toute méthode a 
   un return si le type de retour n'est pas void.
   Mais en C gcc ne s'en soucis pas, quitte à planter sur un Segmentation
   Fault à l'exécution. Pour le moment on suit le comportement de gcc,
   toutefois, en mettant warning à vrai, on peut relever les return menqaunt. *)
(* On vire les instructions après un return certain. *)
let rec ty_stmt env type_r_ask ds : (bool * ty_stmt) = 
  (* bool : a-t-on trouvé un return à coup sûr *)
  match ds.desc with
  | Par_Snil -> (false , Ty_Snil)
  | Par_Sexpr de -> 
      let _,ty_e = ty_expr env de in
      (false , Ty_Sexpr ty_e)
  | Par_Sif (de,ds1,ds2) ->
      (* TODO : supprimer les if triviaux *)
      let _,ty_e = ty_expr env de in
      let b1,ty_s1 = ty_stmt env type_r_ask ds1 in
      let b2,ty_s2 = ty_stmt env type_r_ask ds2 in
      (b1 && b2 , Ty_Sif (ty_e , ty_s1 , ty_s2))
  | Par_Swhile (de,ds') ->
      let _,ty_e = ty_expr env de in
      let _,ty_s' = ty_stmt env type_r_ask ds' in
      (* On ne fait pas confiance au while pour un return *)
      (false , Ty_Swhile (ty_e , ty_s'))
  | Par_Sreturn de ->
      let type_e,ty_e = ty_expr env de in
      test_ty_equiv type_e type_r_ask ;
      (true , Ty_Sreturn ty_e)
  | Par_Sbloc bloc ->
      let (b,ty_bl) = ty_bloc env type_r_ask in
      (b , Ty_Sbloc ty_bl)

and ty_bloc env type_r_ask bl : (bool * ty_bloc) = match bl with
  | [] -> (false , [])
  | {desc = Par_Bdv dvars} :: q -> 
    let (env_v',lty_v) = ty_dvars env.env_v env.env_s dvars in
    let env' = {env_v = env_v' ; env_s = env.env_s ; env_f = env_.env_f} in
    let (b,ty_bl) = ty_bloc env' type_r_ask q in
    (b , lty_v @ ty_bl)

  | {desc = Par_Bstmt s ; loc = loc} :: q ->
    let (b,ty_s) = ty_stmt env type_r_ask {desc = s ; loc = loc} in
    if b then (true , [Ty_Bstmt ty_s])
    else
     (let (b',ty_bl') = ty_bloc env type_r_ask q in
      (b , (Ty_Bstmt ty_s) :: ty_bl') )



(* TRAITEMENT DES DÉFINITIONS DE STRUCTURES *)
(* Les définitions de structures disparaissent après le typage, 
   on ne construit par d'arbre de sortie. En revanche on calcule
   les offsets et les sizes. *)

let process_struct env_s dt : unit =
  if Hashtbl.mem env_s dt.nom.desc 
  then raise (Typing_error {loc = dt.nom.loc ;
    msg = "Type construit déjà existant"}) ;
  (* Pour le moment on ne considère que des long int et des pointeurs, 
     ainsi un champ prend toujours 8 octets. *)
  let offset = ref 0 in
  let tabch = Hashtbl.create 8 in
  Hashtbl.add env_s dt.nom.desc (tabch,0) ; 
  (* Permet d'avoir des champs de type cette structure *)
  List.iter 
   (fun (dvars : par_dv) -> 
      test_welldef env_s dvars.typ ; 
      let type_v = dvars.typ.desc in
      List.iter 
       (fun did -> 
          if Hashtbl.mem tabch did.desc 
          then raise (Typing_error { loc = did.loc ;
            msg = "Au sein d'une structure il ne peut y avoir deux champs de même nom."})
          else (Hashtbl.add tabch did.desc !offset ; offset := 8 + !offset)
        )
        dvars.vars
    )
    dt.fields ;
  Hashtbl.replace env_s dt.nom.desc (tabch,!offset)
  
(* TRAITEMENT DES DÉFINITIONS DE FONCTIONS *)
(* But : ajouter une fonction à l'env *)
let ty_fct env_s env_f (df : par_df) : ty_df =
  if Hashtbl.mem env_f df.nom.desc 
  then raise (Typing_error {loc = df.nom.loc ;
    msg = "Une autre application est déjà nommée ainsi" }) ;
  test_welldef env_s df.type_r ;
  let set_params = ref IdSet.empty in
  let type_params,ty_params = 
    List.split (
    List.map 
     (fun (dp : par_param) -> 
        test_welldef env_s dp.typ ;
        if IdSet.mem dp.nom.desc !set_params 
        then raise (Typing_error {loc = dp.nom.loc ;
          msg = "Deux paramètres d'une application ne peuvent partager le même nom."}) ;
        set_params := IdSet.add dp.nom.desc !set_params ;
        (dp.typ.desc , dp.nom.desc)
      )
      df.params ) 
  in
  Hashtbl.add env_f df.nom.desc 
    {usefull = false ; type_r = df.type_r.desc ; type_params = type_params} ;
  let env = {env_v = IdMap.empty ; env_s = env_s ; env_f = env_f} in
  let (b,ty_bl) = ty_bloc env df.type_r.desc df.body in
  if not !b && !warnings
  then Printf.printf "WARNING : il manque un return à cette fonction" ;
  (* Utiliser raise, si Typing_error on s'arrête, si c'est juste Typing_warning, 
     on peut continuer, et si on veut on affiche le warning rapporté. *)
  { id = df.nom.desc ; params = ty_params ; body = ty_bl }



(* FONCTION PRINCIPALE : *)

let ty_file (pf : par_file) : ty_file =
  let env_s = Hashtbl.create 8 in
  let env_f = Hashtbl.create 8 in
  let rec aux = function
    | [] -> []
    | (Par_Ddt dt) :: q -> 
      process_struct env_s dt ; aux q
    | (Par_Ddf df) :: q -> 
      (ty_fct env_s env_f df) :: (aux q)
      (* On ne garde que les fonctions *)
  in
  let list_ty_f = aux pf in
  List.filter 
   (fun (ty_f : ty_df) ->
      let info_f = Hashtbl.find env_f ty_f.id in
      info_f.usefull)
    list_ty_f










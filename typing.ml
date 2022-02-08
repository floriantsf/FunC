open Ast
open Ast_pars
open Ast_ty

let warnings = ref true

(* FONCTIONS AUXILIAIRES *)

(* Vérifie si deux types sont équivalents *)
let ty_equiv (t1 : ctype_typed) (t2 : ctype_typed) : bool =
  (t1 = t2) || ( match t1,t2 with
  | Typenull , CT Int
  | Typenull , CT (Struct _)
  | CT Int , Typenull 
  | CT (Struct _) , Typenull 
  | Void , CT (Struct _)
  | CT (Struct _) , Void -> true
  | _,_ -> false )

let test_ty_equiv loc t1 t2 =
  if not (ty_equiv t1 t2) 
  then raise (Typing_error {loc = loc ;
    msg = "Les deux membres de cette égalité/comparaison ne sont pas de \
    types équivalents. Respectivement : "^(str_of_ctype_typed t1)
    ^" et "^(str_of_ctype_typed t2) } )


(* Vérifie si un type existe *)
let test_welldef env_s (dtyp : ctype desc) = match dtyp.desc with
  | Int -> ()
  | Struct sr -> if not (Hashtbl.mem env_s sr) 
    then raise (Typing_error {loc = dtyp.loc ; msg = "Structure inconnue."})


(* TRAITEMENT DES EXPRESSIONS *)

let rec ty_expr env de : (ctype_typed * ty_expr) = 
  match de.desc with
  | Par_Eint 0 -> Typenull , Ty_Eint 0
  | Par_Eint n -> CT Int , Ty_Eint n
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
          raise (Typing_error {loc = de'.loc ; 
            msg = "Cette expression a pour type une structure inconnue"})
        | Some (tab_chs,_) -> 
          (match Hashtbl.find_opt tab_chs ch.desc with
          | None -> raise (Typing_error {loc = ch.loc ; 
            msg = ch.desc ^" n'est pas un champ de la structure "^sr})
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
        test_ty_equiv de.loc type_e1 type_e2 ;
        (type_e1, Ty_Eassign_var (id,ty_e2))
      | Ty_Ept (e1',offset1) ->
        test_ty_equiv de.loc type_e1 type_e2 ;
        (type_e1, Ty_Eassign_ch (e1',offset1,ty_e2))
      | _ -> raise (Typing_error { loc = de1.loc ;
        msg = "N'est pas une valeur gauche (ie une variable ou un champ)" })
      end

    | Beq | Bneq | Blt | Ble | Bgt | Bge ->
      test_ty_equiv de.loc type_e1 type_e2 ;
      (CT Int , Ty_Ebinop (op , ty_e1 , ty_e2) )

    | Band | Bor ->
      (CT Int , Ty_Ebinop (op , ty_e1 , ty_e2) )

    | Badd | Bsub | Bmul | Bdiv | Bmod ->
      test_ty_equiv de.loc type_e1 (CT Int) ;
      test_ty_equiv de.loc type_e2 (CT Int) ;
      (CT Int , Ty_Ebinop (op , ty_e1 , ty_e2) )
    end

  | Par_Esize did ->
    begin match Hashtbl.find_opt env.env_s did.desc with
    | None -> raise (Typing_error {loc = did.loc ; msg = "Structure inconnue"})
    | Some (_,size) -> (CT Int , Ty_Eint size)
    end

  | Par_Ecall (did , lde) ->
    begin match Hashtbl.find_opt env.env_f did.desc with
    | None -> raise (Typing_error {loc = did.loc ; msg = "Fonction inconnue"})
    | Some info_f ->
      (* rappel : on jète les fonctions jamais appelées *)
      info_f.usefull <- true ;
      let ltype , lty = List.split (List.map (ty_expr env) lde) in
      let lloc = List.map (fun (de : par_expr desc) -> de.loc) lde in
      let ltype_loc = List.combine ltype lloc in
      begin try
        List.iter2 
          (fun (type_e,loc_e) type_ask -> test_ty_equiv loc_e type_e (CT type_ask)) 
          ltype_loc info_f.type_params ;
        (CT info_f.type_r , Ty_Ecall (did.desc , lty) )
      with | Invalid_argument _ ->
        raise (Typing_error {loc = de.loc ; 
        msg = did.desc ^ " est appelée sur un mauvais nombre d'arguments, "
        ^ (string_of_int (List.length info_f.type_params)) ^ " demandés, "
        ^ (string_of_int (List.length lde)) ^ " donnés." })
      end
    end


(* TRAITEMENT DES INSTRUCTIONS *)

(* REM : 
   - En Java, le compilateur préfère s'assurer que toute méthode a 
    un return si le type de retour n'est pas void.
    Mais en C gcc ne s'en soucit pas, quitte à planter sur un Segmentation
    Fault à l'exécution. Pour le moment on suit le comportement de gcc,
    toutefois, en mettant warning à vrai, on peut relever les return manquant.
   - On vire les instructions après un return certain.
   - On change int x=3,y,z=2 ; en int x; x=3; int y; int z; z=2; 
   Difficulté : le découpage des déclarations de variables, change une instruction
   en plusieurs, mais il ne s'agit pas pour autant d'un sous-bloc d'instructions. 
   (Sinon les variables seraient déclarées locales au sous-bloc...)
   Si l'instruction int x=3,y,z=2 ; apparaissait dans un bloc, il faut la substituer
   par la liste des autres, et non pas la changer en Ty_Bloc (...). 
   Si elle n'était pas dans un bloc, on peut faire un Ty_Bloc. 
   Idem il faut garder ou non le nouveau env_vars. *)

let ty_dvars env (dvars : par_dv) : (ty_env * (ty_stmt list)) =
  test_welldef env.env_s dvars.typ ;
  let type_v = dvars.typ.desc in
  let env' = {env_v = env.env_v ; env_s = env.env_s ; env_f = env.env_f} in 
  let l_ty_s = List.fold_left
   (fun l_ty_s (did, de_opt) ->
      let id = did.desc in
      if IdMap.mem id env'.env_v
      then raise (Typing_error {loc = did.loc ;
        msg = "Nom de variable déjà utilisé, \
        les redéfinitions ne sont pas autorisées." }) ;
      env'.env_v <- IdMap.add id type_v env'.env_v ;
      (Ty_Sdv id) :: 
        begin match de_opt with
        | None -> l_ty_s
        | Some de -> 
          let new_e = Par_Ebinop (Bassign , {desc = Par_Eident id ; loc = did.loc} , de) in
          let new_loc_e = ( fst(did.loc) , snd(de.loc) ) in
          let new_de : par_expr desc = {desc = new_e ; loc = new_loc_e} in
          (Ty_Sexpr (snd (ty_expr env new_de))) :: l_ty_s
        end
    )
    [] dvars.vars_expr in
  (env' , l_ty_s)


let rec ty_stmt env type_r_ask st : (bool * ty_stmt) = match st with
  (* bool : a-t-on trouvé un return à coup sûr *) 
  | Par_Sdv dvars -> 
      let _ , l_ty_s = ty_dvars env dvars in
      (false , Ty_Sbloc l_ty_s)
  | Par_Snil -> (false , Ty_Snil)
  | Par_Sexpr de -> 
      let _,ty_e = ty_expr env de in
      (false , Ty_Sexpr ty_e)
  | Par_Sif (de,st1,st2) ->
      let _,ty_e = ty_expr env de in
      let b1,ty_s1 = ty_stmt env type_r_ask st1 in
      let b2,ty_s2 = ty_stmt env type_r_ask st2 in
      (b1 && b2 , Ty_Sif (ty_e , ty_s1 , ty_s2))
  | Par_Swhile (de,st') ->
      let _,ty_e = ty_expr env de in
      let _,ty_s' = ty_stmt env type_r_ask st' in
      (* On ne fait pas confiance au while pour un return *)
      (false , Ty_Swhile (ty_e , ty_s'))
  | Par_Sreturn de ->
      let type_e,ty_e = ty_expr env de in
      test_ty_equiv de.loc type_e type_r_ask ;
      (true , Ty_Sreturn ty_e)
  | Par_Sbloc l_ds ->
      let rec aux_bloc env' = function
        | [] -> (false , [])
        | (Par_Sdv dvars) :: q ->
            let (env'',l_ty_s) = ty_dvars env' dvars in
            let (b,l_ty_reste) = aux_bloc env'' q in
            (b , l_ty_s @ l_ty_reste)
        | st' :: q -> (* pas une decl_var *)
            let (b,ty_s) = ty_stmt env' type_r_ask st' in
            if b then (true , [ty_s])
            else
            ( let (b',l_ty_reste) = aux_bloc env' q in
              ( b' , ty_s :: l_ty_reste) )
      in
      let (b,l_ty_s) = aux_bloc env l_ds in
      (b , Ty_Sbloc l_ty_s)


(* TRAITEMENT DES DÉFINITIONS DE STRUCTURES *)
(* Les définitions de structures disparaissent après le typage, 
   on ne construit par d'arbre de sortie. En revanche on calcule
   les offsets et les sizes. *)

let process_struct env_s (dt : par_dt) : unit =
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
   (fun (dvars : par_dfl) -> 
      test_welldef env_s dvars.typ ; 
      let type_v = dvars.typ.desc in
      List.iter 
       (fun did -> 
          if Hashtbl.mem tabch did.desc 
          then raise (Typing_error { loc = did.loc ;
            msg = "Au sein d'une structure il ne peut y avoir deux champs de même nom."})
          else (Hashtbl.add tabch did.desc (type_v,!offset) ; offset := 8 + !offset)
        )
        dvars.vars
    )
    dt.fields ;
  Hashtbl.replace env_s dt.nom.desc (tabch,!offset)



(* TRAITEMENT DES DÉFINITIONS DE FONCTIONS *)
(* But : ajouter une fonction à l'env *)
let ty_fct env_s env_f (df : par_df) : ty_df =
  let f_nom = df.nom.desc in
  if Hashtbl.mem env_f f_nom 
  then raise (Typing_error {loc = df.nom.loc ;
    msg = "Une autre application est déjà nommée ainsi" }) ;
  test_welldef env_s df.type_r ;
  let set_params = ref IdSet.empty in
  let type_params,ty_params = 
    List.split ( List.map 
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
  let info_f = {usefull = false ; type_r = df.type_r.desc ; type_params = type_params} in
  if f_nom = "main" then info_f.usefull <- true ;
  Hashtbl.add env_f f_nom info_f ; 
  let env = {env_v = IdMap.empty ; env_s = env_s ; env_f = env_f} in
  let (b,bty_st) = ty_stmt env (CT df.type_r.desc) (Par_Sbloc df.body) in
  let l_ty_s = match bty_st with | Ty_Sbloc l -> l | _ -> failwith "n'arrive pas" in
  if not b && !warnings
  then Printf.printf "WARNING : il manque un return à cette fonction" ;
  (* Utiliser raise, si Typing_error on s'arrête, si c'est juste Typing_warning, 
     on peut continuer, et si on veut on affiche le warning rapporté. *)
  { id = f_nom ; params = ty_params ; body = l_ty_s }



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


open Miniml_types

(* signature minimale pour définir des variables *)
module type VariableSpec =
  sig
    (* type abstrait des variables      *)
    type t

    (* création d'une variable fraîche  *)
    val fraiche : unit -> t

    (* fonctions de comparaison         *)
    (* permet de définir des conteneurs *)
    (* (hash-table, etc) de variables   *)
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val hash : t -> int

    (* fonction d'affichage             *)
    (* on utilise Format.std_formatter  *)
    (* comme premier paramètre          *)
    (* pour la sortie standard          *) 
    val fprintf : Format.formatter -> t -> unit
  end

(* implantation de la spécification     *)
module TypeVariable : VariableSpec =
  struct
    type t = int

    let fraiche =
      let cpt = ref 0 in
      (fun () -> incr cpt; !cpt)

    let compare a b = a - b
    let equal a b = a = b
    let hash a = Hashtbl.hash a

    let fprintf fmt a = Format.fprintf fmt "t{%d}" a
  end

let rec contains liste e =
  match liste with
    | [] -> false
    | (var, typ)::q -> if ((compare e var) == 0) then
                true
              else
                (contains q e)

(*Renvoie le type de la variable e dans la liste de couples (variable, typeDeVariable)*)
(*j'ai oblige a mettre une liste non vide car je savais pas quoi mettre au cas list vide*)
let rec typeof ((var, typ)::q) e =
  if ((compare e var) == 0) then
    typ
  else
    (typeof q e)

(* ******** à compléter ********* *) (* : type a. expr -> a typ = fun  *)
(* L'environnement est une liste ordonnee de couples (nomVariable, typeVariable) *)
(* Le but st de retourner le type de expr *)
(*let rec typer env expr =
  match expr with
          | EConstant c           -> (match c with
                                      | CBooleen _ -> TBool
                                      | CEntier _ -> TInt
                                      | CNil -> TList(TVar(TypeVariable.fraiche ())) (* Tlist 'a *)
                                      | CUnit ->  TUnit
                                     )
          
          | EIdent id             ->  (*if (id in env) return type(id) sinon erreur *) (*pour chercher id dans env, on pop tant qu'on trouve pas*)
                                      if ((contains env id)) then
                                        typeof env id (*on accepte l'expr et on continue (voir regle Var1) *)
                                      else
                                        failwith "error l'ident n'est pas dans l'environnement" (*Var2 *) (*pas sure*)
          | EProd (e1, e2)        -> TProd((typer env e1), (typer env e2))
          | ECons (e1, e2)        -> let type_e2 = typer env e2 in 
                                     (match type_e2 with
                                      | TList(TVar(_)) -> TList(typer env e1)
                                      | _ -> type_e2)
          | EFun (id, e)          ->  TFun (TVar(TypeVariable.fraiche ()), (typer env e))
          | EIf (b, e1, e2)       -> typer env e1 (* Attention : il faut check que type(b) = bool *)
          (*| EApply (f, a)         -> assert false*)
          | ELet (id, e1, e2)     -> let new_env = ((id, (typer env e1))::env) in (typer new_env e2) (*dans env de e2 on met (id, type(e1))*)
          (*| ELetrec (id, e1, e2)  -> let new_env = ((id, (typer new_env e1))::env) in (typer new_env e2)
                                       (*dans env de e2 et de e1 on met (id, type(e1)) jsp si ca peut marcher de mettre le type  de e1 dans env de e1*)
          *)| _ -> assert false
*)
(* fun x -> x + 3 *)

(*[Mylene]*)
(*Pour l'instant on va representer Production(e1, ..., en) par une liste et l'environnement aussi *)

(*Renvoie true si e est dans la liste de couples (variable, typeDeVariable)*)




(*Renvoie le type de l'expr ou alors failwith*)
let rec type_unique expression =
(*je pense que l'algo renvoie une liste de couple avec un environnement et une var/ident)
  il faudrait donc ajouter a la liste des production les nouvelles expr trouvees
  et a la fin il faut que chaque expr soit du meme type (ca c'est peut-etre plus tard)
  TYPES :
    entree : liste de (env, expr)
    env : liste de (id, 'a typ)
    sortie : list de 'a typ

                                        let (t, list_equ) = typeof env id in (t, (nouvelleEquation::list_equ)) 
  *)
  let rec typer env expr list_equ = 
        match expr with
          | EConstant c           -> (match c with
                                      | CBooleen _ -> (TBool, list_equ)
                                      | CEntier _ -> (TInt, list_equ)
                                      | CNil -> (TList(TVar(TypeVariable.fraiche ())), list_equ) (* Tlist 'a *)
                                      | CUnit ->  (TUnit, list_equ)
                                     )
          
          | EIdent id             ->  (*if (id in env) return type(id) sinon erreur *) (*pour chercher id dans env, on pop tant qu'on trouve pas*)
                                      if ((contains env id)) then
                                        ((typeof env id), list_equ) (*on accepte l'expr et on continue (voir regle Var1) *)
                                      else
                                        (TVar(TypeVariable.fraiche ()), list_equ) (*Var2 *) (*pas sure*) (*verifier qu'on a : 'a -> 'a et pas 'a -> 'b pour les fonctions *)
          
          | EProd (e1, e2)        -> let (t1, l1) = typer env e1 list_equ in let (t2, l2) = typer env e2 list_equ in ((TProd(t1, t2), l1@l2))
          
          | ECons (e1, e2)        ->  let (t2, l2) = typer env e2 list_equ in
                                       let (t1, l1) = typer env e1 list_equ in
                                        (t2, (t2,TList(t1))::(l1@l2))
          
          | EFun (_, e)          -> let (t, l) = typer env e list_equ in (TFun (TVar(TypeVariable.fraiche ()), t), l)
          
          | EIf (b, e1, e2)       ->  let (t2, l2) = typer env e2 list_equ in
                                      let (t1, l1) = typer env e1 list_equ in
                                      let (tb, lb) = typer env b list_equ in
                                        typer env e1 ((tb, TBool)::(t1, t2)::(l1@l2@lb))
          
          | EApply (e1, e2)         ->  let alpha = TVar(TypeVariable.fraiche ()) in
                                        let (t2, l2) = typer env e2 list_equ in (* 2 *) (* [3] *)
                                        let (t1,l1) = match e1 with (* + *) (* :: *)
                                          | EBinop(PLUS) | EBinop(MOINS) | EBinop(DIV) | EBinop(MULT)-> (TFun(TInt, TInt), list_equ) (* x -> x + 2 *)
                                          | EBinop(AND) | EBinop(OR) -> (TFun(TBool, TBool), list_equ)
                                          | EBinop(EQU) | EBinop(NOTEQ) | EBinop(INF) | EBinop(SUP) | EBinop(INFEQ) | EBinop(SUPEQ) -> (TFun(TInt, TBool), list_equ)
                                          | EBinop(CONCAT) -> (TFun(TList(alpha), TList(alpha)), list_equ)
                                          | EBinop(CONS) -> (TFun(alpha, TList(alpha)), list_equ)
                                          | _ -> typer env e1 list_equ in
                                        let beta = TVar(TypeVariable.fraiche ()) in (beta, (t1,TFun(t2, beta))::(l1@l2))
(*
EBinop(token) + * / => int   e1 == int-> e2 == int
:: @ => list
= <= => bool
2 + 1
  (EApply(EApply(EBinop(PLUS), EConstant(CEntier(2))), EConstant(CEntier(1))))
*)

          | ELet (id, e1, e2)     ->  let (t1, l1) = typer env e1 list_equ in
                                      let new_env = ((id, t1)::env) in
                                      let (t2, l2) = typer new_env e2 list_equ in
                                        (t2, (l1@l2)) (*dans env de e2 on met (id, type(e1))*)

          | ELetrec (id, e1, e2)  ->  let (t1, l1) = typer env e1 list_equ in
                                      let alpha = TVar(TypeVariable.fraiche ()) in
                                      let new_env = (id, alpha)::env in
                                      let (t2, l2) = typer new_env e2 list_equ in
                                        (t2, (alpha, t1)::(l1@l2))
                                       (*dans env de e2 et de e1 on met (id, type(e1)) jsp si ca peut marcher de mettre le type  de e1 dans env de e1*)
                                      (*test : (ELetrec("x", EIdent("x"), EIdent("x"))) renvoie <abstr>
                                                (ELetrec("x", EIdent("x"), EConstant(CEntier(3))))
                                                on a un pb: ca marche pour let normal ATTENTION*)
          | _ -> assert false

(* 
2 + 1
  (EApply(EApply(EBinop(PLUS), EConstant(CEntier(2))), EConstant(CEntier(1))))
2 :: [] using binop ::
  (EApply(EApply(EBinop(CONS), EConstant(CEntier(2))), EConstant(CNil)));;
 *)


(*tests BINOP
regler les alphas
faire la fonction des equations*)


  in typer [] expression []
  
    (*let prod = (typer [([], expression)]) in
        let rec resultat production =
          match production with
            | [] -> failwith "error le resultat est vide"
            | [typ] -> typ
            | typ1::typ2::q ->  if (typ1 == typ2) then
                                  (resultat q)
                                else
                                  failwith "error les types du resultat sont differents"
        in
          resultat prod
          *)
(*marche pas
let _ = type_unique (ELet("x", EConstant(CEntier(3)), EIdent("x")))*)
(*Fonctionne mais faudrait dire que c'est un Tlist of TInt et pas un TInt *)
(*let _ = type_unique (ECons(EConstant(CEntier(3)), EConstant(CNil))*)



(*
let x = []  'a list  TList of 'a typ
1::[] int avec 'a list donc int list
*)

(*Ameliorations :
  - faire une autre liste dans la fonction qui contient une expr et un typ et a la fin il faut verifier que l'expr est bien egale au type
    pour les regles a cote de la barre de division dans if par exemple *)







(* Partir d'un environnement (Gamma dans les équ), et d'une expr e = Prod(e1, e2, ..., en) à typer
  Récursivement on calcule taui, le type des sous-expr ei dans les env Gammai avec éventuellement des définitions locales suivant Production
  Récupérer ces types et construire le type global tau de e avec éventuellement des alphas, betas...  
  
  Les équations de types servent à définir et contrôler les relations, mais aussi à définir les alphas, betas...
  Elles sont obtenues à chaque étape et accumulées. Ainsi elles forment un résultat de l'algo de typage --> jusque là on a *)

  let rec resol_equ list_equ =
    match list_equ with
      | (t::q) -> match t with
                    | (TInt, TInt) -> return resol_equ q
                    | (TBool, TBool) -> return resol_equ q
                    | (TUnit, TUnit) -> return resol_equ q
                    | (TList(t1), TList(t2)) -> return resol_equ (t1, t2)::q
                    | (TFun(t1, t2), TFun(sig1, sig2)) -> return resol_equ (t1, t2)::(sig1, sig2)::q
                    | ((t1, t2), (sig1, sig2)) -> return resol_equ (t1, sig1)::(t2, sig2)::q
                    | (TVar(TypeVariable.fraiche ()), TVar(TypeVariable.fraiche ())) -> return resol_equ q (* pas certain, est ce que ce sont les deux mêmes alphas ici ?? *)
                    | 
                    | _ -> false
      | _ -> true

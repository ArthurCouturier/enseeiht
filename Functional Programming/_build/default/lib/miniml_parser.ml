open Miniml_types
open Miniml_lexer
open Lazyflux
open Miniml_printer

(* Fonction de lecture d'un fichier.    *)
(* Produit le flux des lexèmes reconnus *)
let read_miniml_tokens_from_file filename : token Flux.t =
  try
    let chan = open_in filename in
    let buf = Lexing.from_channel chan in
    line_g := 1;
    let next_token () =
      try
        let next = token buf in
        if next = EOF
        then
          begin
            close_in chan;
            None
          end
        else
          Some (next, ())
   with
   | ErreurLex msg ->
      begin
        close_in chan;
        raise (ErreurLecture (Format.sprintf "ERREUR : ligne %d, lexème '%s' : %s" !line_g (Lexing.lexeme buf) msg))
      end in
    Flux.unfold next_token ()
 with
    | Sys_error _ -> raise (ErreurLecture (Format.sprintf "ERREUR : Impossible d'ouvrir le fichier '%s' !" filename))
;;

(* Fonction de lecture d'un buffer.   *)
(* Similaire à la fonction précédente *)
let read_miniml_tokens_from_lexbuf buf : token Flux.t =
  line_g := 1;
  let next_token () =
    try
      let next = token buf in
      if next = EOF
      then
        begin
          None
        end
      else
        Some (next, ())
    with
    | ErreurLex msg ->
       begin
         raise (ErreurLecture (Format.sprintf "ERREUR : ligne %d, lexème '%s' : %s" !line_g (Lexing.lexeme buf) msg))
       end in
  Flux.unfold next_token ()
;;

(* Fonction de lecture d'une chaîne.  *)
(* Similaire à la fonction précédente *)
let read_miniml_tokens_from_string chaine : token Flux.t =
  read_miniml_tokens_from_lexbuf (Lexing.from_string chaine)
;;

(* Fonctions auxiliaires de traitement des lexèmes *)
(* contenant une information: IDENT, BOOL et INT   *)
let isident =
  function IDENT _     -> true
         | _           -> false
let isbool =
  function BOOL _      -> true
         | _           -> false
let isint =
  function INT _       -> true
         | _           -> false

let unident =
  function
  | IDENT id    -> id
  | _           -> assert false
let unbool =
  function
  | BOOL b      -> b
  | _           -> assert false   
let unint =
  function
  | INT i       -> i
  | _           -> assert false


(* Fonctions de parsing de MiniML *)
module Solution = Flux;;

(* types des parsers généraux *)
type ('a, 'b) result = ('b * 'a Flux.t) Solution.t;;
type ('a, 'b) parser = 'a Flux.t -> ('a, 'b) result;;

(* interface des parsers: combinateurs de parsers et parsers simples *)
module type Parsing =
  sig
    val map : ('b -> 'c) -> ('a, 'b) parser -> ('a, 'c) parser

    val return : 'b -> ('a, 'b) parser

    val ( >>= ) : ('a, 'b) parser -> ('b -> ('a, 'c) parser) -> ('a, 'c) parser

    val zero : ('a, 'b) parser

    val ( ++ ) : ('a, 'b) parser -> ('a, 'b) parser -> ('a, 'b) parser

    val run : ('a, 'b) parser -> 'a Flux.t -> 'b Solution.t

    val pvide : ('a, unit) parser

    val ptest : ('a -> bool) -> ('a, 'a) parser

    val ( *> ) : ('a, 'b) parser -> ('a, 'c) parser -> ('a, 'b * 'c) parser
  end

(* implantation des parsers, comme vu en TD. On utilise les opérations *)
(* du module Flux et du module Solution                                *)
module Parser : Parsing =
  struct
    let map fmap parse f = Solution.map (fun (b, f') -> (fmap b, f')) (parse f);; 

    let return b f = Solution.return (b, f);;

    let (>>=) parse dep_parse = fun f -> Solution.(parse f >>= fun (b, f') -> dep_parse b f');;

    let zero f = Solution.zero;;

    let (++) parse1 parse2 = fun f -> Solution.(parse1 f ++ parse2 f);;

    let run parse f = Solution.(map fst (filter (fun (b, f') -> Flux.uncons f' = None) (parse f)));;

    let pvide f =
      match Flux.uncons f with
      | None   -> Solution.return ((), f)
      | Some _ -> Solution.zero;;

    let ptest p f =
      match Flux.uncons f with
      | None        -> Solution.zero
      | Some (t, q) -> if p t then Solution.return (t, q) else Solution.zero;;

    let ( *> ) parse1 parse2 = fun f ->
      Solution.(parse1 f >>= fun (b, f') -> parse2 f' >>= fun (c, f'') -> return ((b, c), f''));;
  end

open Parser
(* 'droppe' le resultat d'un parser et le remplace par () *)
let drop p = map (fun x -> ()) p
let p_token token = drop (ptest ((=) token))
(* Definition des parsers des tokens *)
let p_To = p_token TO;;
let p_in = p_token IN;;
let p_Then = p_token THEN;;
let p_Else = p_token ELSE;;
let p_Plus = p_token PLUS;;
let p_Moins = p_token MOINS;;
let p_Mult = p_token MULT;;
let p_Div = p_token DIV;;
let p_And = p_token AND;;
let p_Or = p_token OR;;
let p_Eq = p_token EQU;;
let p_Noteq = p_token NOTEQ;;
let p_SupEq = p_token SUPEQ;;
let p_InfEq = p_token INFEQ;;
let p_Inf = p_token INF;;
let p_Sup = p_token SUP;;
let p_Concat = p_token CONCAT;;
let p_Cons = p_token CONS;;
let p_ParFer = p_token PARF;;
let p_ParOuv = p_token PARO;;
let p_let = p_token LET;;
let p_rec = p_token REC;;
let p_Fun = p_token FUN;;
let p_If = p_token IF;;
let p_CroOuv = p_token CROO;;
let p_CroFer = p_token CROF;;
let p_eof = pvide;;
let p_ident = (ptest isident) >>= fun token -> return (unident token);;
let p_bool = (ptest isbool) >>= fun token -> return (unbool token);;
let p_entier = (ptest isint) >>= fun token -> return (unint token);;

(* Fonction qui parse récursivement une expression
  Paramètres : 


 *)
let rec p_Expr : (token, expr) parser = fun flux ->
    (
   (p_let >>= fun () -> p_ident
         >>= fun ident -> p_Eq
            >>= fun () -> p_Expr
            >>= fun exprLiaison -> p_in
            >>= fun () -> p_Expr
            >>= fun exprDroite -> return (ELet(ident, exprLiaison, exprDroite))(* ELet of ident * expr * expr *)
      ) ++ (p_let >>= fun () -> p_rec
                  >>= fun () -> p_ident
                  >>= fun ident -> p_Eq
                  >>= fun () -> p_Expr
                  >>= fun exprLiaison -> p_in
                  >>= fun () -> p_Expr
                  >>= fun exprDroite -> return (ELetrec(ident, exprLiaison, exprDroite))
      ) ++ (p_ParOuv >>= fun () -> p_Expr
                     >>= fun e1 -> p_Binop
                     >>= fun token -> p_Expr
                     >>= fun e2 -> p_ParFer
                     >>= fun () -> return (EApply(EApply(EBinop(token), e1), e2)) (* (((+) 2) 1);; <==> 1 + 2 <==> EApply(EApply(EBinop(PLUS), 2), 1) *)
      ) ++ (p_ParOuv >>= fun () -> p_Expr
                     >>= fun e -> p_ParFer
                     >>= fun () -> return e
      ) ++ (p_ParOuv >>= fun () -> p_Expr
                     >>= fun e1 -> p_Expr
                     >>= fun e2 -> p_ParFer
                     >>= fun () -> return (EApply(e1, e2))
      ) ++ (p_If >>= fun () -> p_Expr
                 >>= fun e1 -> p_Then
                 >>= fun () -> p_Expr
                 >>= fun e2 -> p_Else
                 >>= fun () -> p_Expr
                 >>= fun e3 -> return (EIf(e1, e2, e3))
      ) ++ (p_ParOuv >>= fun () -> p_Fun
                     >>= fun () -> p_ident
                     >>= fun i -> p_To
                     >>= fun () -> p_Expr
                     >>= fun e -> p_ParFer
                     >>= fun () -> return (EFun(i, e))
      ) ++ (p_ident >>= fun i -> return (EIdent(i)) (* EConstant(constant) *)
      ) ++ (p_Constant >>= fun c -> return (EConstant(c)); (* EConstant(constant) *)
      )
    ) flux
and p_Constant : (token, constant) parser = fun flux ->
  (
    (p_entier >>= fun e -> return (CEntier(e)))
    ++ 
    (p_bool >>= fun b -> return (CBooleen(b))) 
    ++ 
    (p_CroOuv >>= fun () -> p_CroFer >>= fun () -> return (CNil)) 
    ++
    (p_ParOuv >>= fun () -> p_ParFer >>= fun () -> return (CUnit))
  ) flux
and p_Binop : (token, token) parser = fun flux ->
  (
    (p_Plus >>= fun () -> return (PLUS))
    ++ 
    (p_Moins >>= fun () -> return (MOINS)) 
    ++ 
    (p_Mult >>= fun () -> return (MULT)) 
    ++
    (p_Div >>= fun () -> return (DIV))
    ++
    (p_And >>= fun () -> return (AND))
    ++
    (p_Or >>= fun () -> return (OR))
    ++
    (p_Eq >>= fun () -> return (EQU))
    ++
    (p_Noteq >>= fun () -> return (NOTEQ))
    ++
    (p_InfEq >>= fun () -> return (INFEQ))
    ++
    (p_SupEq >>= fun () -> return (SUPEQ))
    ++
    (p_Inf >>= fun () -> return (INF))
    ++
    (p_Sup >>= fun () -> return (SUP))
    ++    
    (p_Concat >>= fun () -> return (CONCAT))
    ++
    (p_Cons >>= fun () -> return (CONS))
  ) flux

let parse_expression flux = run (map fst (p_Expr *> p_eof)) flux;;

let _ =
  let f = read_miniml_tokens_from_file "lib/exemple1.miniml" in
  let progs = parse_expression f in
  match Solution.uncons progs with
  | None        -> (Format.printf "** parsing failed ! **@.";)
  | Some (expression, _) -> 
    begin
      print_expr Format.std_formatter expression;
      Format.printf "\n** parsing succesfull ! **@.\n";
    end


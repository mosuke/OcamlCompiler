open Pretty
module S = Syntax
module C = Closure

exception Error of string
let err s = raise (Error s)

type id = C.id
type binOp = C.binOp

(* ==== 値 ==== *)
type value =
    Var  of id
  | Fun  of id   (* NEW *)
  | IntV of int

(* ==== 式 ==== *)
type cexp =
    ValExp    of value
  | BinOp     of binOp * value * value
  | AppExp    of value * value list
  | IfExp     of value * exp * exp
  | TupleExp  of value list
  | ProjExp   of value * int

and exp =
    CompExp   of cexp
  | LetExp    of id * cexp * exp
  | LoopExp   of id * cexp * exp
  | RecurExp  of value

(* ==== 関数宣言 ==== *)
type decl = RecDecl of id * id list * exp  (* NEW *)

(* ==== プログラム ==== *)
type prog = decl list  (* NEW *)

(* ==== Formatter ==== *)

let string_of_flat prog =
  let pr_of_op = function
      S.Plus -> text "+"
    | S.Mult -> text "*"
    | S.Lt -> text "<" in
  let pr_of_value = function
      Var id -> text id
    | Fun id -> text "#'" <*> text id
    | IntV i ->
      let s = text (string_of_int i) in
      if i < 0 then text "(" <*> s <*> text ")" else s
  in
  let pr_of_values = function
      [] -> text "()"
    | v :: vs' ->
      text "(" <*>
      (List.fold_left
         (fun d v -> d <*> text "," <+> pr_of_value v)
         (pr_of_value v) vs')
      <*> (text ")")
  in
  let rec pr_of_cexp p e =
    let enclose p' e = if p' < p then text "(" <*> e <*> text ")" else e in
    match e with
      ValExp v -> pr_of_value v
    | BinOp (op, v1, v2) ->
      enclose 2 (pr_of_value v1 <+> pr_of_op op <+> pr_of_value v2)
    | AppExp (f, vs) ->
      enclose 3 (pr_of_value f <+> pr_of_values vs)
    | IfExp (v, e1, e2) ->
      enclose 1
        ( ((nest 2
              ( ((text "if 0 <")
                 <+> pr_of_value v
                 <+> text "then"
                 <|> pr_of_exp 1 e1))) <|>
           (nest 2
              ( (text "else" <|> pr_of_exp 1 e2)))))
    | TupleExp vs -> pr_of_values vs
    | ProjExp (v, i) ->
      enclose 2 (pr_of_value v <*> text "." <*> text (string_of_int i))
  and pr_of_exp p e =
    let enclose p' e = if p' < p then text "(" <*> e <*> text ")" else e in
    match e with
      CompExp ce -> pr_of_cexp p ce
    | LetExp (id, ce, e) ->
      enclose 1
        ((nest 2 ( (text "let" <+> text id <+>
                    text "=" <|> pr_of_cexp 1 ce)))
         <+> text "in" <|> pr_of_exp 1 e)
    | LoopExp (id, ce, e) ->
      enclose 1
        ((nest 2 ( (text "loop" <+> text id <+>
                    text "=" <|> pr_of_cexp 1 ce)))
         <+> text "in" <|> pr_of_exp 1 e)
    | RecurExp v ->
      enclose 3 (text "recur" <+> pr_of_value v)
  in
  let pr_of_decl = function
      RecDecl (id, params, body) ->
      let tparms = match params with
          [] -> text ""
        | param :: params' ->
          List.fold_left (fun t p -> t <*> text "," <+> text p)
            (text param) params'
      in
      ( (text "let" <+> text "rec" <+>
         (
           ((text id) <+> text "(" <*> tparms <*> text ")" <+>
            nest 2 (text "=" <|> pr_of_exp 1 body)))))
  in
  layout
    (pretty 30 (List.fold_right (fun decl doc ->
         pr_of_decl decl <|> nil <|> doc) prog nil))

(* ==== フラット化：変数参照と関数参照の区別も同時に行う ==== *)
(*コピー伝播を行う関数*)
let copy_propagation exp =
  let convert_id id d = 
    try let id' = List.assoc id d in  id'
    with Not_found ->  id
  in
  let convert_value value d= 
    match value with 
      Var id ->  Var (convert_id id d)
    | IntV i -> IntV i
    | Fun f -> Fun f 
  in
  let rec convert_exp e d=
    let convert_cexp ce d'=
      match ce with 
        ValExp(v) -> ValExp (convert_value v d')
      | IfExp(v, e1, e2) -> 
        IfExp(
          convert_value v d',
          convert_exp e1 d', 
          convert_exp e2 d' )
      | BinOp(bop, v1, v2) -> BinOp(bop, convert_value v1 d', convert_value v2 d') 
      | AppExp(v,l) -> AppExp(convert_value v d', List.map (fun x -> convert_value x d')l)
      | TupleExp(l) -> TupleExp(List.map (fun x -> convert_value x d')l)
      | ProjExp(v,i) -> ProjExp(convert_value v d', i)
    in
    match e with 
      LetExp(id1, ValExp(Var id2), ex) -> 
      convert_exp ex ((id1, convert_id id2 d)::d)
    | LetExp(id1, cexp, ex) -> LetExp(id1, convert_cexp cexp d, convert_exp ex d)
    | LoopExp(id1, cexp, ex) -> LoopExp(id1, convert_cexp cexp d, convert_exp ex d)
    | RecurExp(e) -> RecurExp(convert_value e d)
    | CompExp(cexp) -> CompExp(convert_cexp cexp d)
  in
  convert_exp exp []
let flatten exp =
  let fun_list = ref [] 
  in
  let var_list = ref []
  in
  let decl_list = ref []
  in
  (*変換しながらリストに入れていく
    input C.Exp
    output Flat.Exp
  *)
  let rec convert_and_search e  = 
    (*環境に従って分類して出力する
      input C.Value
      output Value
    *)
    let classification_val valexp =  
      match valexp with
        C.IntV(i) ->  IntV(i)
      | C.Var(x) -> if List.mem x !var_list 
        then Var x 
        else if List.mem x !fun_list 
        then Fun x
        else err "Not_found"
    in
    (*cexpを変換する*)
    let rec convert_and_search_cexp cexp = 
      match cexp with 
        C.ValExp(t) -> ValExp(classification_val t)
      | C.AppExp(t1,t2) -> AppExp(classification_val t1, List.map classification_val t2)
      | C.BinOp(bop, t1, t2) -> BinOp(bop, classification_val t1, classification_val t2)
      | C.IfExp(t1, e1, e2) -> IfExp(classification_val t1, convert_and_search e1, convert_and_search e2)
      | C.TupleExp(l) -> TupleExp(List.map classification_val l)
      | C.ProjExp(t, i) -> ProjExp(classification_val t, i) 
    in
    match e with 
      C.LetRecExp(id1, id2, e1, e2) -> 
      fun_list :=  id1::!fun_list;
      var_list := List.rev_append id2 !var_list;
      let decl = RecDecl (id1, id2, copy_propagation(convert_and_search e1)) in
      decl_list := decl::!decl_list;
      convert_and_search e2
    | C.LetExp(id, cexp, e) -> 
      var_list := id::!var_list;
      LetExp(id, convert_and_search_cexp cexp, convert_and_search e)
    | C.LoopExp(id, cexp, e) -> 
      var_list := id::!var_list;
      LoopExp(id, convert_and_search_cexp cexp, convert_and_search e)
    | C.RecurExp(v1) -> 
      RecurExp(classification_val v1)
    | C.CompExp(cexp) -> CompExp(convert_and_search_cexp cexp)
  in
  let toplevel = RecDecl ("_toplevel", ["p0";"p1"], (copy_propagation( convert_and_search exp)))
  in
  toplevel::!decl_list
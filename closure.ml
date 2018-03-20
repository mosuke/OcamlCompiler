open Pretty
module S = Syntax
module N = Normal

exception Error of string
let err s = raise (Error s)

type id = N.id
type binOp = N.binOp

let fresh_id = N.fresh_id

(* ==== 値 ==== *)
type value =
    Var  of id
  | IntV of int

(* ==== 式 ==== *)
type cexp =
    ValExp    of value
  | BinOp     of binOp * value * value
  | AppExp    of value * value list     (* NEW *)
  | IfExp     of value * exp * exp
  | TupleExp  of value list             (* NEW *)
  | ProjExp   of value * int

and exp =
    CompExp   of cexp
  | LetExp    of id * cexp * exp
  | LetRecExp of id * id list * exp * exp  (* NEW *)
  | LoopExp   of id * cexp * exp
  | RecurExp  of value

(* ==== Formatter ==== *)

let string_of_closure e =
  let pr_of_op = function
      S.Plus -> text "+"
    | S.Mult -> text "*"
    | S.Lt -> text "<" in
  let pr_of_value = function
      Var id -> text id
    | IntV i ->
      let s = text (string_of_int i) in
      if i < 0 then text "(" <*> s <*> text ")" else s
  in
  let pr_of_values = function
      [] -> text "()"
    | v :: vs' ->
      (text "(" <*>
       (List.fold_left
          (fun d v -> d <*> text "," <+> pr_of_value v)
          (pr_of_value v) vs')
       <*> (text ")"))
  in
  let pr_of_ids = function
      [] -> text "()"
    | id :: ids' ->
      (text "(" <*>
       (List.fold_left
          (fun d i -> d <*> text "," <+> text i)
          (text id) ids')
       <*> (text ")"))
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
    | LetRecExp (id, parms, body, e) ->
      enclose 1
        ((nest 2 ( (text "let" <+> text "rec" <+> text id <+>
                    pr_of_ids parms <+>
                    text "=" <|> pr_of_exp 1 body)))
         <+> text "in" <|> pr_of_exp 1 e)
    | LoopExp (id, ce, e) ->
      enclose 1
        ((nest 2 ( (text "loop" <+> text id <+>
                    text "=" <|> pr_of_cexp 1 ce)))
         <+> text "in" <|> pr_of_exp 1 e)
    | RecurExp v ->
      enclose 3 (text "recur" <+> pr_of_value v)
  in layout (pretty 40 (pr_of_exp 0 e))

let rec print_list = function 
    [] -> ()
  | Var(e)::l -> print_string e ; print_string " " ; print_list l
  |_ -> ()

(* entry point *)
type environment = {
  mutable bound_var_list:(id*id list) list; (*束縛変数の集合 *)
}
(*差集合を取る関数
  m-n 
*)
let rec difference xs ys =
  match xs with
    [] -> []
  | z::zs -> if List.mem z ys then difference zs ys
    else z::difference zs ys

let set_of_list xs =
  let rec iter a = function
      [] -> (List.rev a)
    | y::ys -> if List.mem y ys then iter a ys else iter (y::a) ys
  in
  iter [] xs
  (*
    nomarl.valueをclosure.valueに変換する関数
  *)
let convert_value v = match v with 
    N.Var(i) -> Var(i)
  | N.IntV(i) -> IntV(i)

(*クロージャー変換を行う関数
  input nomal.exp
  output closure.exp
*)
let convert exp = 

  let env = {
    bound_var_list = [("top",[])];
  }
  in
  (*変換を行う関数
    input normal.exp 関数名
    output closure.exp
  *)
  let rec main e fun_name =
    (*****************************変数が定義されているものかのチェックを行う関数**********************************)
    let check x l f_name= 
      match x with 
        N.IntV(i) -> (IntV(i), [])
      | N.Var(i) -> 
        (convert_value x, [i])
    in
    (*****************************expの変換**********************************)
    match e with 
      N.LetRecExp(id1,id2,e1,e2) ->  
      let t = fresh_id ("b"^id1^"_")
      in
      (*
        環境変数を更新する
      *)
      let tmp' = List.assoc fun_name env.bound_var_list in
      let tmp2' = List.remove_assoc fun_name env.bound_var_list in
      env.bound_var_list <- (fun_name, id1::t::tmp')::tmp2';
      env.bound_var_list <- (id1, [t;id1;id2])::env.bound_var_list;
      let  (e1', var_list1) = main e1 id1
      in
      (*外側の関数の束縛変数と、内側の関数の自由変数との差集合を取得*)
      let free_vars = 
        let tmp =  
          difference var_list1 (List.assoc id1 env.bound_var_list)
        in
        set_of_list tmp
      in

      let (e2', var_list2) = main e2 fun_name
      in
      (*内側の関数で呼び出された自由変数がある場合はlet式で宣言する*)
      let
        declareded_free_vars = 
        List.rev (List.fold_left (fun x id -> Var(id)::x) [Var t] (free_vars))
      in
      (*自由変数の宣言*)
      let rec declared_free_vars l i= 
        match l with
          [] -> e1'
        | x::rest -> LetExp(x, ProjExp(Var id1, i), declared_free_vars rest (i+1))
      in
      (LetRecExp(t, 
                 id1::id2::[], 
                 declared_free_vars free_vars 1,
                 LetExp(id1, 
                        TupleExp(declareded_free_vars), 
                        e2'
                       )
                )
      , (t::id1::free_vars@var_list2))
    | N.LetExp(id1,cexp,e) -> 
      (*環境の更新*)
      let (_, tmp) = List.split env.bound_var_list in
      (if List.mem id1 (List.concat tmp) 
       then 
         ()
       else let tmp1 = List.assoc fun_name env.bound_var_list in
         let tmp2 = List.remove_assoc fun_name env.bound_var_list in
         env.bound_var_list <- (fun_name, id1::tmp1)::tmp2);
      (*関数の適応を開始*)
      let (cexp', vars1) = main (CompExp(cexp)) fun_name
      and (e', vars2) = main e fun_name
      in
      let tmp ce= LetExp(id1,
                         ce,
                         e')
      in
      (match cexp' with
         CompExp(ce) -> (tmp ce, 
                         id1::(List.rev_append vars1 vars2))
       | LetExp(id2, ce, CompExp(e2)) ->
         let tmp' = List.assoc fun_name env.bound_var_list in
         let tmp2' = List.remove_assoc fun_name env.bound_var_list in
         env.bound_var_list <- (fun_name, id1::tmp')::tmp2';
         (LetExp(id2, 
                 ce, 
                 tmp e2),
          id1::id2::(List.rev_append vars1 vars2)
         )
       |_ -> err "このパターン意外ありえない")

    | N.LoopExp(id1,cexp,e) -> 
      (*環境の更新*)
      let (_, tmp) = List.split env.bound_var_list in
      (if List.mem id1 (List.concat tmp) 
       then 
         ()
       else let tmp1 = List.assoc fun_name env.bound_var_list in
         let tmp2 = List.remove_assoc fun_name env.bound_var_list in
         env.bound_var_list <- (fun_name, id1::tmp1)::tmp2);
      (*関数の適応を開始*)
      let (cexp', vars1) = main (CompExp(cexp)) fun_name
      and (e', vars2) = main e fun_name
      in
      let tmp ce= LoopExp(id1,
                          ce,
                          e')
      in
      (match cexp' with
         CompExp(ce) -> (tmp ce, 
                         id1::List.rev_append vars1 vars2)
       | LetExp(id2, ce, CompExp(e2)) ->
         let tmp' = List.assoc fun_name env.bound_var_list in
         let tmp2' = List.remove_assoc fun_name env.bound_var_list in
         env.bound_var_list <- (fun_name, id1::tmp')::tmp2';
         (LetExp(id2, 
                 ce, 
                 tmp e2),
          id1::id2::List.rev_append vars1 vars2
         )
       |_ -> err "この��ターンのみ")

    | N.RecurExp(v) -> 
      let (v', var) = check v env.bound_var_list fun_name
      in
      (RecurExp(v'), var)
    | N.CompExp(cexp) -> 
      (*****************************cexpの変換**********************************)
      match cexp with 
        N.ValExp(IntV(i)) ->  (CompExp(ValExp((IntV i))),
                               [])
      | N.ValExp(Var(t)) -> 
        let (v, _) = check (N.Var t) env.bound_var_list fun_name
        in
        (CompExp
           (ValExp(v)),
         [t]
        )
      | N.IfExp(v, e1, e2) ->  
        let (e1', vars1)= main e1 fun_name
        and (e2', vars2) = main e2 fun_name
        and (v', var) = check v env.bound_var_list fun_name
        in
        (CompExp(IfExp(v', e1', e2')),
         var@(List.rev_append vars1 vars2))
      | N.BinOp(bop, t1,t2) ->
        let (t1', var1) = check t1 env.bound_var_list fun_name
        and (t2', var2) = check t2 env.bound_var_list fun_name
        in
        (CompExp(BinOp(bop,
                       t1',
                       t2')), 
         var1@var2
        )
      | N.AppExp(t1,t2) -> 
        let (t1', var1) = check t1 env.bound_var_list fun_name
        and (t2', var2) = check t2 env.bound_var_list fun_name
        in
        let r = fresh_id "r" in 
        let tmp' = List.assoc fun_name env.bound_var_list in
        let tmp2' = List.remove_assoc fun_name env.bound_var_list in
        env.bound_var_list <- (fun_name, r::tmp')::tmp2';        
        (LetExp(r,
                ProjExp(t1', 0),
                CompExp(AppExp(Var r, [t1';t2']))),
         r::var1@var2)
      | N.TupleExp (t1, t2) ->
        let (t1', var1) = check t1 env.bound_var_list fun_name
        and (t2', var2) = check t2 env.bound_var_list fun_name
        in
        (CompExp (TupleExp([t1';t2'])),var1@var2)
      | N.ProjExp(t, i) ->
        let (t', var1) = check t env.bound_var_list fun_name
        in
        (CompExp (ProjExp (t',i)),var1)
  in
  let (exp, _) = main exp "top"
  in exp
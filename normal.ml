open Pretty
module S = Syntax

exception Error of string
let err s = raise (Error s)

type id = S.id
type binOp = S.binOp

let fresh_id = Misc.fresh_id_maker "_"

(* ==== 値 ==== *)
type value =
    Var  of id
  | IntV of int

(* ==== 式 ==== *)
type cexp =
    ValExp    of value
  | BinOp     of binOp * value * value
  | AppExp    of value * value
  | IfExp     of value * exp * exp
  | TupleExp  of value * value
  | ProjExp   of value * int

and exp =
    CompExp   of cexp
  | LetExp    of id * cexp * exp
  | LetRecExp of id * id * exp * exp
  | LoopExp   of id * cexp * exp
  | RecurExp  of value

(* ==== Formatter ==== *)

let string_of_norm e =
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
  let rec pr_of_cexp p e =
    let enclose p' e = if p' < p then text "(" <*> e <*> text ")" else e in
    match e with
      ValExp v -> pr_of_value v
    | BinOp (op, v1, v2) ->
      enclose 2 (pr_of_value v1 <+> pr_of_op op <+> pr_of_value v2)
    | AppExp (f, v) ->
      enclose 3 (pr_of_value f <+> pr_of_value v)
    | IfExp (v, e1, e2) ->
      enclose 1
        ( ((nest 2
              ( ((text "if 0 <")
                 <+> pr_of_value v
                 <+> text "then"
                 <|> pr_of_exp 1 e1))) <|>
           (nest 2
              ( (text "else" <|> pr_of_exp 1 e2)))))
    | TupleExp (v1, v2) ->
      text "(" <*> pr_of_value v1 <*> text ","
      <+> pr_of_value v2 <*> text ")"
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
    | LetRecExp (id, v, body, e) ->
      enclose 1
        ((nest 2 ( (text "let" <+> text "rec" <+>
                    text id <+> text v <+>
                    text "=" <|> pr_of_exp 1 body)))
         <+> text "in" <|> pr_of_exp 1 e)
    | LoopExp (id, ce, e) ->
      enclose 1
        ((nest 2 ( (text "loop" <+> text id <+>
                    text "=" <|> pr_of_cexp 1 ce)))
         <+> text "in" <|> pr_of_exp 1 e)
    | RecurExp v ->
      enclose 3 (text "recur" <+> pr_of_value v)
  in layout (pretty 30 (pr_of_exp 0 e))


(* ==== 正規形への変換 ==== *)


(*正規化を行う関数*)
let n e =
  let rec normalize e =
    (*代入操作を行う行う関数*)
    (*　e中に現れるすべての自由変数xにe'を代入する  
        input e x(e中の自由変数) e' 
        output 代入された e  
    *)
    let rec substitution e x e' =  
      (*CompExpより値の取り出し*)
      let getCexp e = 
        match e with
          CompExp ce -> ce 
        |_ -> err "err1"
      in 
      let id_check v a = match v with 
          Var(id) -> if id = x then e' else a
        | IntV(i) -> a
      in
      match e' with 
        LetExp(a, e1, e2) -> LetExp(a, e1, substitution e x e2)
      | LetRecExp(id1, id2, e1, e2) -> LetRecExp(id1, id2, e1, substitution e x e2)
      | LoopExp(id, e1, e2) -> LoopExp(id, e1, substitution e x e2)
      | _ ->
        match e with 
          CompExp(ce) -> 
          (match ce with 
             ValExp(var) ->
             id_check var (CompExp(ValExp var))
           | IfExp(t, e1, e2) -> CompExp(IfExp(t, substitution e1 x e ,substitution e2 x e))
           | _ -> CompExp(ce))
        | RecurExp(e) -> RecurExp e
        | LetRecExp(id1, id2, e1, e2) -> 
          LetRecExp(id1, id2, substitution e1 x e', substitution e2 x e')
        | LetExp(id, e1, e2) -> 
          LetExp(id, getCexp (substitution (CompExp e1) x e'), substitution e2 x e')
        | LoopExp(id, e1, e2) ->
          LoopExp(id, getCexp (substitution (CompExp e1) x e'), substitution e2 x e')
    in
    (*cexp 型のコードしか埋め込めないところへ，exp 型の値を埋め込むための関数
      式eと関数kの２つを受取り、eがcexpかどうかに従っていずれかの代入を行う関数v
      v = if(eがcexpの時) k(e) else k()
      input e k
      output k(e) or let x = e in k(x)
    *) 
    let v e k =
      match e with 
        CompExp(ce) -> k ce
      | _ -> let x = fresh_id "_x_" 
        in  substitution (k (ValExp(Var x))) x e
    in
    (*無駄な束縛を減らすための関数
      in Exp, k(function)
      out Exp
    *)
    let v2 e k = 
      match e with 
        CompExp(ValExp(t)) -> k t
      | LetRecExp(id1, id2, e1, CompExp(ValExp(Var id3))) -> 
        if id1 = id3 then LetRecExp(id1, id2, e1, k (Var id1)) 
        else let x = fresh_id "_x_" 
          in v e (fun t -> LetExp(x, t, k (Var x)))
      |_ ->  let x = fresh_id "_x_" 
        in v e (fun t -> LetExp(x, t, k (Var x)))
    in
    match e with 
      Syntax.Var(id) -> 
      CompExp(ValExp(Var id))
    | Syntax.ILit(n) -> 
      CompExp(ValExp(IntV n))
    | Syntax.BLit(true) -> 
      CompExp(ValExp(IntV 1))
    | Syntax.BLit(false) -> 
      CompExp(ValExp(IntV 0))
    | Syntax.BinOp(bop, e1, e2) ->
      v2 (normalize e1) (fun x -> (v2 (normalize e2) (fun y -> CompExp(BinOp(bop, x, y)))))
    | Syntax.IfExp(e, e1, e2) -> 
      v2 (normalize e) (fun x -> CompExp(IfExp(x, normalize e1, normalize e2)))
    | Syntax.LetExp(id, e1, e2) -> 
      let k x = LetExp(id, x, normalize e2)
      in v (normalize e1) k
    | Syntax.LoopExp(id, e1, e2) -> 
      let k x = LoopExp(id, x, normalize e2)
      in v (normalize e1) k
    | Syntax.AppExp(e1,e2) -> 
      v2 (normalize e1) (fun x -> (v2 (normalize e2) (fun y -> CompExp(AppExp(x, y)))))
    | Syntax.RecurExp (e) -> 
      v2 (normalize e) (fun x -> RecurExp(x))
    | Syntax.TupleExp (e1,e2) -> 
      let t1 = fresh_id  "t1_" 
      in let t2 = fresh_id "t2_" 
      in v (normalize e1)
        (fun x -> LetExp(t1, x, v (normalize e2) 
                           (fun x -> LetExp(t2, x, CompExp(TupleExp(Var t1, Var t2))))))
    | Syntax.ProjExp(e,i) -> 
      let t1 = fresh_id"t1_" 
      in let k x = LetExp(t1, x, CompExp(ProjExp(Var t1, i-1)))
      in v (normalize e) k
    | Syntax.FunExp(id, e) -> 
      let t = fresh_id "t_" 
      in LetRecExp(t, id, normalize e, CompExp(ValExp(Var t)))
    | Syntax.LetRecExp (id1, id2, e1, e2) ->     
      LetRecExp(id1, id2, normalize e1, normalize e2)
  in normalize e
(* ==== recur式が末尾位置にのみ書かれていることを検査 ==== *)
let recur_check exp =
  let rec checker e in_end =
    match e with
      Syntax.LoopExp (id, e1, e2) -> checker e1 false; checker e2 true
    | Syntax.LetExp (id, e1, e2) ->
      checker e1 in_end; checker e2 in_end
    | Syntax.LetRecExp (id1, id2, e1, e2) ->
      checker e1 in_end; checker e2 in_end
    | Syntax.AppExp(e1,e2) ->
      checker e1 false; checker e2 false
    | Syntax.IfExp(e1, e2, e3) ->
      checker e2 in_end; checker e3 in_end
    | Syntax.RecurExp (e) -> if in_end = true then () else err "unexpected recur"
    | _ -> ()
  in 
  checker exp false 
(* ==== entry point ==== *)
let rec convert prog =
  recur_check prog ;
  n prog
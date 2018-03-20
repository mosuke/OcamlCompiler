module S = Syntax
module F = Flat

exception Error of string
let err s = raise (Error s)
let fresh_id = Normal.fresh_id

type binOp = S.binOp

type id = int
type label = string

type operand =
    Param of int     (* param(n) *)
  | Local of id      (* local(ofs) *)
  | Proc  of label   (* labimm(l) *)
  | IntV  of int     (* imm(n) *)

type instr =
    Move of id * operand (* local(ofs) <- op *)
  | BinOp of id * S.binOp * operand * operand
  (* local(ofs) <- bop(op_1, op_2) *)
  | Label of label (* l: *)
  | BranchIf of operand * label (* if op then goto l *) (* l: *)
  | Goto of label (* goto l *)
  | Call of id * operand * operand list
  (* local(ofs) <- call op_f(op_1, ..., op_n) *)
  | Return of operand (* return(op) *)
  | Malloc of id * operand list (* new ofs [op_1, ..., op_n] *)
  | Read of id * operand * int (* read ofs #i(op) *)
  | BEGIN of label (* データフロー解析で内部的に使用 *)
  | END of label   (* データフロー解析で内部的に使用 *)

type decl =
    ProcDecl of label * int * instr list  (* int は局所変数の個数 *)

type prog = decl list

(* ==== Formatter ==== *)

let string_of_binop = function
    S.Plus -> "add"
  | S.Mult -> "mul"
  | S.Lt   -> "lt"

let string_of_operand = function
    Param i -> "p" ^ string_of_int i
  | Local o -> (* -1 は生存変数解析で使われる特殊な値 *)
    if o = -1 then "*" else "t" ^ string_of_int o
  | Proc  l -> l
  | IntV  i -> string_of_int i

let string_of_instr idt tab = function
    Move (t, v) ->
    idt ^ "move" ^ tab ^ "t" ^ string_of_int t ^ ", " ^
    string_of_operand v
  | BinOp (t, op, v1, v2) ->
    idt ^ string_of_binop op ^ tab ^ "t" ^ string_of_int t ^ ", " ^
    string_of_operand v1 ^ ", " ^ string_of_operand v2
  | Label lbl -> lbl ^ ":"
  | BranchIf (v, lbl) ->
    idt ^ "bif" ^ tab ^ string_of_operand v ^ ", " ^ lbl
  | Goto lbl ->
    idt ^ "goto" ^ tab ^ lbl
  | Call (dst, tgt, [a0; a1]) ->
    idt ^ "call" ^ tab ^ "t" ^ string_of_int dst ^ ", " ^
    string_of_operand tgt ^
    "(" ^ string_of_operand a0 ^ ", " ^ string_of_operand a1 ^ ")"
  | Call (_, _, args) -> err ("Illegal number of arguments: " ^
                              string_of_int (List.length args))
  | Return v ->
    idt ^ "ret" ^ tab ^ string_of_operand v
  | Malloc (t, vs) ->
    idt ^ "new" ^ tab ^ "t" ^ string_of_int t ^ " [" ^
    (String.concat ", " (List.map string_of_operand vs)) ^ "]"
  | Read (t, v, i) ->
    idt ^ "read" ^ tab ^ "t" ^ string_of_int t ^ " #" ^
    string_of_int i ^ "(" ^ string_of_operand v ^ ")"
  | BEGIN lbl ->
    idt ^ "<BEGIN: " ^ lbl ^ ">"
  | END lbl ->
    idt ^ "<END: " ^ lbl ^ ">"

let string_of_decl (ProcDecl (lbl, n, instrs)) =
  "proc " ^ lbl ^ "(" ^ string_of_int n ^ ") =\n" ^
  (String.concat "\n"
     (List.map (fun i -> string_of_instr "  " "\t" i) instrs)) ^ "\n"

let string_of_vm prog =
  String.concat "\n" (List.map string_of_decl prog)


(* ==== 仮想機械コードへの変換 ==== *)
(*環境変数の定義
  result: expを変換したinstrを蓄えておくlist
  counter: 束縛変数が現れた回数を数えるカウンター
*)
type env = {mutable result : instr list; mutable counter : int }
let trans_decl (F.RecDecl (proc_name, params, body)) =
  (*
    変数の記憶領域が d に書いてあると仮定 して e を評価した結果を tgt に格納する
    expをinstrに変換し、resultに足していく
  *)
  let env = {result = []; counter = 0} in
  let  add x = env.result <- x ::env.result 

  and get t d=   match t with 
      F.Var i ->
      begin 
        try List.assoc i d  with 
        | Not_found  -> err ("変数"^i^"が見つからない")
      end

    | F.Fun i -> 
      begin
        try
          List.assoc i d 
        with 
        | Not_found  -> Proc i  
      end
    | F.IntV i -> IntV i

  in 
  let rec convert ex (*部分関数*)d tgt (*loopのあとに付けるラベル*)l (*直前のループが束縛する変数のid*)loop_id = 
    begin
      match ex with 
      (************************cexpの変換*************************)
        F.CompExp(cexp) -> 
        begin 
          match cexp with 
            F.ValExp(Fun t) -> 
            begin
              try
                add (Move(tgt, List.assoc t d))
              with 
              | Not_found  -> add(Malloc(tgt,
                                         [Proc t]))
            end
          | F.ValExp(t) -> add (Move(tgt, get t d))
          | F.BinOp(bop, t1, t2) -> add (BinOp(tgt, bop, get t1 d, get t2 d))
          | F.IfExp(t, e1, e2) -> 
            let l1 = fresh_id ("lb"^proc_name^"_")
            and l2 = fresh_id ("lb"^proc_name^"_")
            in  
            add (BranchIf(get t d, l1));
            convert e2 d tgt l loop_id;
            add (Goto l2);
            add (Label l1);
            convert e1 d tgt l loop_id;
            add (Label l2)
          | F.AppExp(t1, l) ->
            add (Call(tgt,
                      get t1 d, 
                      List.map(fun x -> get x d) l))
          | F.TupleExp(l) ->
            add (Malloc(tgt, 
                        List.map(
                          fun x -> match x with F.Fun t -> Proc t |_ -> get x d 
                        ) l))
          | F.ProjExp(t, i) -> add (Read(tgt, get t d,i))
        end
      (**************************expの変換*************************)
      | F.LetExp (id, cexp, e) -> 
        let t' = env.counter <- 1 + env.counter; env.counter - 1 
        in 
        let d' = (id, Local t')::d in 
        convert (CompExp(cexp)) d' t' l loop_id;
        convert e d' tgt l loop_id
      | F.LoopExp (id, cexp, e) ->
        let t' = env.counter <- 1 + env.counter; env.counter - 1 
        and l' = fresh_id "l"
        in 
        let d' = (id, Local t')::d in 
        convert (CompExp(cexp)) d' t' l loop_id;
        add(Label l');
        convert e d' tgt l' t';
      | F.RecurExp(t) ->
        add (Move(loop_id, get t d));
        add(Goto l)
    end
  in
  let t0 = env.counter <- 1 + env.counter; env.counter - 1 
  in 
  convert body (List.combine params [Param(0);Param(1)]) t0 "dummy" (-1);
  add (Return(Local t0));
  ProcDecl (proc_name, env.counter,
            List.rev env.result)

(* entry point *)
let trans = List.map trans_decl

module S = Syntax
open Arm_spec

exception Error of string
let err s = raise (Error s)
let fresh_id = Normal.fresh_id 

(* ==== メモリアクセス関連 ==== *)

(* 「reg中のアドレスからoffsetワード目」をあらわすaddr *)
let mem_access reg offset = RI (reg, offset * 4)

let local_access i = mem_access Fp (-i-2)

(* Vm.Param は 0 から数えるものと仮定 *)
let param_to_reg = function
    0 -> A1
  | 1 -> A2
  | i -> err ("invalid parameter: " ^ string_of_int i)

(* Vm.operandから値を取得し，レジスタrdに格納するような
   stmt listを生成 *)
let gen_operand rd = function
    Vm.Param i ->
    let rs = param_to_reg i in
    if rd = rs then [] else [Instr (Mov (rd, (R rs)))]
  | Vm.Local i -> [Instr (Ldr (rd, local_access i))]
  | Vm.Proc  lbl -> [Instr (Ldr (rd, L lbl))]
  | Vm.IntV  i -> [Instr (Mov (rd, I i))]

(* ==== 仮想マシンコード --> アセンブリコード ==== *)

(* V.decl -> loc list *)
let gen_decl (Vm.ProcDecl (name, nlocal, instrs)) =
  (*
    (Vm.id*addr)のリスト 
  *)
  let locations_of_local_vars = 
    let rec make n i l = 
      if i < n then make n (i + 1) ((i, RI(Fp, -(8+i*4)))::l) 
      else l in make nlocal 0 [(0, RI(Fp, -8))]
  in
  (*operandに応じて、regにロードする関数*)
  let store_reg_with_op op reg= 
    match op with  
      Vm.Local i -> Instr (Ldr (reg, List.assoc i locations_of_local_vars))
    | Vm.Proc i -> Instr (Ldr (reg, L i))
    | Vm.Param i -> if i = 0 then Instr (Mov (reg, R A1)) else  Instr (Mov (reg, R A2))
    | Vm.IntV i ->  Instr (Mov (reg, I i))
  in
  (*vmporg.instrをarm_noreg.stmt listに変換する関数*)
  let convert vmprog =
    match vmprog with 
      Vm.Move (id, op) -> 
      let r = V1 in
      [
        store_reg_with_op op r;
        Instr (Str (r, List.assoc id locations_of_local_vars))
      ]
    | Vm.BinOp(id, S.Lt, op1, op2) ->  
      let label1 = fresh_id ("llb"^name^"_")
      and label2 = fresh_id ("llb"^name^"_") 
      and r1 = V1 and r2 = V2 in
      [
        store_reg_with_op op1 r1;
        store_reg_with_op op2 r2;
        Instr (Cmp (r1, R r2));
        Instr (Blt label1);
        Instr (Mov (r1, I 0));
        Instr (B label2);
        Label label1;
        Instr (Mov (r1, I 1));
        Label label2;
        Instr (Str (r1, List.assoc id locations_of_local_vars))
      ]
    | Vm.BinOp(id, bop, op1, op2) ->  
      let r1 = V1 and r2 = V2 in
      let b = if bop = S.Plus then Add (r1, r1, R r2)else Mul (r1, r1, R r2)in
      [
        store_reg_with_op op1 r1;
        store_reg_with_op op2 r2;
        Instr (b);
        Instr (Str (r1, List.assoc id locations_of_local_vars))
      ]
    | Vm.BranchIf(op, label) -> 
      let r = V1 in
      [
        store_reg_with_op op r;
        Instr (Cmp (r, I 0));
        Instr (Bne label)
      ]
    | Vm.Read(id, op, i) -> 
      let r = V1 in
      [
        store_reg_with_op op r;
        Instr (Ldr (r, RI(r, 4*i)));
        Instr (Str (r, List.assoc id locations_of_local_vars))
      ]
    | Vm.Call(id, opf, [op1; op2]) -> 
      let r = V1 in
      [
        Instr (Str (A1, RI(Sp, 0)));(*⟨a1，a2 レジスタを所定の位置に退避 ⟩*)
        Instr (Str (A2, RI(Sp, 4)));
        store_reg_with_op op1 A1;(*⟨op1 op2 を a1 a2レジスタに格納 ⟩*)
        store_reg_with_op op2 A2;
        store_reg_with_op opf r;
        Instr (Blx r);
        Instr (Str (A1, List.assoc id locations_of_local_vars));
        Instr (Ldr (A1, RI(Sp, 0)));
        Instr (Ldr (A2, RI(Sp, 4)))
      ] 
    | Vm.Call(_,_, l) -> err "引数は２つのはず"
    | Vm.Goto (l) -> 
      [
        Instr (B l)
      ]
    | Vm.Label (l) -> 
      [
        Label l;
      ]
    | Vm.Return (op) -> 
      [
        store_reg_with_op op A1;
        Instr (B (name ^ "_ret"));
      ]
    | Vm.Malloc(id, l) -> 
      let r1 = V1 and r2 = V2 
      and address = List.assoc id locations_of_local_vars
      and n = List.length l
      in
      (*lの中身をr1に
          まずlの内容をr1に移
          次に、r1をr2+index*4にストアする
      *)
      let bottom = 
        let rec calc k i= 
          match k with 
            [] -> []
          | x::rest -> (store_reg_with_op x r1)
                       ::(Instr (Str (r1, RI(r2, 4*i))))
                       ::(calc rest (i+1))
        in calc l 0
      in
      [
        Instr (Str (A1, RI(Sp, 0)));(*a1，a2 レジスタを所定の位置に退避*)
        Instr (Str (A2, RI(Sp, 4)));
        store_reg_with_op (IntV(n)) A1;(*mymallocの引数*)
        Instr (Bl "mymalloc");(*mymallocの呼び出し*)
        Instr (Str (A1, address));(*addressにA１の内容を退避*)
        Instr (Ldr (A1, RI(Sp, 0)));(*a1，a2 レジスタを所定の位置から復帰*)
        Instr (Ldr (A2, RI(Sp, 4)));
        Instr (Ldr (r2, address));
      ] 
      @ bottom
    |_ -> err "BEGINとENDは使われない"
  in
  let tmp = 
    [
      Label (name ^ "_ret");
      Instr (Add (Sp, Fp, I 4));
      Instr (Ldr (Lr, RI (Fp, -4)));
      Instr (Ldr (Fp, RI (Fp, 0)));
      Instr (Bx Lr);
    ]
  and tmp2 = [Dir (D_align 2);
              Dir (D_global name);
              Label name;
              Instr (Str (Fp, RI (Sp, -4)));
              Instr (Str (Lr, RI (Sp, -8)));
              Instr (Sub (Fp, Sp, I (4)));
              Instr (Sub (Sp, Sp, I (16 + 4 * nlocal)));]
  in
  let result = List.concat (tmp2::List.fold_left (fun x y -> (convert y)::x) [tmp] (List.rev instrs))
  in List.fold_right (fun x y -> if x = Instr (Mov (A1, R A1))|| x = Instr ( Mov(A2, R A2)) then y else x::y) result []

(* entry point: Vm.decl list -> stmt list *)
let codegen vmprog =
  Dir D_text :: (List.concat (List.rev  (List.map gen_decl vmprog)))
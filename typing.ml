open Syntax

(* 型を同じにできない場合に起きる例外 *)
exception Unify of Type.t * Type.t

(* r が型 ty に現れるかをチェックする (occur check) *)
(* occur : Type.t option ref -> Type.t -> bool *)
let rec occur r ty = match ty with
    Type.TInt -> false
  | Type.TBool -> false
  | Type.TFun (ty1, ty2) -> occur r ty1 || occur r ty2
  | Type.TVar(r') ->
      if r == r' then true
      else begin match !r' with
          None -> false
        | Some (ty') -> occur r ty'
      end
  | Type.TList(l) -> false
        
(* ty1 = ty2 となるように、型変数への代入をする *)
(* unify : Type.t -> Type.t -> unit *)
let rec unify ty1 ty2 = match (ty1, ty2) with
    (Type.TInt, Type.TInt) -> ()
  | (Type.TBool, Type.TBool) -> ()
  | (Type.TFun (ty1, ty1'), Type.TFun (ty2, ty2')) ->
      begin
        unify ty1 ty2;
        unify ty1' ty2'
      end
  | (Type.TVar (r1), Type.TVar (r2)) when r1 == r2 -> ()
  | (Type.TVar (r1), _) ->
      begin match !r1 with
          None -> if occur r1 ty2 then raise (Unify (ty1, ty2))
                                  else r1 := Some (ty2)
        | Some (ty1') -> unify ty1' ty2
      end
  | (_, Type.TVar (r2)) ->
      begin match !r2 with
          None -> if occur r2 ty1 then raise (Unify (ty1, ty2))
                                  else r2 := Some (ty1)
        | Some (ty2') -> unify ty1 ty2'
      end
  | (_, _) -> raise (Unify (ty1, ty2))

(* 型推論 *)
(* g : Syntax.t -> (string, Type.t) Env.t -> Type.t *)
let rec g expr tenv =
  try
    begin match expr with
        Number (num) -> Type.TInt
      | Bool (b) -> Type.TBool
      | Var (s) ->  begin try Env.get tenv s with
        Not_found -> failwith ("Unbound variable: " ^ s)
        end
      | Nil ->  Type.TList([])
      | Op (t1, op, t2) ->
          begin match op with
              Plus | Minus | Times | Divide ->
                let ty1 = g t1 tenv in
                let ty2 = g t2 tenv in
                unify ty1 Type.TInt;
                unify ty2 Type.TInt;
                Type.TInt;
            | Equal | Less | More -> 
              let ty1 = g t1 tenv in
                  let ty2 = g t2 tenv in
                  unify ty1 Type.TInt;
                  unify ty2 Type.TInt;
                  Type.TBool; 
            | _ -> failwith ("未サポート：" ^ Syntax.to_string expr)
          end
      | OpIf (t1, t2, t3) -> 
          let ty1 = g t1 tenv in
            unify ty1 Type.TBool;
            let ty2 = g t2 tenv in
            let ty3 = g t3 tenv in
              unify ty2 ty3; ty2;
      | Let (x, t1, t2) ->
          let ty1 = g t1 tenv in
          let tenv' = Env.extend tenv x ty1 in
          let ty2 = g t2 tenv' in ty2
      | Letrec (f, x, t1, t2) -> 
          let tyx = Type.gen_type () in 
          let tenv1 = Env.extend tenv x tyx in
          let ty1 = Type.gen_type () in
          let tyf = Type.TFun (tyx, ty1) in 
          let tenv1 = Env.extend tenv1 f tyf in 
          let ty2 = g t2 tenv1 in ty2
      | Fun (x, t) ->
          let ty1 = Type.gen_type () in
          let tenv' = Env.extend tenv x ty1 in
          let ty2 = g t tenv' in
          Type.TFun (ty1, ty2)
      | App (t1, t2) ->
          let ty1 = g t1 tenv in
          let ty2 = g t2 tenv in
          begin match ty1 with
            Type.TFun (x, t) -> unify x ty2; t
            |  _ -> failwith ("Failed function：" ^ Syntax.to_string expr)
          end
      | Cons (t1, t2) -> 
          let ty1 = g t1 tenv in
          let ty2 = g t2 tenv in
          begin match ty2 with
            Type.TList(l) -> 
              begin match l with
                first :: rest -> unify ty1 first; Type.TList([ty1]);
                | [] -> Type.TList([ty1])
              end
            | _ -> failwith ("Failed cons:" ^ Syntax.to_string expr)
          end;
      | Match (arg1, arg2, x, y, arg3) -> failwith ("未サポート：" ^ Syntax.to_string expr)
      | Raise (t1) -> 
          let ty1 = g t1 tenv in 
          unify ty1 Type.TInt;
          Type.TInt;
      | Try (arg1, x, arg2) -> failwith ("未サポート：" ^ Syntax.to_string expr)
    end
  with Unify (ty1, ty2) -> begin (* unify できなかった *)
    print_endline "式";
    print_string "	";
    Syntax.print expr;
    print_newline ();
    print_endline "を型推論中に型エラーがおきました。";
    print_string "	";
    Type.print ty1;
    print_newline ();
    print_endline "と";
    print_string "	";
    Type.print ty2;
    print_newline ();
    print_endline "は unify できません。";
    exit 0
  end

(* 型推論の入り口 *)
(* Typing.f : Syntax.t -> Type.t *)
let f expr =
  let ty = g expr Env.empty in
  ty

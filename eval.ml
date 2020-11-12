open Syntax
open Value
open Env

(* 実際の計算をする関数 *)
(* Eval.f : Syntax.t -> Value.t *)
let rec f expr env = match expr with
    Number (n) -> VNumber (n)
  | Bool (b) -> VBool (b)
  | Var (s) ->  begin try Env.get env s with
                    Not_found -> failwith ("Unbound variable: " ^ s)
                end
  | Op (arg1, Plus, arg2) ->
      let v1 = f arg1 env in
      let v2 = f arg2 env in
      begin match (v1, v2) with
          (VNumber (n1), VNumber (n2)) -> VNumber (n1 + n2)
        | (_, _) -> failwith ("Bad arguments to +: " ^
                              Value.to_string v1 env ^ ", " ^
                              Value.to_string v2 env)
      end
  | Op (arg1, Minus, arg2) ->
      let v1 = f arg1 env in
      let v2 = f arg2 env in
      begin match (v1, v2) with
          (VNumber (n1), VNumber (n2)) -> VNumber (n1 - n2)
        | (_, _) -> failwith ("Bad arguments to -: " ^
                              Value.to_string v1 env ^ ", " ^
                              Value.to_string v2 env)
      end
  | Op (arg1, Times, arg2) ->
      let v1 = f arg1 env in
      let v2 = f arg2 env in
      begin match (v1, v2) with
          (VNumber (n1), VNumber (n2)) -> VNumber (n1 * n2)
        | (_, _) -> failwith ("Bad arguments to *: " ^
                              Value.to_string v1 env ^ ", " ^
                              Value.to_string v2 env)
      end
  | Op (arg1, Equal, arg2) ->
      let v1 = f arg1 env in
      let v2 = f arg2 env in
      begin match (v1, v2) with
          (VNumber (n1), VNumber (n2)) -> if n1 == n2 then VBool(true) else VBool(false)
        | (VBool(b1), VBool(b2)) -> if b1 == b2 then VBool(true) else VBool(false)
        | (_, _) -> failwith ("Bad arguments to =: " ^
                              Value.to_string v1 env ^ ", " ^
                              Value.to_string v2 env)
      end
  | Op (arg1, Less, arg2) ->
      let v1 = f arg1 env in
      let v2 = f arg2 env in
      begin match (v1, v2) with
          (VNumber (n1), VNumber (n2)) -> if n1 < n2 then VBool(true) else VBool(false)
        | (_, _) -> failwith ("Bad arguments to <: " ^
                              Value.to_string v1 env ^ ", " ^
                              Value.to_string v2 env)
      end
    (* parserで逆になっている *)
  | Op (arg1, More, arg2) ->
      let v1 = f arg1 env in
      let v2 = f arg2 env in
      begin match (v1, v2) with
          (VNumber (n1), VNumber (n2)) -> if n1 < n2 then VBool(true) else VBool(false)
        | (_, _) -> failwith ("Bad arguments to <: " ^
                              Value.to_string v1 env ^ ", " ^
                              Value.to_string v2 env)
      end
  | OpIf (arg1, arg2, arg3) -> 
      let b1 = f arg1 env in
      begin match (b1) with
        VBool (b1) ->
            let v2 = f arg2 env in 
            let v3 = f arg3 env in
            begin match (v2, v3) with
                (VNumber (v2), VNumber(v3)) -> if b1 then VNumber (v2) else VNumber(v3)
                | (VNumber (v2), VBool(v3)) -> if b1 then VNumber (v2) else VBool(v3)
                | (VBool (v2), VNumber(v3)) -> if b1 then VBool (v2) else VNumber(v3)
                | (VBool (v2), VBool(v3)) -> if b1 then VBool (v2) else VBool(v3)
                | (_, _) -> failwith ("If.statement match error")
                end
        | (_) -> failwith ("Predicate is not a boolean: " ^ Value.to_string b1 env)
      end
  | Let (x, arg2, arg3) ->
      let v2 = f arg2 env in
      let env1 = Env.extend env x v2 in f arg3 env1
  | Fun (x, arg1) -> Clo(x, arg1, env) 
  | App (arg1, arg2) -> 
        let v1 = f arg1 env in 
        let v2 = f arg2 env in
        begin match v1 with
            Clo(x, t, env1) -> 
                let env2 = Env.extend env1 x v2 in f t env2
            | (_) -> failwith ("Not a function: " ^ Value.to_string v1 env )
        end
  | Op (_, _, _) -> failwith ("Parse.fail op")
  | Letrec(_, _, _, _) -> failwith ("Rec is not implemented yet")

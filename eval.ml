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
  | Nil ->  VList ([])
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
            if b1 then f arg2 env else f arg3 env
        | (_) -> failwith ("Predicate is not a boolean: " ^ Value.to_string b1 env)
      end
  | Let (x, arg2, arg3) ->
      let v2 = f arg2 env in
      let env1 = Env.extend env x v2 in f arg3 env1
  | Letrec (g, x, arg1, arg2) -> 
      let env1 = Env.extend env g (CloR (g, x, arg1, env)) in f arg2 env1
  | Fun (x, arg1) -> Clo(x, arg1, env) 
  | App (arg1, arg2) -> 
        let v1 = f arg1 env in 
        let v2 = f arg2 env in
        begin match v1 with
            Clo(x, t, env1) -> 
                let env2 = Env.extend env1 x v2 in f t env2
            | CloR (g, x, t, env1) ->
                let env2 = Env.extend env1 g (CloR (g, x, t, env1)) in
                let env3 = Env.extend env2 x v2 in f t env3
            | (_) -> failwith ("Not a function: " ^ Value.to_string v1 env )
        end
  | Cons (arg1, arg2) -> 
        let v1 = f arg1 env in
        let v2 = f arg2 env in
        begin match v2  with
            VList (lst) -> VList(v1::lst)
            | (_) -> failwith ("Not a list:" ^ Value.to_string v1 env)
        end
  | Match (arg1, arg2, x, y, arg3) -> 
        let v1 = f arg1 env in
        begin match v1 with 
            VList([]) -> f arg2 env 
            | VList(first::rest) -> 
                let env1 = Env.extend env x first in
                let env2 = Env.extend env1 y (VList(rest)) in f arg3 env2
            | (_) -> failwith ("Not match a list:" ^ Syntax.to_string arg1^" "^Syntax.to_string arg2^" "^x^" "^y^" "^Syntax.to_string arg3)
        end
  | Op (_, _, _) -> failwith ("Parse.fail op")

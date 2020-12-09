open Syntax
open Value
open Env

(* 実際の計算をする関数 *)
(* Eval.f : Syntax.t -> Value.t *)
let rec f expr env cont = match expr with
    Number (n) -> cont (VNumber(n))
  | Bool (b) ->  cont (VBool(b))
  | Var (s) ->  begin try cont(Env.get env s) with
                    Not_found -> cont (failwith ("Unbound variable: " ^ s))
                end
  | Nil ->  VList ([])
  | Op (arg1, Plus, arg2) ->
      f arg1 env (fun x -> 
      f arg2 env (fun y -> 
        begin match (x, y) with
          (VNumber (n1), VNumber (n2)) -> cont (VNumber (n1 + n2))
          | (_, _) -> cont (failwith ("Bad arguments to +: " ^
                            Value.to_string x env ^ ", " ^
                            Value.to_string y env ))
      end))
  | Op (arg1, Minus, arg2) ->
      f arg1 env (fun x -> 
      f arg2 env (fun y -> 
        begin match (x, y) with
          (VNumber (n1), VNumber (n2)) -> cont (VNumber (n1 - n2))
          | (_, _) -> cont (failwith ("Bad arguments to -: " ^
                            Value.to_string x env ^ ", " ^
                            Value.to_string y env ))
      end))
  | Op (arg1, Times, arg2) ->
      f arg1 env (fun x -> 
      f arg2 env (fun y -> 
        begin match (x, y) with
          (VNumber (n1), VNumber (n2)) -> cont (VNumber (n1 * n2))
          | (_, _) -> cont (failwith ("Bad arguments to *: " ^
                            Value.to_string x env ^ ", " ^
                            Value.to_string y env ))
      end))
  | Op (arg1, Equal, arg2) ->
      f arg1 env (fun x -> 
      f arg2 env (fun y -> 
        begin match (x, y) with
          (VNumber (n1), VNumber (n2)) -> if n1 == n2 then cont (VBool(true)) else cont(VBool(false))
          | (VBool (n1), VBool (n2)) -> if n1 == n2 then cont (VBool(true)) else cont(VBool(false))
          | (_, _) -> cont (failwith ("Bad arguments to =: " ^
                            Value.to_string x env ^ ", " ^
                            Value.to_string y env ))
        end))
  | Op (arg1, Less, arg2) ->
        f arg1 env (fun x -> 
        f arg2 env (fun y -> 
            begin match (x, y) with
            (VNumber (n1), VNumber (n2)) -> if n1 < n2 then cont (VBool(true)) else cont(VBool(false))
            | (_, _) -> cont (failwith ("Bad arguments to <: " ^
                                Value.to_string x env ^ ", " ^
                                Value.to_string y env ))
            end))
    (* parserで逆になっている *)
  | Op (arg1, More, arg2) ->
        f arg1 env (fun x -> 
        f arg2 env (fun y -> 
            begin match (x, y) with
            (VNumber (n1), VNumber (n2)) -> if n1 < n2 then cont (VBool(true)) else cont(VBool(false))
            | (_, _) -> cont (failwith ("Bad arguments to <: " ^
                                Value.to_string x env ^ ", " ^
                                Value.to_string y env ))
            end))
  | OpIf (arg1, arg2, arg3) -> 
      f arg1 env (fun x -> 
      begin match x with
      VBool (x) ->
            if x then f arg2 env cont else f arg3 env cont
        | (_) -> cont(failwith ("Predicate is not a boolean: " ^ Value.to_string x env))
      end)
  | Let (x, arg2, arg3) ->
      f arg2 env (fun v2 ->
      let env1 = Env.extend env x v2 in f arg3 env1 cont
      )
  | Letrec (g, x, arg1, arg2) -> 
      let env1 = Env.extend env g (CloR (g, x, arg1, env)) in f arg2 env1 cont
  | Fun (x, arg1) -> cont (Clo(x, arg1, env) )
  | App (arg1, arg2) -> 
        let v1 = f arg1 env cont in 
        let v2 = f arg2 env cont in
        begin match v1 with
            Clo(x, t, env1) -> 
                let env2 = Env.extend env1 x v2 in f t env2 cont
            | CloR (g, x, t, env1) ->
                let env2 = Env.extend env1 g (CloR (g, x, t, env1)) in
                let env3 = Env.extend env2 x v2 in f t env3 cont
            | (_) -> cont(failwith ("Not a function: " ^ Value.to_string v1 env ))
        end
  | Cons (arg1, arg2) -> 
        let v1 = f arg1 env cont in
        let v2 = f arg2 env cont in
        begin match v2  with
            VList (lst) -> VList(v1::lst)
            | (_) -> cont(failwith ("Not a list:" ^ Value.to_string v1 env))
        end
  | Match (arg1, arg2, x, y, arg3) -> 
        let v1 = f arg1 env cont in
        begin match v1 with 
            VList([]) -> f arg2 env cont
            | VList(first::rest) -> 
                let env1 = Env.extend env x first in
                let env2 = Env.extend env1 y (VList(rest)) in f arg3 env2 cont
            | (_) -> cont(failwith ("Not a list: " ^ Value.to_string v1 env))
        end
  | Op (_, _, _) -> cont(failwith ("Parse.fail op"))

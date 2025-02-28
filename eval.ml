open Syntax
open Value
open Env

(* 実際の計算をする関数 *)
(* Eval.f : Syntax.t -> Value.t *)
let rec f expr env cont = match expr with
  | Number (n) -> cont (Ok(VNumber(n)))
  | Bool (b) ->  cont (Ok(VBool(b)))
  | Var (s) ->  begin try cont(Ok(Env.get env s)) with
                    Not_found -> failwith ("Unbound variable: " ^ s)
                end
  | Nil ->  cont(Ok(VList ([])))
  | Op (arg1, Plus, arg2) ->
      f arg1 env (fun x -> 
      f arg2 env (fun y -> 
        begin match (x, y) with
          (Ok(VNumber (n1)), Ok(VNumber (n2))) -> cont (Ok(VNumber (n1 + n2)))
          | (Error(VNumber (n1)), Ok(VNumber (n2))) -> cont (Error(VNumber (n1)))
          | (Ok(VNumber (n1)), Error(VNumber (n2))) -> cont (Error(VNumber (n2)))
          | (_, _) ->  failwith ("Bad arguments to +: " ^
                            Value.to_string x ^ ", " ^
                            Value.to_string y )
      end))
  | Op (arg1, Minus, arg2) ->
      f arg1 env (fun x -> 
      f arg2 env (fun y -> 
        begin match (x, y) with
          (Ok(VNumber (n1)), Ok(VNumber (n2))) -> cont (Ok(VNumber (n1 - n2)))
          | (Error(VNumber (n1)), Ok(VNumber (n2))) -> cont (Error(VNumber (n1)))
          | (Ok(VNumber (n1)), Error(VNumber (n2))) -> cont (Error(VNumber (n2)))
          | (_, _) ->  failwith ("Bad arguments to -: " ^
                            Value.to_string x ^ ", " ^
                            Value.to_string y )
      end))
  | Op (arg1, Times, arg2) ->
      f arg1 env (fun x -> 
      f arg2 env (fun y -> 
        begin match (x, y) with
          (Ok(VNumber (n1)), Ok(VNumber (n2))) -> cont (Ok(VNumber (n1 * n2)))
          | (Error(VNumber (n1)), Ok(VNumber (n2))) -> cont (Error(VNumber (n1)))
          | (Ok(VNumber (n1)), Error(VNumber (n2))) -> cont (Error(VNumber (n2)))
          | (_, _) -> failwith ("Bad arguments to *: " ^
                            Value.to_string x ^ ", " ^
                            Value.to_string y )
      end))
  | Op (arg1, Divide, arg2) -> 
      f arg1 env (fun x -> 
      f arg2 env (fun y -> 
        begin match (x, y) with
            (Ok(VNumber (n1)), Ok(VNumber (n2))) -> 
                if n2 == 0 then cont(Error(VNumber(0)))
                else cont (Ok(VNumber (n1 / n2)))
            | (Error(VNumber (n1)), Ok(VNumber (n2))) -> cont (Error(VNumber (n1)))
            | (Ok(VNumber (n1)), Error(VNumber (n2))) -> cont (Error(VNumber (n2)))
            | (_, _) -> failwith ("Bad arguments to /: " ^
                            Value.to_string x ^ ", " ^
                            Value.to_string y )
      end))
  | Op (arg1, Equal, arg2) ->
      f arg1 env (fun x -> 
      f arg2 env (fun y -> 
        begin match (x, y) with
            (Error(VNumber (n1)), Ok(VNumber (n2))) -> cont (Error(VNumber (n1)))
            | (Ok(VNumber (n1)), Error(VNumber (n2))) -> cont (Error(VNumber (n2)))
            | (Ok(VNumber (n1)), Ok(VNumber (n2))) -> if n1 == n2 then cont (Ok(VBool(true))) else cont(Ok(VBool(false)))
            | (Ok(VBool (n1)), Ok(VBool (n2))) -> if n1 == n2 then cont (Ok(VBool(true))) else cont(Ok(VBool(false)))
            | (_, _) -> cont (failwith ("Bad arguments to =: " ^
                            Value.to_string x ^ ", " ^
                            Value.to_string y ))
        end))
  | Op (arg1, Less, arg2) ->
        f arg1 env (fun x -> 
        f arg2 env (fun y -> 
            begin match (x, y) with
            (Error(VNumber (n1)), Ok(VNumber (n2))) -> cont (Error(VNumber (n1)))
            | (Ok(VNumber (n1)), Error(VNumber (n2))) -> cont (Error(VNumber (n2)))
            | (Ok(VNumber (n1)), Ok(VNumber (n2))) -> if n1 < n2 then cont (Ok(VBool(true))) else cont(Ok(VBool(false)))
            | (_, _) -> cont (failwith ("Bad arguments to <: " ^
                                Value.to_string x ^ ", " ^
                                Value.to_string y ))
            end))
    (* parserで逆になっている *)
  | Op (arg1, More, arg2) ->
        f arg1 env (fun x -> 
        f arg2 env (fun y -> 
            begin match (x, y) with
            (Error(VNumber (n1)), Ok(VNumber (n2))) -> cont (Error(VNumber (n1)))
            | (Ok(VNumber (n1)), Error(VNumber (n2))) -> cont (Error(VNumber (n2)))
            | (Ok(VNumber (n1)), Ok(VNumber (n2))) -> if n1 < n2 then cont (Ok(VBool(true))) else cont(Ok(VBool(false)))
            | (_, _) -> cont (failwith ("Bad arguments to <: " ^
                                Value.to_string x ^ ", " ^
                                Value.to_string y ))
            end))
  | OpIf (arg1, arg2, arg3) -> 
      f arg1 env (fun x -> 
      begin match x with
        Ok(VBool (x)) ->
            if x then f arg2 env cont else f arg3 env cont
        | Error(_) -> cont (x)
        | Ok(_) -> failwith ("Predicate is not a boolean: " ^ Value.to_string x)
      end)
  | Let (x, arg2, arg3) ->
      f arg2 env (fun v2 ->
      let env1 = Env.extend env x (Value.ans_to_value v2) in f arg3 env1 cont)
  | Letrec (g, x, arg1, arg2) -> 
      let env1 = Env.extend env g (CloR (g, x, arg1, env)) in f arg2 env1 cont
  | Fun (x, arg1) -> cont (Ok(Clo(x, arg1, env)))
  | App (arg1, arg2) -> 
        f arg1 env (fun v1 -> 
        f arg2 env (fun v2 ->
        begin match v1 with
            Ok(Clo(x, t, env1)) -> 
                let env2 = Env.extend env1 x (Value.ans_to_value v2) in f t env2 cont
            | Ok(CloR (g, x, t, env1)) ->
                let env2 = Env.extend env1 g (CloR (g, x, t, env1)) in
                let env3 = Env.extend env2 x (Value.ans_to_value v2) in f t env3 cont
            | Error(_) -> cont(v1)
            | (_) -> cont(failwith ("Not a function: " ^ Value.to_string v1 ))
        end))
  | Cons (arg1, arg2) -> 
        f arg1 env (fun v1 ->
        f arg2 env (fun v2 ->
        begin match v2 with
            Ok(VList (lst)) -> cont(Ok(VList(Value.ans_to_value v1::lst)))
            | Error(_) -> cont(v2)
            | (_) -> failwith ("Not a list:" ^ Value.to_string v1)
        end))
  | Match (arg1, arg2, x, y, arg3) -> 
        f arg1 env (fun v1 ->
            begin match v1 with 
                Ok(VList([])) -> f arg2 env cont
                | Ok(VList(first::rest)) -> 
                    let env1 = Env.extend env x first in
                    let env2 = Env.extend env1 y (VList(rest)) in f arg3 env2 cont
                | Error(_) -> cont(v1)
                | (_) -> failwith ("Not a list: " ^ Value.to_string v1)
            end
        )
  | Raise (arg1) -> f arg1 env (fun v1 -> cont(Error(ans_to_value v1)))
  | Try (arg1, x, arg2) -> 
        f arg1 env (fun v1 ->
            begin match v1 with 
                Ok(_) -> cont(v1) 
                | Error(n) -> 
                    let env1 = Env.extend env x n in
                    f arg2 env1 (fun v2 -> cont(v2))
            end
        )
  | Op (_, _, _) -> failwith ("Parse.fail op")

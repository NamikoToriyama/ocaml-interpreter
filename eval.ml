open Syntax
open Value

(* 実際の計算をする関数 *)
(* Eval.f : Syntax.t -> Value.t *)
let rec f expr = match expr with
    Number (n) -> VNumber (n)
  | Bool (b) -> VBool (b)
  | Op (arg1, Plus, arg2) ->
      let v1 = f arg1 in
      let v2 = f arg2 in
      begin match (v1, v2) with
          (VNumber (n1), VNumber (n2)) -> VNumber (n1 + n2)
        | (_, _) -> failwith ("Bad arguments to +: " ^
                              Value.to_string v1 ^ ", " ^
                              Value.to_string v2)
      end
  | Op (arg1, Minus, arg2) ->
      let v1 = f arg1 in
      let v2 = f arg2 in
      begin match (v1, v2) with
          (VNumber (n1), VNumber (n2)) -> VNumber (n1 - n2)
        | (_, _) -> failwith ("Bad arguments to -: " ^
                              Value.to_string v1 ^ ", " ^
                              Value.to_string v2)
      end
  | Op (arg1, Times, arg2) ->
      let v1 = f arg1 in
      let v2 = f arg2 in
      begin match (v1, v2) with
          (VNumber (n1), VNumber (n2)) -> VNumber (n1 * n2)
        | (_, _) -> failwith ("Bad arguments to *: " ^
                              Value.to_string v1 ^ ", " ^
                              Value.to_string v2)
      end
  | Op (arg1, Equal, arg2) ->
      let v1 = f arg1 in
      let v2 = f arg2 in
      begin match (v1, v2) with
          (VNumber (n1), VNumber (n2)) -> if n1 == n2 then VBool(true) else VBool(false)
        | (VBool(b1), VBool(b2)) -> if b1 == b2 then VBool(true) else VBool(false)
        | (_, _) -> failwith ("Bad arguments to =: " ^
                              Value.to_string v1 ^ ", " ^
                              Value.to_string v2)
      end
  | Op (arg1, Less, arg2) ->
      let v1 = f arg1 in
      let v2 = f arg2 in
      begin match (v1, v2) with
          (VNumber (n1), VNumber (n2)) -> if n1 < n2 then VBool(true) else VBool(false)
        | (_, _) -> failwith ("Bad arguments to <: " ^
                              Value.to_string v1 ^ ", " ^
                              Value.to_string v2)
      end
  | Op (arg1, More, arg2) ->
      let v1 = f arg1 in
      let v2 = f arg2 in
      begin match (v1, v2) with
          (VNumber (n1), VNumber (n2)) -> if n1 < n2 then VBool(true) else VBool(false)
        | (_, _) -> failwith ("Bad arguments to <: " ^
                              Value.to_string v1 ^ ", " ^
                              Value.to_string v2)
      end
  | OpIf (If, arg1, Then, arg2, Else, arg3) -> 
      let b1 = f arg1 in
      begin match (b1) with
        VBool (b1) ->
            let v2 = f arg2 in 
            let v3 = f arg3 in
            begin match (v2, v3) with
                (VNumber (v2), VNumber(v3)) -> if b1 then VNumber (v2) else VNumber(v3)
                | (VNumber (v2), VBool(v3)) -> if b1 then VNumber (v2) else VBool(v3)
                | (VBool (v2), VNumber(v3)) -> if b1 then VBool (v2) else VNumber(v3)
                | (VBool (v2), VBool(v3)) -> if b1 then VBool (v2) else VBool(v3)
                end
        | (_) -> failwith ("Predicate is not a boolean: " ^ Value.to_string b1 )
      end
      

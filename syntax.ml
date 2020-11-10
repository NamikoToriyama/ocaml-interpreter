(* op_t : ２項演算子の型 *)
type op_t = Plus | Minus | Times | Equal | Less | More | If | Else 

(* ２項演算子を文字列にする関数 *)
(* op_to_string : op_t -> string *)
let op_to_string op = match op with
    Plus -> " + "
  | Minus -> " - "
  | Times -> " * "
  | Equal -> " = "
  | Less -> " < "
  | More -> " > "
  | If -> " if "
  | Else -> " else "

(* Syntax.t : プログラムを表す型 *)
type t = Number of int
       | Bool of bool
       | Var of string
       | Op of t * op_t * t
       | OpIf of  t * t * t
       | OpLet of t * t * t

(* プログラムを文字列にする関数 *)
(* Syntax.to_string : Syntax.t -> string *)
let rec to_string exp = match exp with
    Number (n) -> string_of_int n
  | Bool (b) -> if b then "true" else "false"
  | Var (s) -> s
  | Op (arg1, op, arg2) ->
      "(" ^ to_string arg1
          ^ op_to_string op
          ^ to_string arg2 ^ ")"
  | OpIf (arg1, arg2, arg3) -> 
      "(" ^ to_string arg1
          ^ to_string arg2
          ^ to_string arg3 ^ ")"
  | OpLet (arg1, arg2, arg3) ->
      "(" ^ to_string arg1 
          ^ to_string arg2 
          ^ to_string arg3 ^ ")"

(* プログラムをプリントする関数 *)
(* Syntax.print : Syntax.t -> unit *)
let print exp =
  let str = to_string exp in
  print_string str

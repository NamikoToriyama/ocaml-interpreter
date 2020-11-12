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
type t = Number of int                         (* 整数 *)
       | Bool of bool                          (* 真偽値 *)
       | Var of string                         (* 変数 *)
       | Op of t * op_t * t                    (* ２項演算 *)
       | OpIf of  t * t * t                    (* 条件文 *)
       | Let of string * t * t                    (* 変数定義 *)
       | Letrec of string * string * t * t     (* 再帰関数定義 *)
       | Fun of string * t                     (* fun 文 *)
       | App of t * t                          (* 関数呼び出し*)

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
  | Let (x, arg2, arg3) ->
      "(let" ^  x ^ " = " 
          ^ to_string arg2 
          ^ to_string arg3 ^ ")"
  | Letrec (f, x, arg1, arg2) -> 
      "(let rec" ^ f ^  " " 
          ^ x  ^ "="
          ^ to_string arg1
          ^ to_string arg2 ^ ")"
  | Fun (x, arg1) ->
      "(fun " ^ x ^ "->" ^ to_string arg1 ^ ")"
  | App (arg1, arg2) ->
      "(" ^ to_string arg1 ^ " " ^ to_string arg2 ^")"
  
  
(* プログラムをプリントする関数 *)
(* Syntax.print : Syntax.t -> unit *)
let print exp =
  let str = to_string exp in
  print_string str

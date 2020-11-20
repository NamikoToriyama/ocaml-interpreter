(* Value.t : プログラムの実行結果を表す型 *)
type t = VNumber of int
        | VBool of bool
        | Clo of string * Syntax.t * (string, t)Env.t
        | CloR of string * string * Syntax.t * (string, t)Env.t
        | VList of t list

let rec list_to_string lst str = match lst with
  [] -> str ^ "[]"
  | first :: rest -> 
    match first with
  | VNumber(n) -> list_to_string rest (str^string_of_int n ^ " :: ")
  | VBool (b) -> if b then list_to_string rest  str ^ " :: true" else list_to_string rest  str ^ " ::false"
  | (_) -> "value unexpected type"

(* プログラムの実行結果を文字列にする関数 *)
(* Value.to_string : Value.t -> string *)
let to_string value env = match value with
    VNumber (n) -> string_of_int n
  | VBool (b) -> if b then "true" else "false"
  | Clo(x, s, e) -> "<fun>"
  | CloR(g, x, s, e) -> "fun"
  | VList (lst) -> list_to_string lst "" 

(* プログラムの実行結果をプリントする関数 *)
(* Value.print : Value.t -> unit *)
let print exp env =
  let str = to_string exp env in
  print_string str

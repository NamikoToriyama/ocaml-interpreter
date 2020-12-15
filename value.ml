(* Value.t : プログラムの実行結果を表す型 *)
type t = VNumber of int
        | VBool of bool
        | Clo of string * Syntax.t * (string, t)Env.t
        | CloR of string * string * Syntax.t * (string, t)Env.t
        | VList of t list

(* answer type : 最終結果の型 *)
type 'a ans_t = Ok of 'a | Error of 'a

let rec list_to_string lst str = match lst with
  [] -> str ^ "[]"
  | first :: rest -> 
    match first with
  | VNumber(n) -> list_to_string rest (str^string_of_int n ^ " :: ")
  | VBool (b) -> if b then list_to_string rest (str ^ "true :: ") else list_to_string rest (str ^ "false :: ")
  | (_) -> "value unexpected type"

(* プログラムの実行結果を文字列にする関数 *)
(* Value.to_string : Value.t -> string *)
let to_string value env = match value with
    Ok(VNumber (n)) -> string_of_int n
  | Ok(VBool (b)) -> if b then "true" else "false"
  | Ok(Clo(x, s, e)) -> "<fun>"
  | Ok(CloR(g, x, s, e)) -> "fun"
  | Ok(VList (lst)) -> list_to_string lst ""
  | Error(_) ->  "Error "

(* Value.ans_to_value : Value.t Value.ans_t -> Value.t *)
let ans_to_value value = match value with
    Ok(VNumber (n)) -> VNumber (n)
    | Ok(VBool (b)) -> VBool (b)
    | Ok(Clo(x, s, e)) -> Clo(x, s, e)
    | Ok(CloR(g, x, s, e)) -> CloR(g, x, s, e)
    | Ok(VList (lst)) -> VList (lst)
    | Error(_) ->  failwith("error")

(* プログラムの実行結果をプリントする関数 *)
(* Value.print : Value.t -> unit *)
let print exp env =
  let str = to_string exp env in
  print_string str

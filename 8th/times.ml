(* 目的：リストの要素をすべて掛け合わせる *)
(* times : int list -> int *)
let rec times lst = match lst with
    [] -> 1
  | 0 :: rest -> 0
  | first :: rest -> first * times rest

(* テスト *)
let test1 = times [1; 2; 3; 4; 5] = 120
let test2 = times [1; 2; 0; 4; 5] = 0

(* CPS 版 *)

(* times2 : int list -> (int -> int) -> int *)
let rec times2 lst cont = match lst with
    [] -> cont 1
  | 0 :: rest -> 0
  | first :: rest -> times2 rest (fun x -> cont (first * x))

(* 初期継続 *)
(* id : 'a -> 'a *)
let id x = x

(* テスト *)
let test1 = times2 [1; 2; 3; 4; 5] id = 120
let test2 = times2 [1; 2; 0; 4; 5] id = 0

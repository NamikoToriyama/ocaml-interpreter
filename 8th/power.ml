(* 目的：リストのmのn乗を求める *)
(* power : int -> int -> int *)
let rec power m n =
  if n = 0 then 1 
  else m * power m (n - 1) 

(* テスト *)
let test1 = power 2 5 = 32
let test2 = power 9 2 = 81

(* CPS 版 *)

(* power2 : int -> int -> (int -> 'a) -> 'a *)
let rec power2 m n cont =
  if n = 0 then cont 1
  else power2 m (n - 1) (fun x -> cont (m * x))

(* 初期継続 *)
(* id : 'a -> 'a *)
let id x = x

(* テスト *)
let test1 = power2 2 5 id = 32
let test2 = power2 9 2 id = 81
let test3 = power2 2 0 id = 1
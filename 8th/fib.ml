(* 目的：フィボナッチ数列を求める *)
(* fib : int -> int *)
let rec fib n = 
  if n = 0 then 0
  else if n = 1 then 1
  else fib(n-1) + fib(n-2)

(* テスト *)
let test1 = fib 1 = 1
let test2 = fib 2 = 1
let test3 = fib 3 = 2
let test4 = fib 10 = 55

(* fib2 : int -> (int -> 'a) -> 'a  *)
let rec fib2 n cont = 
  if n = 0 then cont 0
  else if n = 1 then cont 1
  else fib2 (n-1) (fun x ->  fib2 (n-2) (fun y ->  cont (x + y)))

(* 初期継続 *)
(* id : 'a -> 'a *)
let id x = x

(* テスト *)
let test1 = fib2 1 id = 1
let test2 = fib2 2 id = 1
let test3 = fib2 3 id = 2
let test4 = fib2 10 id = 55

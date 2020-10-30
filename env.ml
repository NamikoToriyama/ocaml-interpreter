type ('a, 'b) t = Empty | Node of ('a, 'b) t * 'a * 'b * ('a, 'b) t

(* 空の環境 *)
let empty = Empty

(* 環境 env の中で変数 var の値を返す *)
let rec get env var = match env with
  Empty -> raise Not_found
  | Node (left, key, value, right) -> 
      if var = key then value
      else if var < key then get left var
      else get right var
  

(* 環境 env に変数 var の値を value に登録した新たな環境を返す *)
let rec extend env var value = match env with
  Empty -> Node (Empty, var, value, Empty)
  | Node (left, k, v, right) -> 
      if var = k
	        then Node (left, var, value, right)
      else if var < k
	        then Node (extend left var value, k, v, right)
	    else Node (left, k, v, extend right var value)

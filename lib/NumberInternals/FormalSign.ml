
type t = 
  | Plus
  | Minus
  | Null;;

let int_of_sign (s: t) : int =
  match s with 
  | Plus -> 1
  | Null -> 0
  | Minus -> (-1);;

let sign_of_int (i: int) : t =
  match i with 
  | 1 -> Plus
  | 0 -> Null
  | (-1) -> Minus
  | _ -> failwith "Int 'i' is such that |i| < 2...";;

let mul (s1: t) (s2: t) : t = 
  sign_of_int ((int_of_sign s1) * (int_of_sign s2));;
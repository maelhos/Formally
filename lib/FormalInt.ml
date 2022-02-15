
type t = int;;

let simplify (x: t) : t = x;;

let add (x: t) (y: t): t = 
  simplify (x+y);;

let sub (x: t) (y: t): t = 
  simplify (x-y);;

let mul (x: t) (y: t): t = 
  simplify (x*y);;

let sign (x: t) : FormalSign.t = 
  match x with 
  | _ when x > 0 -> FormalSign.Plus
  | _ when x = 0 -> FormalSign.Null
  | _ -> FormalSign.Minus;;

let cmp (x: t) (y: t) : FormalCompare.t =
  match sub x y with
  | 0 -> FormalCompare.Equal
  | a when sign a = FormalSign.Plus -> FormalCompare.Above
  | _ -> FormalCompare.Bellow;;


(* //// *)

let rec gcd (a: t) (b: t) : t = 
  match b with
  | 0 -> a
  | _ -> gcd b (a mod b);;
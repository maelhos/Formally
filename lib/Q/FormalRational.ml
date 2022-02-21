open FormalNumberInternals
open FormalZ

type t = {a: int; b: int};;
let num (x: t) : int = x.a;;
let den (x: t) : int = x.b;;

let rec simplify (x: t) : t = 
  let g = FormalInt.gcd x.a x.b in
  match x with
  | _ when x.a < 0 && x.b < 0 -> simplify {a = -x.a; b = -x.b}
  | _ when  g > 1 -> simplify {a = x.a / g; b = x.b / g}
  | _ -> x;;

let add (x: t) (y: t): t = 
  simplify {a = x.a*y.b + x.b*y.a; b = x.b*y.b};;

let sub (x: t) (y: t): t = 
  simplify {a = x.a*y.b - x.b*y.a; b = x.b*y.b};;

let mul (x: t) (y: t): t = 
  simplify {a = x.a * y.a; b = x.b * y.b};;

let inv (x: t) : t = 
  simplify {a = x.b ; b = x.a};;

let div (x: t) (y: t): t = 
  simplify (mul x (inv y));;

let sign (x: t) : FormalSign.t = 
  FormalSign.mul (FormalInt.sign x.a) (FormalInt.sign x.b);;
  
let cmp (x: t) (y: t) : FormalCompare.t =
  match sub x y with
  | {a = 0; b = _} -> FormalCompare.Equal
  | a when sign a = FormalSign.Plus -> FormalCompare.Above
  | _ -> FormalCompare.Bellow;;
  
let frac_of_int (i: int) : t = {a = i; b = 1};;
let float_of_Frat (x: t) : float = (float_of_int x.a) /. (float_of_int x.b);;
let int_of_Frat (x: t) : int = int_of_float (float_of_Frat x);;
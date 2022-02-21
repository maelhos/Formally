open FormalNumberInternals

type t = int;;

let add (x: t) (y: t) : t = x+y;;
 
let sub (x: t) (y: t) : t = x-y;;
 
let mul (x: t) (y: t) : t = x*y;;

exception NoNegativePowerForIntegers;;
let pow (x: t) (y: t) : t = 
  let rec aux (x: t) (y: t) : t =
    if y = 0 then 1 else (if y land 1 = 0 then aux (x*x) (y/2) else x * aux (x*x) ((y-1)/2))
  in
  if y < 0 then raise NoNegativePowerForIntegers else aux x y ;;

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
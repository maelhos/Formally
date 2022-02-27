open FormalNumber.FormalExpr
open FormalOperators

let formalType (f: formalExpr) : string = 
  match f with
  | Fint(_) -> "Fint"
  | Frat(_) -> "Frat";;

let print_formal (x: formalExpr) : unit =
  match x with
  | Fint(a) -> print_int a
  | Frat({a=x;b=y}) -> print_int x; print_char '/'; print_int y;;

let ( +$ ) (x: formalExpr) (y: formalExpr) : formalExpr = Add.add x y;;

let ( -$ ) (x: formalExpr) (y: formalExpr) : formalExpr = Sub.sub x y;;

let ( *$ ) (x: formalExpr) (y: formalExpr) : formalExpr = Mul.mul x y;;

let ( /$ ) (x: formalExpr) (y: formalExpr) : formalExpr = Div.div x y;;

let ( **$ ) (x: formalExpr) (y: formalExpr) : formalExpr = Pow.pow x y;;
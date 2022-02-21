open FormalNumber

let formalType (f: formalNumber) : string = 
  match f with
  | Fint(_) -> "Fint"
  | Frat(_) -> "Frat";;

let print_formal (x: formalNumber) : unit =
  match x with
  | Fint(a) -> print_int a
  | Frat({a=x;b=y}) -> print_int x; print_char '/'; print_int y;;

let ( +$ ) (x: formalNumber) (y: formalNumber) : formalNumber = Add.add x y;;

let ( -$ ) (x: formalNumber) (y: formalNumber) : formalNumber = Sub.sub x y;;

let ( *$ ) (x: formalNumber) (y: formalNumber) : formalNumber = Mul.mul x y;;

let ( /$ ) (x: formalNumber) (y: formalNumber) : formalNumber = Div.div x y;;

let ( **$ ) (x: formalNumber) (y: formalNumber) : formalNumber = Pow.pow x y;;

type formalNumber = 
  | Fint of FormalInt.t
  | Frat of FormalRational.t;;

let formalType (f: formalNumber) : string = 
  match f with
  | Fint(_) -> "Fint"
  | Frat(_) -> "Frat";;

let rec simplify (x: formalNumber) : formalNumber =
  match x with
  | Fint(a) -> Fint(a)
  | Frat(a) when FormalRational.den a = 1 -> simplify (Fint(FormalRational.num a))
  | Frat(a) -> Frat(a);;

let print_formal (x: formalNumber) : unit =
  match x with
  | Fint(a) -> print_int a
  | Frat({a=x;b=y}) -> print_int x; print_char '/'; print_int y;;

let add (x: formalNumber) (y: formalNumber) : formalNumber = 
  match x, y with
  | Fint(a), Fint(b) -> Fint(FormalInt.add a b)
  | Fint(a), Frat(b) -> simplify (Frat(FormalRational.add (FormalRational.frac_of_int a) b))
  | Frat(a), Fint(b) -> simplify (Frat(FormalRational.add (FormalRational.frac_of_int b) a))
  | Frat(a), Frat(b) -> simplify (Frat(FormalRational.add a b));;
let ( +$ ) (x: formalNumber) (y: formalNumber) : formalNumber = add x y;;

let sub (x: formalNumber) (y: formalNumber) : formalNumber = 
  match x, y with
  | Fint(a), Fint(b) -> Fint(FormalInt.sub a b)
  | Fint(a), Frat(b) -> simplify (Frat(FormalRational.sub (FormalRational.frac_of_int a) b))
  | Frat(a), Fint(b) -> simplify (Frat(FormalRational.sub (FormalRational.frac_of_int b) a))
  | Frat(a), Frat(b) -> simplify (Frat(FormalRational.sub a b));;
let ( -$ ) (x: formalNumber) (y: formalNumber) : formalNumber = sub x y;;

let mul (x: formalNumber) (y: formalNumber) : formalNumber = 
  match x, y with
  | Fint(a), Fint(b) -> Fint(FormalInt.mul a b)
  | Fint(a), Frat(b) -> simplify (Frat(FormalRational.mul (FormalRational.frac_of_int a) b))
  | Frat(a), Fint(b) -> simplify (Frat(FormalRational.mul (FormalRational.frac_of_int b) a))
  | Frat(a), Frat(b) -> simplify (Frat(FormalRational.mul a b));;
let ( *$ ) (x: formalNumber) (y: formalNumber) : formalNumber = mul x y;;

let div (x: formalNumber) (y: formalNumber) : formalNumber = 
  match x, y with
  | Fint(a), Fint(b) -> simplify (Frat(FormalRational.div (FormalRational.frac_of_int a) (FormalRational.frac_of_int b)))
  | Fint(a), Frat(b) -> simplify (Frat(FormalRational.div (FormalRational.frac_of_int a) b))
  | Frat(a), Fint(b) -> simplify (Frat(FormalRational.div (FormalRational.frac_of_int b) a))
  | Frat(a), Frat(b) -> simplify (Frat(FormalRational.div a b));;
let ( /$ ) (x: formalNumber) (y: formalNumber) : formalNumber = div x y;;
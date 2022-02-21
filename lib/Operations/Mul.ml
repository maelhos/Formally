open FormalNumber.FormalNum
open FormalNumber.Simplify
open FormalZ
open FormalQ

let mul (x: formalNumber) (y: formalNumber) : formalNumber = 
  match x, y with
  | Fint(a), Fint(b) -> Fint(FormalInt.mul a b)
  | Fint(a), Frat(b) -> simplify (Frat(FormalRational.mul (FormalRational.frac_of_int a) b))
  | Frat(a), Fint(b) -> simplify (Frat(FormalRational.mul (FormalRational.frac_of_int b) a))
  | Frat(a), Frat(b) -> simplify (Frat(FormalRational.mul a b));;
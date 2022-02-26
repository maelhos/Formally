open FormalNumber.FormalNum
open FormalNumber.Simplify
open FormalZ
open FormalQ

let pow (x: formalNumber) (y: formalNumber) : formalNumber = (* work in progress ... *)
  match x, y with
  | Fint(a), Fint(b) -> Fint(FormalInt.pow a b)
  | Fint(a), Frat(b) -> simplify (Frat(FormalRational.mul (FormalRational.frac_of_int a) b))
  | Frat(a), Fint(b) -> simplify (Frat(FormalRational.powInt a b))
  | Frat(a), Frat(b) -> simplify (Frat(FormalRational.mul a b));;
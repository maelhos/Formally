open FormalNumber
open Simplify

let add (x: formalNumber) (y: formalNumber) : formalNumber = 
  match x, y with
  | Fint(a), Fint(b) -> Fint(FormalInt.add a b)
  | Fint(a), Frat(b) -> simplify (Frat(FormalRational.add (FormalRational.frac_of_int a) b))
  | Frat(a), Fint(b) -> simplify (Frat(FormalRational.add (FormalRational.frac_of_int b) a))
  | Frat(a), Frat(b) -> simplify (Frat(FormalRational.add a b));;
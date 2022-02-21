open FormalNumber
open Simplify

let div (x: formalNumber) (y: formalNumber) : formalNumber = 
  match x, y with
  | Fint(a), Fint(b) -> simplify (Frat(FormalRational.div (FormalRational.frac_of_int a) (FormalRational.frac_of_int b)))
  | Fint(a), Frat(b) -> simplify (Frat(FormalRational.div (FormalRational.frac_of_int a) b))
  | Frat(a), Fint(b) -> simplify (Frat(FormalRational.div (FormalRational.frac_of_int b) a))
  | Frat(a), Frat(b) -> simplify (Frat(FormalRational.div a b));;
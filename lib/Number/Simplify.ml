open FormalNumber

let rec simplify (x: formalNumber) : formalNumber =
  match x with
  | Fint(a) -> Fint(a)
  | Frat(a) when FormalRational.den a = 1 -> simplify (Fint(FormalRational.num a))
  | Frat(a) -> Frat(a);;

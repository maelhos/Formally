
type formalNumber = 
  | Fint of FormalInt.t
  | Frat of FormalRational.t;;

let ( *$ ) (l: int list) (l2: int list) : int list = l @ l2;;

let add (x: formalNumber) (y: formalNumber) : formalNumber = 
  match x, y with
  | Fint(a), Fint(b) -> Fint(FormalInt.add a b)
  | Fint(a), Frat(b) -> Frat(FormalRational.add (FormalRational.frac_of_int a) b)
  | Frat(a), Fint(b) -> Frat(FormalRational.add (FormalRational.frac_of_int b) a)
  | 

open FormalNum

type formalExpr = 
  | Opp of (formalExpr*formalExpr -> formalExpr)*formalExpr*formalExpr
  | Const of formalNumber
  | Sym of char;;

type exprOrFnum = 
  | Fexpr of formalExpr
  | Fnum of formalNumber;;

let rec isSimplifableConst (f: formalExpr) : bool = 
  match f with 
  | Const(_) -> true
  | Sym(_) -> false
  | Opp(_,a,b) -> (isSimplifableConst a) && (isSimplifableConst b);;

let rec simplifyToConst (f: formalExpr) : formalNumber = 
  match f with 
  | Sym(a) -> failwith ("Expression contains unknown symbol : " ^ (String.make 1 a))
  | Const(a) -> a
  | Opp(f, a, b) -> f (a, b);;
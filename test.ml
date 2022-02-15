open Formally


let rec print_int_list (l: int list) : unit= 
  match l with
  | [] -> ()
  | h1::h2::t -> Printf.printf "%d ;" h1; print_int_list (h2::t)
  | h::[] -> Printf.printf "%d\n" h;;

let a = Fint(497)*$(Fint(8) /$ Fint(14) +$ Fint(3) *$ Fint(5) /$ Fint(71));;

print_formal a;;
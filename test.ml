open Formally


let rec print_int_list (l: int list) : unit= 
  match l with
  | [] -> ()
  | h1::h2::t -> Printf.printf "%d ;" h1; print_int_list (h2::t)
  | h::[] -> Printf.printf "%d\n" h;;

let a = Fint(492) **$ Fint(7);;

print_formal a;;
print_newline ();;
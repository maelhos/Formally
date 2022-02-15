open Formally

let a = [5;2];;

let b = [8; 5; 2];;

let c = a *$ b;;

let rec print_int_list (l: int list) : unit= 
  match l with
  | [] -> ()
  | h1::h2::t -> Printf.printf "%d ;" h1; print_int_list (h2::t)
  | h::[] -> Printf.printf "%d\n" h;;

let () = print_int_list c;;

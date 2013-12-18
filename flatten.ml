(* Concatenates the elements of a list of lists. *)

let rec flatten' lst acc = match lst with
  | [] -> acc
  | hd::tl -> flatten' tl (acc @ hd);;

let flatten lst = flatten' lst [];;

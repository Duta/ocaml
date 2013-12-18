(* Count the number of strictly negative
 * (less than 0) elements in a list of integers. *)

let rec nneg = function
  | [] -> 0
  | h::t -> (if h < 0 then 1 else 0) + nneg t;;

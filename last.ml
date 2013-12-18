(* Returns the last element in a non-empty list. *)

let rec last = function
  | [x] -> x
  | x::xs -> last xs;;

(* Checks whether the first and the last
 * elements of a list exist and are equal. *)

let rec last = function
  | [x] -> x
  | x::xs -> last xs;;

let firstlasteq lst = match lst with
  | [] -> false
  | h::_ -> h = last lst;;

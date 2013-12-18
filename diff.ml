(* Produces the difference of two lists: in the resulting list each
 * element occurs the number of times it occurs in the first
 * argument minus the number of times it occurs in the second. *)

let rec mem x = function
  | [] -> false
  | h::t -> h = x || mem x t;;

let rec remove x = function
  | [] -> failwith "x not in list"
  | h::t -> if h = x then t else h::(remove x t);;

let rec diff a b = match a with
  | [] -> []
  | h::t -> if mem h b then diff t (remove h b) else h::(diff t b);;

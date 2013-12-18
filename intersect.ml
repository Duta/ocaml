(* Produces the intersection of two lists: in the resulting list each element
 * occurs the minimum number of times it occurs in the two arguments. *)

let rec mem x = function
  | [] -> false
  | h::t -> h = x || mem x t;;

let rec remove x = function
  | [] -> failwith "x not in list"
  | h::t -> if h = x then t else h::(remove x t);;

let rec intersect a b = match a with
  | [] -> if b = [] then [] else intersect b a
  | h::t ->
      if mem h b then
        let b' = remove h b in
        h::(intersect t b')
      else
        intersect t b;;

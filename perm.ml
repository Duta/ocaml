(* Checks whether two lists are permutations of each other. *)

let rec ins item lst = match lst with
  (* If the empty list, we can insert it *)
  | [] -> [item]
  (* If item is less than the head,
   * return it inserted at the front.
   * Else, recursively check the tail
   * (with head prepended to that result) *)
  | x::xs -> if item < x then item::lst else x::ins item xs;;

let rec sort = function
  (* The empty list is already sorted *)
  | [] -> []
  (* Sort the tail, and insert head
   * at the correct point in that list *)
  | x::xs -> ins x (sort xs);;

let perm a b = sort a = sort b;;

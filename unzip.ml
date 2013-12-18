(* Unzips a list of pairs, producing a pair of lists. *)

let rec unzip' lst (acc1, acc2) = match lst with
  | [] -> (acc1, acc2)
  | (a, b)::t -> unzip' t (acc1 @ [a], acc2 @ [b]);;

let unzip (lst : (('a * 'b) list)) = unzip' lst ([], []);;

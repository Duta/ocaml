(* Takes a list of pairs and produces a
 * list of the first elements of the pairs. *)

let rec proj1' lst acc = match lst with
  | [] -> acc
  | (a, b)::t -> proj1' t (acc @ [a]);;

let proj1 lst = proj1' lst [];;

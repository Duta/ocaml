(* Returns true iff the first argument
 * is a sub-list of the second argument. *)

let rec mem x = function
  | [] -> false
  | h::t -> h = x || mem x t;;

let rec remove x = function
  | [] -> failwith "x not in list"
  | h::t -> if h = x then t else h::(remove x t);;

let rec prelist a b = match a, b with
  | [], _ -> true
  | _, [] -> false
  | h::t, h'::t' -> h = h' && prelist t t';;

let rec sublist a b = match a, b with
  | [], _ -> true
  | _, [] -> false
  | h::_, h'::t' -> (h = h' && prelist a b) || sublist a t';;

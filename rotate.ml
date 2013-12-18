(* Rotates a list of N lists of size M into M lists of size N. *)

let rec heads = function
  | [] -> []
  | []::_ -> failwith "Invalid list"
  | (x::_)::t -> x::heads t;;

let rec tails = function
  | [] -> []
  | []::_ -> failwith "Invalid list"
  | (_::x)::t -> x::tails t;;

let rec rotate = function
  | [] -> []
  | []::t -> rotate t
  | l -> (heads l)::rotate (tails l);;

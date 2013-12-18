(* Multiplication function ( * ) for the Z|S natural number representation. *)

type nat = Zero | Suc of nat;;

let rec add acc a b = match a with
  | Zero -> acc
  | Suc a -> add (Suc acc) a b;;

let ( + ) a b = add b a b;;

let rec mul acc a b = match a with
  | Zero -> acc
  | Suc a -> mul (b + acc) a b;;

let ( * ) = mul Zero;;

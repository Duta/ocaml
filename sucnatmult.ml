(* Multiplication function ( * ) for the Z|S natural number representation. *)

type nat = Zero | Suc of nat;;

let rec ( + ) a b = match a with
  | Zero -> b
  | Suc a -> Suc (a + b);;

let rec ( * ) a b = match a with
  | Zero -> Zero
  | Suc a -> b + (a * b);;

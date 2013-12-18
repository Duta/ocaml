(* Division for the Z|S natural number representation which
 * returns the result of integer division and the remainder *)

type nat = Zero | Suc of nat;;

let rec div a b =
  if a < b || b = Zero then
    (Zero, a)
  else
    let amt, rem = div (a - b) b in
    (Suc amt, rem);;

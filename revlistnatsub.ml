(* Subtraction for natural numbers represented
 * as a (reversed) list of digits in an arbitrary base. *)

type nat = int list;;

let rec sub' carry m n base = match m, n with
  | [], []     -> []
  | [], _      -> [0]
  | _, []      -> sub' carry m [0] base
  | x::m, y::n -> let x = if carry then x - 1 else x in
                  let temp = x - y in
                  let carry = temp < 0 in
                  let res = if carry then temp + base else temp in
                  res::(sub' carry m n base);;

let sub = sub' false;;

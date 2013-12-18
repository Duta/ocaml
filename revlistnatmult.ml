(* Multiplication for natural numbers represented
 * as a (reversed) list of digits in an arbitrary base. *)

type nat = int list;;

let add m n base =
  let rec aux carry m n = match m, n with
    | [], []     -> if carry then [1] else []
    | [], n
    | n, []      -> aux carry n [0]
    | x::m, y::n -> let temp = x + y + if carry then 1 else 0 in
                    let carry' = temp >= base in
                    let res = if carry' then temp - base else temp in
                    res::(aux carry' m n)
  in
  aux false m n;;

let spread [x] base =
  if x >= base then
    [x mod base] @ [x / base]
  else
    [x];;

let rec f x xs base = match xs with
  | [] -> []
  | h::t -> add (spread [x*h] base) (0::(f x t base)) base;;

let rec mul m n base = match m with
  | []   -> []
  | h::t -> add (f h n base) (0::(mul t n base)) base;;

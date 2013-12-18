(* Returns the most frequent element in a non-empty list. *)

let rec extract lst max = match lst with
  | [] -> fst max
  | h::t -> extract t (if snd h > snd max then h else max);;

let rec update lst el acc = match lst with
  | [] -> acc @ [(el, 1)]
  | h::t -> if el = fst h
            then acc @ [(el, (snd h) + 1)] @ t
            else update t el (acc @ [h]);;

let rec freq' lst acc = match lst with
  | [] -> extract acc (0, 0)
  | h::t -> freq' t (update acc h []);;

let freq lst = freq' lst [];;

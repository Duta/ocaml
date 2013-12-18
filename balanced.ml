(* A btree is balanced if the difference between
 * the height of the right subtree and the height
 * of the left subtree (in any subtree) is at most 1.
 * Returns true iff the btree is balanced. *)

type 'a btree = Leaf | Node of ('a * 'a btree * 'a btree);;

let rec height = function
  | Leaf -> 1
  | Node (n, l, r) -> 1 + (max (height l) (height r));;

let rec height = function
  | Leaf -> 1
  | Node (n, l, r) -> 1 + (max (height l) (height r));;

let rec balanced = function
  | Leaf -> true
  | Node (n, l, r) ->
      let diff = (height l) - (height r) in
      -1 <= diff && diff <= 1 && balanced l && balanced r;;

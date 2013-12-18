(* Takes 6 arguments and returns the smallest one. *)

let min6 x1 x2 x3 x4 x5 x6 =
  min (min (min x1 x2) (min x3 x4)) (min x5 x6);;
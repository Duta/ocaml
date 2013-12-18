(* Consider a map given as a list of pairs of cities indicating
 * that we can drive directly between the two cities.
 * Given two cities produces the shortest route between the two cities.
 * If there is no such route, returns the empty list. *)

let shortestroute edges start goal =
  (* Returns true iff the element is not a member of the list *)
  let rec notmem m = function
    | [] -> true
    | h::t -> m <> h && notmem m t
  in
  (* Returns a list of new nodes to explore from v *)
  let nextnodes v path =
    let rec aux acc edges = match edges with
      | [] -> acc
      | (a, b)::t -> let acc' = if a = v && notmem b path then b::acc
                                else if b = v && notmem a path then a::acc
                                else acc in
                     aux acc' t
    in
    aux [] edges
  in
  (* Returns the length of the list *)
  let rec len = function
    | [] -> 0
    | _::t -> 1 + len t
  in
  (* Returns the shortest list in a list of lists *)
  let rec shortest = function
    | [] -> []
    | [x] -> x
    | h::t -> let min = shortest t in
              if len h < len min then h else min
  in
  (* Returns all routes from start to goal *)
  let rec allroutes path = match path with
    | [] -> []
    | v::_ -> let nextnodes = nextnodes v path in
              let rec explore = function
                | [] -> []
                | h::t -> allroutes (h::path) @ (explore t)
              in
              if v = start then [path]
              else explore nextnodes
  in
  shortest (allroutes [goal]);;

(* Consider a map given as a list of pairs of cities indicating
 * that we can drive directly between the two cities.
 * Given two cities produces a route between the two cities.
 * If there is no such route, returns the empty list. *)

let findroute edges start goal =
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
  let rec aux path = match path with
    | [] -> []
    | v::_ -> let nextnodes = nextnodes v path in
              let rec explore = function
                | [] -> []
                | h::t -> let route = aux (h::path) in
                          if route <> [] then route
                          else explore t
              in
              if v = start then path
              else explore nextnodes
  in
  aux [goal];;

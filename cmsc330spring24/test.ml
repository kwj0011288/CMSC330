type node = int
type edge = node * node
type graph = (node * edge list)

(* Function to find the edges starting from a specific node *)
let rec find_edges start graph = 
  match graph with
  | [] -> []
  | (node, edges)::rest ->
    if node = start then edges
    else find_edges start rest


let rec reachable start graph visited =
  let rec is_visited node visited =
    match visited with
    | [] -> false
    | h::t -> if h = node then true else is_visited node t
  in
  if is_visited start visited then visited
  else
    let edges = find_edges start graph in
    let visited = start::visited in
    List.fold_left (fun visited edge -> reachable edge graph visited) visited edges






(* Define the graph as a list of tuples representing edges *)
type vertex = int
type edge = vertex * vertex
type graph = edge list

(* Helper function to find neighbors of a given vertex *)
let rec find_neighbors vertex graph =
  match graph with
  | [] -> []
  | (v1, v2)::rest ->
      if v1 = vertex then v2::(find_neighbors vertex rest)
      else if v2 = vertex then v1::(find_neighbors vertex rest)
      else find_neighbors vertex rest

(* Function to compute all reachable nodes from a start node *)
let rec reachable_nodes start graph visited =
  if List.mem start visited then visited
  else
    let neighbors = find_neighbors start graph in
    let visited' = start :: visited in
    List.fold_left (fun acc node -> reachable_nodes node graph acc) visited' neighbors

(* Example usage *)
let graph = [(1, 2); (1, 3); (2, 4); (3, 5); (4, 6); (5, 6)]
let start_node = 1
let reachable = reachable_nodes start_node graph []
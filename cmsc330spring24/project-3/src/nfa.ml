open List
open Sets

(*********)
(* Types *)
(*********)

type ('q, 's) transition = 'q * 's option * 'q

type ('q, 's) nfa_t = {
  sigma: 's list;
  qs: 'q list;
  q0: 'q;
  fs: 'q list;
  delta: ('q, 's) transition list;
}

(***********)
(* Utility *)
(***********)

(* explode converts a string to a character list *)
let explode (s: string) : char list =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l)
  in
  exp (String.length s - 1) []

(****************)
(* Part 1: NFAs *)
(****************)

(*
let nfa_ex = {
    sigma = ['a'];
    qs = [0; 1; 2];
    q0 = 0;
    fs = [2];
    delta = [(0, Some 'a', 1); (1, None, 2)]
}   

let dfa_ex = {
    sigma = ['a'; 'b'; 'c'];
    qs = [0; 1; 2];
    q0 = 0;
    fs = [2];
    delta = [(0, Some 'a', 1); (1, Some 'b', 0); (1, Some 'c', 2)]
}
*)

let move (nfa : ('q, 's) nfa_t) (qs : 'q list) (s : 's option) : 'q list =
  List.fold_left
    (fun acc (start, sym, ending) ->
      if (elem start qs) && (sym = s) 
        then insert ending acc else acc) [] nfa.delta


let e_closure nfa qs =
  let rec helper states visited =
    match states with
    | [] -> visited
    | h::t ->
        if elem h visited then helper t visited
        else
          let next = 
            fold_left (fun acc (start, value, dest) ->
              if start = h && value = None && not (elem dest visited)
              then insert dest acc
              else acc) [] nfa.delta
          in
          helper (union t next) (insert h visited)
  in
  let init = [] in
  let result = helper qs init in
  List.sort_uniq Stdlib.compare result

let accept (nfa: ('q, char) nfa_t) (input: string) : bool =
  let rec helper qs input =
    match input with
    | [] -> List.exists (fun elem -> List.mem elem nfa.fs) qs
    | h :: t -> helper (e_closure nfa (move nfa qs (Some h))) t
  in
  helper (e_closure nfa [nfa.q0]) (explode input)

(*******************************)
(* Part 2: Subset Construction *)
(*******************************)

let new_states (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list list = 
  List.fold_left 
    (fun acc sigma -> e_closure nfa (move nfa qs (Some sigma)) :: acc) [] nfa.sigma

let new_trans (nfa: ('q,'s) nfa_t) (qs: 'q list) : ('q list, 's) transition list =
  List.fold_left
    (fun acc sigma -> (qs, Some sigma, e_closure nfa (move nfa qs (Some sigma))) :: acc) [] nfa.sigma

let new_finals (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list list = 
  List.fold_left
    (fun acc q -> 
      if List.exists (fun elem -> elem = q) nfa.fs 
        then qs :: acc else acc) [] qs

let rec nfa_to_dfa_step (nfa: ('q,'s) nfa_t) (dfa: ('q list, 's) nfa_t)
    (work: 'q list list) : ('q list, 's) nfa_t =
  match work with
  | [] -> dfa 
  | h::t ->  
      let update = 
        {
        sigma = dfa.sigma;
        qs = dfa.qs @ new_states nfa h;
        q0 = dfa.q0;
        fs = dfa.fs @ new_finals nfa h;
        delta = insert_all (new_trans nfa h) dfa.delta;
      } 
    in
      let new_work = t @ (List.filter (fun state -> not (List.mem state dfa.qs)) (new_states nfa h)) in
      nfa_to_dfa_step nfa update new_work


let nfa_to_dfa (nfa: ('q,'s) nfa_t) : ('q list, 's) nfa_t =
  let state = e_closure nfa [nfa.q0] in
  let init = 
    {
    sigma = nfa.sigma;
    qs = [state];
    q0 = state;
    fs = (if List.exists (fun q -> List.mem q nfa.fs) state then [state] else []);
    delta = [];
  }
in
  let dfa =  nfa_to_dfa_step nfa init [state] in
  let result = 
    {
    sigma = List.sort_uniq Stdlib.compare dfa.sigma;
    qs = List.sort_uniq Stdlib.compare dfa.qs;
    q0 = List.sort_uniq Stdlib.compare dfa.q0;
    fs = List.sort_uniq Stdlib.compare dfa.fs;
    delta = List.map (fun (q, s, r) -> (List.sort_uniq Stdlib.compare q, s, List.sort_uniq Stdlib.compare r)) dfa.delta;
  }
in
  result


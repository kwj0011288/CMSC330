open Funs 
open Stdlib

(***********************************)
(* Part 1: Non-Recursive Functions *)
(***********************************)


let abs x = if x < 0 then -x else x

let rev_tup tup = match tup with
  | (a, b, c) -> (c, b, a)

let is_even x = if x mod 2 == 0 then true else false

let area point1 point2 = match (point1 ,point2) with
  | (a,b) , (c,d) -> abs(c - a) * abs(d - b)

(*******************************)
(* Part 2: Recursive Functions *)
(*******************************)

let rec fibonacci n = if n <= 1 then n else fibonacci(n - 1) + fibonacci(n - 2)

let rec pow x p = if p = 0 then 1 else x * pow x (p - 1)

let log x y = 
  let rec helper x y z =
    if pow x z > y then z - 1 else helper x y (z + 1)
  in helper x y 1

let gcf x y = 
  let rec helper x y i = match (x, y) with
    | (0, _) -> y
    | (_, 0) -> x
    | (_, _) -> if  y mod (y - i) = 0 && x mod (y - i) = 0 then (y-i) else helper x y (i + 1)
    in helper x y 0

(*****************)
(* Part 3: Lists *)
(*****************)

let reverse lst = 
  let rec helper lst answer = match lst with
    | [] -> answer
    | h::t -> helper t (h :: answer)
  in helper lst []

let rec zip lst1 lst2 = match (lst1, lst2) with
    |(_,[]) -> []
    |([],_)-> []
    |((a1, b1) :: t1), ((a2, b2) :: t2) -> (a1, b1, a2, b2) :: zip t1 t2

let rec merge lst1 lst2 = match (lst1, lst2) with
    | [], lst2 -> lst2  
    | lst1, [] -> lst1  
    | (h1 :: t1), (h2 :: t2) ->
    if h1 <= h2 then h1 :: (merge t1 (h2 :: t2)) else h2 :: (merge (h1 :: t1) t2)

let rec is_present lst v = match lst with
  | [] -> false
  | h::t -> if h = v then true else is_present t v

let every_nth n lst =
  let rec helper lst i = match lst with
    | [] -> []
    | h::t -> if i mod n = 0 then h :: (helper t (i + 1)) else helper t (i + 1)
  in helper lst 1
    
let jumping_tuples lst1 lst2 =
  let rec helper lst1 lst2 first second i = match lst1, lst2 with 
    | [], _ -> first @ second 
    | _, [] -> first @ second 
    | (a1,a2)::t1, (b1,b2)::t2 ->
        if i mod 2 = 0 then
          helper t1 t2 (first @ [b2]) (second @ [a1]) (i+1)
        else
          helper t1 t2 (first @ [a1]) (second @ [b2]) (i+1)
  in helper lst1 lst2 [] [] 0

let max_func_chain init funcs = 
  let rec helper value funcs = match funcs with
    | [] -> value
    | h_func::t_func -> 
        if (helper value t_func) > (helper (h_func value) t_func) then
          helper value t_func
        else
          helper (h_func value) t_func
  in helper init funcs

(*****************)
(* Part 4: HOF *)
(*****************)

let is_there lst x =
  fold (fun result elem -> if elem = x then true else result) false lst

let count_occ lst target = 
  fold (fun count elem -> if elem = target then (count + 1) else count) 0 lst

let uniq lst =
  fold (fun result elem ->
      if is_there result elem then 
        result else elem::result) [] lst

let every_xth x lst = 
  let (index, answer) = fold (fun (i, result) elem -> 
      if i mod x = 0 then 
        (i + 1, result @ [elem]) else (i + 1, result)) (1,[]) lst
  in answer
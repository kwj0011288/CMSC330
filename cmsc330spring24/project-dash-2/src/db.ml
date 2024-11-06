type person = { name : string; age : int; hobbies : string list }

type comparator = person -> person -> int

type condition =
| True
| False
| Age of (int -> bool)
| Name of (string -> bool)
| Hobbies of (string list -> bool)
| And of condition * condition
| Or of condition * condition
| Not of condition
| If of condition * condition * condition

(* TODO: Implement functions below *)

type db = person list

let newDatabase = []

let insert person db = person :: db

let remove name db =
List.fold_right (fun find acc -> if find.name = name then acc else find :: acc) db []

let sort comparator db = List.sort comparator db

let query condition db =
let rec helper condition person =
  match condition with
  | True -> true
  | False -> false
  | Age f -> if f person.age then true else false
  | Name f -> if f person.name then true else false
  | Hobbies f -> if f person.hobbies then true else false
  | And (f1, f2) -> helper f1 person && helper f2 person
  | Or (f1, f2) -> helper f1 person || helper f2 person
  | Not f -> not (helper f person)
  | If (f1, f2, f3) -> if helper f1 person then helper f2 person else helper f3 person
in
List.fold_right (fun person acc -> if helper condition person then person :: acc else acc) db []

let queryBy condition db comparator = sort comparator (query condition db)

let update condition db change =
let rec helper condition person =
  match condition with
  | True -> true
  | False -> false
  | Age f -> f person.age
  | Name f -> f person.name
  | Hobbies f -> f person.hobbies
  | And (f1, f2) -> helper f1 person && helper f2 person
  | Or (f1, f2) -> helper f1 person || helper f2 person
  | Not f -> not (helper f person)
  | If (f1, f2, f3) -> if helper f1 person then helper f2 person else helper f3 person
in
List.map (fun person -> if helper condition person then change person else person) db

let deleteAll condition db = 
let rec helper condition person = match condition with
  | True -> true
  | False -> false
  | Age (f)-> f person.age
  | Name (f) -> f person.name
  | Hobbies(f) -> f person.hobbies
  | And (f1, f2) -> helper f1 person && helper f2 person
  | Or (f1, f2) -> helper f1 person || helper f2 person
  | Not (f) -> not (helper f person)
  | If (f1, f2, f3) -> if helper f1 person then helper f2 person else helper f3 person
in List.fold_right (fun person acc -> if helper condition person then acc else person :: acc) db []

open Ast
open Utils

let extend env x v = (x, v) :: env

let rec lookup env x =
  match env with
  | [] -> None
  | (var, value) :: t -> if x = var then Some value else lookup t x

let rec optimize env e = 
  match e with
  | Int x -> Int x
  | Bool x -> Bool x
  | ID x -> (match lookup env x with
              | Some y -> y
              | None -> ID x)
  | Binop (op, e1, e2) -> 
      let expr1 = optimize env e1 in
      let expr2 = optimize env e2 in
      (match (op, expr1, expr2) with
      
      | (Add, Int 0, x) -> x
      | (Add, x, Int 0) -> x
      | (Add, Int x, Int y) -> Int (x + y)

      | (Sub, x, Int 0) -> x
      | (Sub, Int x, Int y) -> Int (x - y)

      | (Mult, Int 0, _) -> Int 0
      | (Mult, _, Int 0) -> Int 0
      | (Mult, Int 1, x) -> x
      | (Mult, x, Int 1) -> x
      | (Mult, Int x, Int y) -> Int (x * y)

      | (Div, x, Int 0) -> raise DivByZeroError
      | (Div, x, Int 1) -> x
      | (Div, Int 0, _) -> Int 0
      | (Div, Int x, Int y) -> Int (x / y)

      | (Greater, Int x, Int y) -> Bool (x > y)
      | (Less, Int x, Int y) -> Bool (x < y)
      | (GreaterEqual, Int x, Int y) -> Bool (x >= y)
      | (LessEqual, Int x, Int y) -> Bool (x <= y)
      | (Equal, Int x, Int y) -> Bool (x = y)
      | (NotEqual, Int x, Int y) -> Bool (x <> y)
      
      | (Or, Bool true, _) -> Bool true
      | (Or, _, Bool true) -> Bool true
      | (Or, Bool x, Bool y) -> Bool (x || y)

      | (And, Bool false, _) -> Bool false
      | (And, _, Bool false) -> Bool false
      | (And, Bool x, Bool y) -> Bool (x && y)

      | _ -> Binop (op, expr1, expr2))

  | Not x -> (match optimize env x with
              | Bool b -> Bool (not b)
              | ex -> Not ex)
  | If (e1, e2, e3) -> 
      (match optimize env e1 with
      | Bool true -> optimize env e2
      | Bool false -> optimize env e3
      | expr1 -> If (expr1, optimize env e2, optimize env e3))

  | Fun (x, y, z) -> Fun (x, y, optimize ((x, ID x) :: env) z)

  | App (x, y) -> (match optimize env x with
     | Fun (x1, exp, y1) -> optimize ((x1, (optimize env y)) :: env) y1
     | _ -> App ((optimize env x), (optimize env y)))

  | Let (x, y, z) -> (optimize (extend env x y) z)

  | LetRec (x, t, e1, e2) -> LetRec (x, t, optimize env e1, optimize env e2)

  | Record lst -> Record (List.map (fun (l, e) -> (l, optimize env e)) lst)

  | Select (l, e) -> match optimize env e with
    | Record lst -> let rec helper fields = match fields with
          | [] -> None
          | (x, y) :: t -> if x = l then Some (optimize env y) else helper t 
        in
        (match helper lst with
         | Some value -> value 
         | None -> Select (l, optimize env e))
    | _ -> Select (l, optimize env e)
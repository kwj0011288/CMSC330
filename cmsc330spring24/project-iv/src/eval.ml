open Types

(* Provided functions - DO NOT MODIFY *)

(* Adds mapping [x:v] to environment [env] *)
let extend env x v = (x, ref v) :: env

(* Returns [v] if [x:v] is a mapping in [env]; uses the
   most recent if multiple mappings for [x] are present *)
let rec lookup env x =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value) :: t -> if x = var then !value else lookup t x

(* Creates a placeholder mapping for [x] in [env]; needed
   for handling recursive definitions *)
let extend_tmp env x = (x, ref (Int 0)) :: env

(* Updates the (most recent) mapping in [env] for [x] to [v] *)
let rec update env x v =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value) :: t -> if x = var then value := v else update t x v

(* Part 1: Evaluating expressions *)

(* Evaluates MicroCaml expression [e] in environment [env],
   returning an expression, or throwing an exception on error *)

let rec eval_expr env e = match e with
  | Int x -> Int x
  | Bool x -> Bool x
  | String x -> String x
  | ID x -> lookup env x
  | Not x -> (match eval_expr env x with
    | Bool x -> Bool (not x)
    | _ -> raise (TypeError "not error"))
  | Binop (oper, x, y) -> 
    (match (oper, (eval_expr env x), (eval_expr env y)) with
      | (Add, Int x, Int y) -> Int (x + y)
      | (Sub, Int x, Int y) -> Int (x - y)
      | (Mult, Int x, Int y) -> Int (x * y)
      | (Div, Int x, Int y) -> if y = 0 then raise DivByZeroError else Int (x / y)
      | (Concat, String x, String y) -> String (x ^ y)
      | (Equal, Int x, Int y) -> Bool (x = y)
      | (Equal, String x, String y) -> Bool (x = y)
      | (Equal, Bool x, Bool y) -> Bool (x = y)
      | (NotEqual, Int x, Int y) -> Bool (x <> y)
      | (NotEqual, String x, String y) -> Bool (x <> y)
      | (NotEqual, Bool x, Bool y) -> Bool (x <> y)
      | (Less, Int x, Int y) -> Bool (x < y)
      | (Greater, Int x, Int y) -> Bool (x > y)
      | (LessEqual, Int x, Int y) -> Bool (x <= y)
      | (GreaterEqual, Int x, Int y) -> Bool (x >= y)
      | (And, Bool x, Bool y) -> Bool (x && y)
      | (Or, Bool x, Bool y) -> Bool (x || y)
      | _ -> raise (TypeError "binop error"))
  | If (x, y, z) -> (match eval_expr env x with
    | Bool true -> eval_expr env y
    | Bool false -> eval_expr env z
    | _ -> raise (TypeError "if error"))
  | Let (x, y, z, g) ->
      let updated_env =
        if y then
          let envir = extend_tmp env x in
          let init = eval_expr envir z in
          update envir x init;
          envir
        else
          extend env x (eval_expr env z)
      in
      eval_expr updated_env g
  | Fun (x, y) -> Closure (env, x, y)
  | App (x, y) -> (match eval_expr env x with
    | Closure (a, b, c) ->
        eval_expr (extend a b (eval_expr env y)) c
    | _ -> raise (TypeError "app error"))
  | Record x -> Record x
  | Select (Lab x, y) -> 
      (match eval_expr env y with
      | Record a -> 
          let rec helper a = match a with
              | [] -> raise (SelectError ("Select Error"))
              | (Lab b, c) :: t ->
                  if b = x then c
                  else helper t
          in helper a
      | _ -> raise (TypeError "select error"))
  | Closure _ -> raise (TypeError "Closure error")
  | _ -> raise (TypeError "error")
  (* 
type expr =
  | Int of int
  | Bool of bool
  | String of string
  | ID of var
  | Fun of var * expr
  | Not of expr
  | Binop of op * expr * expr
  | If of expr * expr * expr
  | App of expr * expr
  | Let of var * bool * expr * expr
  | Closure of environment * var * expr
  | Record of (label * expr) list
  | Select of label * expr

*)

(* Part 2: Evaluating mutop directive *)

(* Evaluates MicroCaml mutop directive [m] in environment [env],
   returning a possibly updated environment paired with
   a value option; throws an exception on error *)
let eval_mutop env m = match m with
  | Def (x, y) -> 
    (( extend env x (eval_expr env y)), Some (eval_expr env y))
  | Expr x -> 
    (env, Some (eval_expr env x))
  | NoOp ->
    (env, None)

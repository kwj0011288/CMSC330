open Ast
open Utils

let extend env x v = (x, v) :: env

let rec lookup env x =
  match env with
  | [] -> raise (DeclareError ("Unbound type for " ^ x))
  | (var, value) :: t -> if x = var then value else lookup t x

let rec is_subtype t1 t2 = match t1, t2 with
  | TBool, TBool -> true
  | TInt, TInt -> true
  | TArrow (x1, y1), TArrow (x2, y2) -> 
      is_subtype x2 x1 && is_subtype y1 y2
  | TRec lst1, TRec lst2 ->
      let rec lookup env x =
        match env with
        | [] -> None
        | (Lab x1, y) :: _ when x = x1 -> Some y
        | _ :: t -> lookup t x
      in
      let rec check lst = match lst with
        | [] -> true
        | (Lab x, t2) :: t -> 
          match lookup lst1 x with
          | Some t1 -> if is_subtype t1 t2 then check t else false
          | None -> false
      in
      check lst2
  | TBottom, _ -> true
  | _, _ -> false

  (*
  type exptype =
  | TInt
  | TBool
  | TArrow of exptype * exptype
  | TRec of (label * exptype) list

(* MicroCaml AST *)
type expr =
  | Int of int
  | Bool of bool
  | ID of var
  | Fun of var * exptype * expr
  | Not of expr
  | Binop of op * expr * expr
  | If of expr * expr * expr
  | App of expr * expr
  | Let of var * expr * expr
  | LetRec of var * exptype * expr * expr
  | Record of (label * expr) list
  | Select of label * expr   
  *)
let rec typecheck gamma e = match e with
  | Int _ -> TInt
  | Bool _ -> TBool
  | ID x -> lookup gamma x
  | Fun (x, t1, e) -> TArrow (t1, typecheck (extend gamma x t1) e)
  | Not e -> if is_subtype (typecheck gamma e) TBool then TBool
      else raise (TypeError "Not error")
  | Binop (op, x, y) ->
    let t1 = typecheck gamma x in
    let t2 = typecheck gamma y in
    (match op with
      | Sub ->  if is_subtype t1 TInt && is_subtype t2 TInt then TInt
                else raise (TypeError "Sub error")
      | Add ->  if is_subtype t1 TInt && is_subtype t2 TInt then TInt
                else raise (TypeError "Add error")
      | Mult -> if is_subtype t1 TInt && is_subtype t2 TInt then TInt
                else raise (TypeError "Mult error")
      | Div ->  if is_subtype t1 TInt && is_subtype t2 TInt then TInt
                else raise (TypeError "Div error")
      | Greater -> if is_subtype t1 TInt && is_subtype t2 TInt then TBool
                      else raise (TypeError "Greater error")
      | Less ->  if is_subtype t1 TInt && is_subtype t2 TInt then TBool
                        else raise (TypeError "Less error")
      | GreaterEqual -> if is_subtype t1 TInt && is_subtype t2 TInt then TBool
                        else raise (TypeError "GreaterEqual error")
      | LessEqual -> if is_subtype t1 TInt && is_subtype t2 TInt then TBool
                        else raise (TypeError "LessEqual error")
      | Equal -> if is_subtype t1 t2 || is_subtype t2 t1 then TBool
                  else raise (TypeError "Equal error")

      | NotEqual -> if is_subtype t1 t2 || is_subtype t2 t1 then TBool
                    else raise (TypeError "NotEqual error")
      | And -> if is_subtype t1 TBool && is_subtype t2 TBool then TBool
                else raise (TypeError "And error")
      | Or -> if is_subtype t1 TBool && is_subtype t2 TBool then TBool
              else raise (TypeError "Or error"))

  | If (x, y, z) -> 
   (let t1 = typecheck gamma x in
    let t2 = typecheck gamma y in
    let t3 = typecheck gamma z in
    if t1 = TBool && t2 = t3 then t2 else raise (TypeError "If error"))
  | Let (x, e1, e2) -> typecheck (extend gamma x (typecheck gamma e1)) e2
  | LetRec (x, y, e1, e2) -> if (is_subtype (typecheck (extend gamma x y) e1) y) then typecheck (extend gamma x y) e2
      else raise (TypeError "Type error in let rec expression")
  | Record lst -> TRec (List.map (fun (Lab l, e) -> Lab l, typecheck gamma e) lst)
  | Select (Lab x, y) -> (let rec find lst label = match lst with
      | [] -> None
      | (Lab x, t) :: tail -> if x = label then Some t else find tail label in
          match typecheck gamma y with
          | TRec lst -> (match find lst x with
              | Some t -> t
              | None -> raise (SelectError "Trec error"))
          | _ -> raise (TypeError "Select error"))
  | App (e1, e2) -> (match (typecheck gamma e1) with
      | TArrow (x, y) -> if is_subtype (typecheck gamma e2) x then y  else raise (TypeError "Tarrow error")
      | _ -> raise (TypeError "App error"))
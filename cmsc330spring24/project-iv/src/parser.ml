open Types
open Utils

(* Provided functions - DO NOT MODIFY *)

(* Matches the next token in the list, throwing an error if it doesn't match the given token *)
let match_token (toks : token list) (tok : token) =
  match toks with
  | [] -> raise (InvalidInputException (string_of_token tok))
  | h :: t when h = tok -> t
  | h :: _ ->
      raise
        (InvalidInputException
           (Printf.sprintf "Expected %s from input %s, got %s"
              (string_of_token tok)
              (string_of_list string_of_token toks)
              (string_of_token h)))

(* Matches a sequence of tokens given as the second list in the order in which they appear, throwing an error if they don't match *)
let match_many (toks : token list) (to_match : token list) =
  List.fold_left match_token toks to_match

(* Return the next token in the token list as an option *)
let lookahead (toks : token list) =
  match toks with [] -> None | h :: t -> Some h

(* Return the token at the nth index in the token list as an option*)
let rec lookahead_many (toks : token list) (n : int) =
  match (toks, n) with
  | h :: _, 0 -> Some h
  | _ :: t, n when n > 0 -> lookahead_many t (n - 1)
  | _ -> None

(* Part 2: Parsing expressions *)

let rec parse_expr toks =
  match lookahead toks with
  | Some Tok_Let -> parse_let_expr toks
  | Some Tok_If -> parse_if_expr toks
  | Some Tok_Fun -> parse_fun_expr toks
  | _ -> parse_or_expr toks


  and parse_let_expr toks =
    let toks = match_token toks Tok_Let in
    let (toks, is_rec) = match lookahead toks with
      | Some Tok_Rec -> (match_token toks Tok_Rec, true)
      | _ -> (toks, false)
    in
    let (toks, id) = match lookahead toks with
      | Some (Tok_ID id) -> (match_token toks (Tok_ID id), id)
      | _ -> raise (InvalidInputException "let error")
    in
    let toks = match_token toks Tok_Equal in
    let (toks, expr1) = parse_expr toks in
    let toks = match_token toks Tok_In in
    let (toks, expr2) = parse_expr toks in
    (toks, Let (id, is_rec, expr1, expr2))
  

and parse_fun_expr toks =
  let toks = match_token toks Tok_Fun in
  let (toks, arg) =
    match lookahead toks with
    | Some (Tok_ID id) -> (match_token toks (Tok_ID id), id)
    | _ -> raise (InvalidInputException "fun error")
  in
  let toks = match_token toks Tok_Arrow in
  let (toks, expr1) = parse_expr toks in
  (toks, Fun (arg, expr1))

  and parse_if_expr toks = 
    let toks = match_token toks Tok_If in
    let (toks, exp1) = parse_expr toks in
    let toks = match_token toks Tok_Then in
    let (toks, exp2) = parse_expr toks in
    let toks = match_token toks Tok_Else in
    let (toks, exp3) = parse_expr toks in
    (toks, If (exp1, exp2, exp3))
  

(* and parse_if_expr toks = 
  match lookahead toks with
  | Some Tok_If ->
    let toks = match_token toks Tok_If in
    let (toks, exp1) = parse_expr toks in
    let toks = match lookahead toks with
      | Some Tok_Then -> match_token toks Tok_Then
      | _ -> raise (InvalidInputException "Expected 'then' after 'if'") in
    let (toks, exp2) = parse_expr toks in
    let toks = match lookahead toks with
      | Some Tok_Else -> match_token toks Tok_Else
      | _ -> raise (InvalidInputException "Expected 'else' after 'then'") in
    let (toks, exp3) = parse_expr toks in
    (toks, If (exp1, exp2, exp3))
  | _ -> raise (InvalidInputException "Unexpected token") *)

and parse_or_expr toks =
  let (toks, exp1) = parse_and_expr toks in
  match lookahead toks with
  | Some Tok_Or ->
    let toks = match_token toks Tok_Or in
    let (toks, exp2) = parse_or_expr toks in
    (toks, Binop (Or, exp1, exp2))
  | _ -> (toks, exp1)


and parse_and_expr toks =
  let (toks, exp1) = parse_equality_expr toks in
  match lookahead toks with
  | Some Tok_And ->
    let toks = match_token toks Tok_And in
    let (toks, exp2) = parse_and_expr toks in
    (toks, Binop (And, exp1, exp2))
  | _ -> (toks, exp1)

and parse_equality_expr toks =
  let (toks, exp1) = parse_relational_expr toks in
  match lookahead toks with
  | Some (Tok_Equal) ->
    let toks = match_token toks Tok_Equal in
    let (toks, exp2) = parse_equality_expr toks in
    (toks, Binop (Equal, exp1, exp2))
  | Some (Tok_NotEqual) ->
    let toks = match_token toks Tok_NotEqual in
    let (toks, exp2) = parse_equality_expr toks in
    (toks, Binop (NotEqual, exp1, exp2))
  | _ -> (toks, exp1)

and parse_relational_expr toks =
  let (toks, exp1) = parse_additive_expr toks in
  match lookahead toks with
  | Some (Tok_Less) ->
    let toks = match_token toks Tok_Less in
    let (toks, exp2) = parse_relational_expr toks in
    (toks, Binop (Less, exp1, exp2))
  | Some (Tok_Greater) ->
    let toks = match_token toks Tok_Greater in
    let (toks, exp2) = parse_relational_expr toks in
    (toks, Binop (Greater, exp1, exp2))
  | Some (Tok_LessEqual) ->
    let toks = match_token toks Tok_LessEqual in
    let (toks, exp2) = parse_relational_expr toks in
    (toks, Binop (LessEqual, exp1, exp2))
  | Some (Tok_GreaterEqual) ->
    let toks = match_token toks Tok_GreaterEqual in
    let (toks, exp2) = parse_relational_expr toks in
    (toks, Binop (GreaterEqual, exp1, exp2))
  | _ -> (toks, exp1)
  
and parse_mult_expr toks =
  let (toks, exp1) = parse_concat_expr toks in
  match lookahead toks with
  | Some (Tok_Mult) ->
    let toks = match_token toks Tok_Mult in
    let (toks, exp2) = parse_mult_expr toks in
    (toks, Binop (Mult, exp1, exp2))
  | Some (Tok_Div) ->
    let toks = match_token toks Tok_Div in
    let (toks, exp2) = parse_mult_expr toks in
    (toks, Binop (Div, exp1, exp2))
  | _ -> (toks, exp1)

and parse_concat_expr toks =
  let (toks, exp1) = parse_unary_expr toks in
  match lookahead toks with
  | Some Tok_Concat ->
    let toks = match_token toks Tok_Concat in
    let (toks, exp2) = parse_concat_expr toks in
    (toks, Binop (Concat, exp1, exp2))
  | _ -> (toks, exp1)

and parse_additive_expr toks =
  let (toks, exp1) = parse_mult_expr toks in
  match lookahead toks with
  | Some (Tok_Add) ->
    let toks = match_token toks Tok_Add in
    let (toks, exp2) = parse_additive_expr toks in
    (toks, Binop (Add, exp1, exp2))
  | Some (Tok_Sub) ->
    let toks = match_token toks Tok_Sub in
    let (toks, exp2) = parse_additive_expr toks in
    (toks, Binop (Sub, exp1, exp2))
  | _ -> (toks, exp1)
  
and parse_unary_expr toks =
  match lookahead toks with
  | Some Tok_Not ->
    let toks = match_token toks Tok_Not in
    let (toks, exp) = parse_unary_expr toks in
    (toks, Not exp)
  | _ -> parse_app_expr toks

   and parse_app_expr toks =
    let (toks, exp1) = parse_select_expr toks in
    match lookahead toks with
    | Some Tok_Int _ | Some Tok_Bool _ 
    | Some Tok_String _ | Some Tok_ID _ 
    | Some Tok_LParen | Some Tok_LCurly  ->
      let (toks, exp2) = parse_primary_expr toks in
      (toks, App (exp1, exp2))
    | _ -> (toks, exp1)
  
and parse_select_expr toks =
  let (toks, exp1) = parse_primary_expr toks in
  match lookahead toks with
  | Some Tok_Dot ->
    let toks = match_token toks Tok_Dot in
    (match lookahead toks with
      | Some (Tok_ID id) ->
        let toks = match_token toks (Tok_ID id) in
        (toks, Select (Lab id, exp1))
      | _ -> raise (InvalidInputException "select error"))
  | _ -> (toks, exp1)


and parse_primary_expr toks =
      match lookahead toks with
      | Some (Tok_Int x) -> (match_token toks (Tok_Int x), Int x)
      | Some (Tok_Bool y) -> (match_token toks (Tok_Bool y), Bool y)
      | Some (Tok_String z) -> (match_token toks (Tok_String z), String z)
      | Some (Tok_ID id) -> (match_token toks (Tok_ID id), ID id)
      | Some Tok_LParen ->
        let toks = match_token toks Tok_LParen in
        let (toks, e) = parse_expr toks in
        let toks = match_token toks Tok_RParen in
        (toks, e)
      | _ -> parse_record_expr toks
    
and parse_record_expr toks =
  let toks = match_token toks Tok_LCurly in 
  let (toks, record) = parse_record_body_expr toks in
  let toks = match_token toks Tok_RCurly in
  (toks, record)

  and parse_record_body_expr toks =
    match lookahead toks with
    | Some (Tok_ID id) ->
      let toks = match_token toks (Tok_ID id) in
      let toks = match_token toks Tok_Equal in
      let (toks, e) = parse_expr toks in
      let toks, more =
        match lookahead toks with
        | Some Tok_Semi -> 
          let toks = match_token toks Tok_Semi in
          parse_record_body_expr toks
        | _ -> (toks, Record []) 
      in
      (toks, Record ((Lab id, e) :: (match more with Record fields -> fields | _ -> failwith "Expected record")))
    | _ -> (toks, Record []) 
  


(* Part 3: Parsing mutop *)

let rec parse_mutop toks =
  match lookahead toks with
  | Some Tok_Def ->
    let (toks1, e) = parse_def toks in
    (toks1, e)
  | Some Tok_DoubleSemi ->
    let toks1 = match_token toks Tok_DoubleSemi in
    (toks1, NoOp)
  | _ ->
    let (toks1, e) = parse_expr_mutop toks in
    (toks1, e)

and parse_def toks =
  let toks1 = match_token toks Tok_Def in 
  let (toks1, idval) =
    match lookahead toks1 with
    | Some Tok_ID x -> (match_token toks1 (Tok_ID x), x)
    | _ -> raise (InvalidInputException "problem with parsing def") in
  let toks1 = match_token toks1 Tok_Equal in
  let (toks1, e) = parse_expr toks1 in
  let toks1 = match_token toks1 Tok_DoubleSemi in
  (toks1, Def(idval, e))

and parse_expr_mutop toks =
  let (toks1, e) = parse_expr toks in
  let toks1 = match_token toks1 Tok_DoubleSemi in
  (toks1, Expr e)

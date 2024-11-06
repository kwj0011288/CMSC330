type 'a tree = | BiNode of 'a tree * 'a * 'a tree | Leaf

type 'a flat = | Lf | Nd of 'a

type 'a n_tree = Node of 'a * 'a n_tree list

(* TODO: Implement the functions below *)

let rec flatten (input : 'a tree) : 'a flat list =
  match input with
  | Leaf -> [ Lf ]
  | BiNode (left, value, right) -> flatten left @ flatten right @ [ Nd value ]

let unflatten (input : 'a flat list) : 'a tree =
  let rev_input = List.rev input in
  let rec helper (input : 'a flat list) : 'a tree * 'a flat list =
    match input with
    | [] -> Leaf, []
    | Lf :: t -> Leaf, t
    | Nd h :: t ->
      let right, rest1 = helper t in
      let left, rest2 = helper rest1 in
      BiNode (left, h, right), rest2 in
  let tree, _ = helper rev_input in tree


let rec encode (input : 'a n_tree) : ('a * int) list =
  match input with
  | Node (value, num) ->
    (value, List.length num) :: List.fold_right (fun elem acc -> encode elem @ acc) num []

let decode (input : ('a * int) list) : 'a n_tree =
  (* 입력 리스트를 재귀적으로 처리하여 트리를 생성하는 도우미 함수 *)
  let rec helper input =
    match input with
    | [] -> failwith "Invalid_args" (* 입력 리스트가 비어있으면 예외 발생 *)
    | (value, num) :: t -> (* 리스트의 첫 번째 요소(노드 값과 자식 수)를 분해 *)
      if num = 0 then Node (value, []), t (* 자식 노드가 없는 경우, 현재 노드와 나머지 리스트 반환 *)
      else (
        (* 주어진 수만큼 자식 노드를 생성하기 위한 또 다른 도우미 함수 *)
        let rec helper2 n rest acc =
          if n = 0 then List.rev acc, rest (* 필요한 모든 자식 노드를 생성하면, 누적된 리스트와 나머지 입력 반환 *)
          else (
            let tree, t2 = helper rest in (* 다음 자식 노드 생성을 위해 helper 함수 재귀 호출 *)
            helper2 (n - 1) t2 (tree :: acc)) (* 생성된 자식 노드를 누적 리스트에 추가하고, 남은 자식 수를 감소시킴 *)
        in
        let children, t2 = helper2 num t [] in (* 자식 노드들을 생성 *)
        Node (value, children), t2) (* 현재 노드와 생성된 자식 노드들로 노드 생성, 나머지 입력과 함께 반환 *)
  in
  let tree, rest = helper input in (* 입력 리스트로부터 트리 생성 시작 *)
  match rest with
  | [] -> tree (* 모든 입력이 처리되었으면 최종 트리 반환 *)
  | _ -> failwith "Invalid_args" 

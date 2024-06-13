type 'a tree = BiNode of 'a tree * 'a * 'a tree | Leaf
type 'a flat = Lf | Nd of 'a
type 'a n_tree = Node of 'a * 'a n_tree list

let rec flatten (input : 'a tree) : 'a flat list =
  match input with
  | Leaf -> [Lf]
  | BiNode (left, value, right) ->
      flatten left @ flatten right @ [Nd value]

let rec build_tree (flat_list : 'a flat list) (stack : 'a tree list) : 'a tree * 'a tree list =
  match flat_list with
  | [] -> (match stack with
           | [] -> failwith "Invalid input: Not enough elements in the list"
           | [tree] -> (tree, [])
           | _ -> failwith "Invalid input: Not enough elements in the list")
  | Lf :: tl ->
      build_tree tl (Leaf :: stack)
  | Nd value :: tl ->
      match stack with
      | right_tree :: left_tree :: rest ->
          build_tree tl (BiNode (left_tree, value, right_tree) :: rest)
      | _ -> failwith "Invalid input: Not enough elements in stack"

let unflatten (input : 'a flat list) : 'a tree =
  fst(build_tree input [])

let rec encode (input : 'a n_tree) : ('a * int) list =
  match input with
  | Node (data, children) ->
      (data, List.length children) :: (List.concat_map encode children)

let rec decode_helper (nodes : ('a n_tree * int) list) (tpls : ('a * int) list) : 'a n_tree =
  match nodes with
  | [] -> failwith "decode: empty list"
  | (Node(value, children), remaining) :: tl ->
      if remaining = 0 then
        match tl with
        | [] -> Node (value, children)
        | (Node(v, c), rem_c) :: tail ->
            decode_helper ((Node(v, c @ [Node(value, children)]), rem_c - 1) :: tail)  tpls
      else
        match tpls with
        | [] -> failwith "decode: empty list"
        | (v, num_c) :: tail ->
            decode_helper ((Node (v, []), num_c) :: nodes) tail

let decode (input : ('a * int) list) : 'a n_tree =
  match input with
  | [] -> failwith "decode: empty input list"
  | (value, num_children) :: tl -> decode_helper [(Node (value, []), num_children)] tl
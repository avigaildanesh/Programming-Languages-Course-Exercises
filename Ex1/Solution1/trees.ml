
type 'a binary_tree =
  | Empty
  | Node of 'a * 'a binary_tree * 'a binary_tree ;;


let rec construct l =
  match l with
  | [] -> Empty
  | [h] -> Node (h, Empty, Empty)
  | h::t ->
    let left  = construct (List.filter (fun n -> n < h) t) in
    let right  = construct (List.filter (fun n -> n > h) t) in
    Node (h, left, right);;


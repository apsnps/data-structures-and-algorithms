type tree = Leaf | Node of int * tree * tree

let get_tree () : tree = 
  let n = read_int () in
  let rec read_tree i pairs_and_indices =
    if i > n then
      pairs_and_indices
    else
      let (a,b) = Scanf.scanf " %d %d" (fun a b -> a,b) in
      read_tree (i+1) (((a,b),i)::pairs_and_indices)
  in
  let max_index_with_children = n * 2 in
  let arr = Array.make max_index_with_children None in
  let get_branch index : tree =
    if index = -1 then
      Leaf
    else if index < max_index_with_children then 
      match arr.(index) with
      | None -> Node (index, Leaf, Leaf)
      | Some tree -> tree
    else
      Node (index, Leaf, Leaf)
  in
  let rec make_tree pairs_and_indices : tree =
    match pairs_and_indices with
    | [] ->
      begin match arr.(1) with
        | Some tree -> tree
        | None -> failwith "no tree"
      end
    | ((a,b),i)::tail ->
        arr.(i) <- Some (Node (i, get_branch a, get_branch b));
        make_tree tail
  in
  if n > 1 then make_tree (read_tree 1 [])
  else Node (1, Leaf, Leaf)

let swap_nodes k tree : tree =
  let rec go depth tree =
    match tree with
    | Leaf -> Leaf
    | Node (x, left, right) ->
        if depth mod k = 0 then
          Node (x, go (depth+1) right, go (depth+1) left)
        else
          Node (x, go (depth+1) left, go (depth+1) right)
  in
  go 1 tree

let rec print_tree tree =
  match tree with
  | Leaf ->
      ()
  | Node (x, left, right) ->
      print_tree left;
      print_int x; print_char ' ';
      print_tree right

let handle_swaps t tree =
  let rec go count tree =
    if count > t then
      ()
    else begin
      let k = Scanf.scanf " %d" (fun x -> x) in
      let new_tree = swap_nodes k tree in
      print_tree new_tree; print_newline ();
      go (count+1) new_tree end
  in
  go 1 tree
  
let () =
  let tree = get_tree () in
  let t = Scanf.scanf " %d" (fun x -> x) in
  handle_swaps t tree

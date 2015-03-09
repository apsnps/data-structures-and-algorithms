type 'a node = {
  value: 'a;
  mutable prev: 'a node option;
  mutable next: 'a node option
}

(* (first,last) :
 *   first is a reference to the first node of the list,
 *   last is a reference to the last node of the list.
 * Representation invariant: !first = None <=> !last = None. *)
type 'a t = 'a node option ref * 'a node option ref

let create () = (ref None, ref None)

let push_front value (first,last) =
  let new_first = {value; prev=None; next=None} in
  match !first with
  | None ->
      first := Some new_first;
      last := Some new_first
  | Some old_first ->
      new_first.next <- Some old_first;
      old_first.prev <- Some new_first;
      first := Some new_first

let push_back (first,last) value = 
  let new_last = {value; prev=None; next=None} in
  match !last with
  | None ->
      first := Some new_last;
      last := Some new_last
  | Some old_last ->
      old_last.next <- Some new_last;
      new_last.prev <- Some old_last;
      last := Some new_last

let pop_front (first,last) = 
  match !first with
  | None ->
      None
  | Some node -> 
      begin match node.next with
      | None -> last := None
      | Some second_node -> second_node.prev <- None
      end;
      first := node.next;
      Some node.value

let pop_back (first,last) = 
  match !last with
  | None -> None
  | Some node ->
      begin match node.prev with
      | None -> first := None
      | Some second_to_last_node -> second_to_last_node.next <- None
      end;
      last := node.prev;
      Some node.value

let get_value = function
  | None -> None
  | Some node -> Some node.value  

let peek_front (first,_) = get_value !first

let peek_back (_,last) = get_value !last  
      
let iter_left f (first,_) =
  let rec iter = function
    | Some node ->
        f node.value;
        iter node.next
    | None -> ()
  in
  iter !first
      
let iter_right f (_,last) =
  let rec iter = function
    | Some node ->
        f node.value;
        iter node.prev
    | None -> ()
  in
  iter !last

let copy lst =
  let lst' = create () in
  iter_left (push_back lst') lst;
  lst'

let to_list (_,last) =
  let rec go acc = function
    | None -> acc
    | Some node -> go (node.value::acc) node.prev
  in
  go [] !last

let of_list lst =
  let doubly = create () in
  List.iter (push_back doubly) lst;
  doubly

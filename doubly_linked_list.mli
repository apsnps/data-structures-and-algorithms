(* Imperative implementation of a doubly linked list with constant time push 
 * and pop to both the front and back of the list. 
 * Allows traversal in both directions.
 *
 * Uses mutable data!!! 
 *
 * Does not allow arbitrary placement of nodes into the list in constant time. 
 * Such a feature would require nodes to know which list they are in for 
 * better efficiency and error handling. *)

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

(* creates an empty doubly_linked_list *)
val create : unit -> 'a t

val push_front : 'a -> 'a t -> unit

val push_back : 'a t -> 'a -> unit

val pop_front : 'a t -> 'a option

val pop_back : 'a t -> 'a option

(* do not use push_back or pop_back in the function argument *)
val iter_left : ('a -> unit) -> 'a t -> unit

(* do not use push_front or pop_front in the function argument *)
val iter_right : ('a -> unit) -> 'a t -> unit

val copy : 'a t -> 'a t

val to_list : 'a t -> 'a list

val of_list : 'a list -> 'a t

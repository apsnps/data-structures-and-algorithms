open Doubly_linked_list

let remove_option = function
  | None -> failwith "remove_option"
  | Some x -> x

(* returns an array with the same length as input_array, where each element is
 * the minimum of the length window_size subarray of the input array starting 
 * at its index. i.e.,
 * output_array.(i) = 
 *   min {input_array.(i), input_array.(i+1), ..., input_array.(i+n-1)}. *)
let sliding_window_minimum (input: int array) (window_size: int) =
  let deque = Doubly_linked_list.create () in
  (* pops back of deque until value to insert is larger than the last element
   * in deque, then pushes value to back. this ensures deque is always sorted
   * in ascendoring order *)
  let rec ordered_push_back value =
    match peek_back deque with
    | None -> push_back deque value
    | Some x -> 
        if value < x then begin
          ignore (pop_back deque);
          ordered_push_back value end
        else
          push_back deque value
  in
  let length = Array.length input in
  let output = Array.make length 0 in
  for i = 0 to (min length window_size) - 1 do
    ordered_push_back input.(i)
  done;
  for i = 0 to length-1 do
    let front = remove_option (peek_front deque) in
    output.(i) <- front;
    (if front = input.(i) then ignore (pop_front deque) else ());
    if i + window_size < length then
      ordered_push_back input.(i + window_size)
    else
      ()
  done;
  output

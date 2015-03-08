open Doubly_linked_list

let print_int_option = function
  | None -> print_string "none "
  | Some value -> print_int value; print_char ' '

let () =
  let lst = create () in 
  push_front 3 lst;
  print_string "3 = "; iter_left print_int lst; print_newline ();
  print_string "3 = "; iter_right print_int lst; print_newline ();
  push_front 2 lst;
  print_string "23 = "; iter_left print_int lst; print_newline ();
  print_string "32 = "; iter_right print_int lst; print_newline ();
  push_front 1 lst;
  push_back lst 4;
  push_back lst 5;
  push_back lst 6;
  print_string "123456 = "; iter_left print_int lst; print_newline ();
  print_string "654321 = "; iter_right print_int lst; print_newline ();
  print_newline ();

  let native_lst = to_list lst in
  print_string "123456 = "; List.iter print_int native_lst; print_newline ();
  print_newline ();

  print_string "1 2 6 5 = ";
  print_int_option (pop_front lst);
  print_int_option (pop_front lst);
  print_int_option (pop_back lst);
  print_int_option (pop_back lst);
  print_newline ();
  print_string "34 = "; iter_left print_int lst; print_newline ();
  print_string "43 = "; iter_right print_int lst; print_newline ();
  push_front 0 lst;
  push_back lst 7;
  print_string "0347 = "; iter_left print_int lst; print_newline ();
  print_string "7430 = "; iter_right print_int lst; print_newline ();

  let lst' = copy lst in
  print_string "7 4 3 0 = "; 
  iter_right (fun _ -> print_int_option (pop_back lst)) lst;
  print_newline ();
  print_string "0 3 4 7 = "; 
  iter_left (fun _ -> print_int_option (pop_front lst')) lst';
  print_newline ();

  print_string "none none = ";
  print_int_option (pop_front lst);
  print_int_option (pop_back lst);
  print_newline (); print_newline ();

  let l1 = create () in
  push_back l1 1;
  push_back l1 2;
  push_back l1 3;
  let l2 = copy l1 in
  push_back l1 4;
  push_back l1 5;
  push_back l1 6;
  print_string "123456 = "; iter_left print_int l1; print_newline ();
  print_string "123 = "; iter_left print_int l2; print_newline ();
  print_string "654321 = "; iter_right print_int l1; print_newline ();
  print_string "321 = "; iter_right print_int l2; print_newline ();
  print_newline ();

  let native_lst2 = [11;12;13;14;15] in
  let lst2 = of_list native_lst2 in
  print_string "11 12 13 14 15 = "; 
  iter_left (fun x -> print_int x; print_char ' ') lst2; print_newline ();
  push_back lst2 16;
  push_front 10 lst2;
  print_string "10 11 12 13 14 15 16 = "; 
  iter_left (fun x -> print_int x; print_char ' ') lst2; print_newline ();
  print_newline ();

open Alcotest
open Utilities.Lists

let test_contains_slice () =
  let lst1 = [1; 2; 3; 4; 5; 6; 7; 8; 9] in
  let needle1 = [1; 2; 3] in 
  let needle2 = [7; 8; 9] in
  let needle3 = [4;5] in 
  let bad_needle1 = [3; 2] in
  let bad_needle2 = [2; 4] in

  check bool "Lst contains [1; 2; 3]" true (contains_slice lst1 needle1);
  check bool "Lst did not contain [3; 2]" false (contains_slice lst1 bad_needle1);
  check bool "Lst contains [7; 8; 9]" true (contains_slice lst1 needle2);
  check bool "Lst did not contain [2; 4]" false (contains_slice lst1 bad_needle2);
  check bool "Lst contains [4;5]" true (contains_slice lst1 needle3);
  (* End of test *)
  ()

let test_contains_char_slice () = 
  let lst = ['a'; 'b'; 'c'; 'd'; 'e'] in 
  let needle = ['b';'c'] in 
  let bad_needle1 = ['b';'d'] in 

  check bool "Lst contains ['b';'c']" true (contains_slice lst needle);
  check bool "Lst does not contain ['b';'d']" false (contains_slice lst bad_needle1);
  (* end of test *)
  ()

  let test_contains_els () =
    let lst1 = [1; 2; 3; 4; 5; 6; 7; 8; 9] in
    let needle1 = [1; 2; 3] in 
    let needle2 = [7; 8; 9] in
    let needle3 = [4;7] in 
    let bad_needle1 = [3; 11] in
    let bad_needle2 = [0; 2] in
  
    check bool "Lst contains [1; 2; 3]" true (contains_els lst1 needle1);
    check bool "Lst did not contain [3; 2]" false (contains_els lst1 bad_needle1);
    check bool "Lst contains [7; 8; 9]" true (contains_els lst1 needle2);
    check bool "Lst did not contain [2; 4]" false (contains_els lst1 bad_needle2);
    check bool "Lst contains [4;5]" true (contains_els lst1 needle3);
    (* End of test *)
    ()

let () = 
  let open Alcotest in 
  run "Tests" [
    "List Tests", [
      test_case "Integer Slice tests" `Quick test_contains_slice; 
      test_case "Char Slice tests" `Quick test_contains_char_slice; 
      test_case "Test Slice element tests" `Quick test_contains_els;
    ];
  ]
open OUnit2
open Functions
open Printf

let t_string name value expected = name>::
  (fun ctxt -> assert_equal expected value ~printer:(fun x -> x))

let t_int name value expected = name>::
  (fun ctxt -> assert_equal expected value ~printer:string_of_int)

let t_list name value expected = name>::
  (fun ctxt -> assert_equal expected value)

let bt1 = Node("a", Leaf, Leaf);;
let bt2 = Node("a", Node("b", Leaf, Leaf), Node("c", Leaf, Leaf));;
let bt3 = Leaf;;
let bt4 = Node("a", Node("b", Leaf, Node("c", Leaf, Leaf)), Node("d", Leaf, Leaf));;
let bt5 = Node("a", Node("b", Node("c", Node("d", Node("e", Leaf, Leaf),
      Leaf), Leaf), Leaf), Leaf);;

let ls1 = [1];;
let ls2 = [1;2];;
let ls3 = [-1;-2;3];;
let ls4 = [-99999;-4;-3;-2;-1;0;1;2;3;4;5;6;7;8;9;10;11;12;10000;20000];;
let ls5 = [5000000];;
let emptyL = [];;

let sl1 = [""];;
let sl2 = ["1"];;
let sl3 = ["abcd";"bcd";"cd";"d"];;


let suite =
"suite">:::
 [

 t_int "fact" (1 + 1) 2;
 t_int "min1" (min 4 5) 4;
 t_int "min2" (min 1 1) 1;
 t_int "min3" (min (-2) 2) (-2);
 t_int "min4" (min 0 0) 0;
 t_int "min5" (min 4 1000) 4;

(* for fibonacci *)
 t_int "fib1" (fibonacci 4) 3;
 t_int "fib2" (fibonacci 1) 1;
 t_int "fib3" (fibonacci 2) 1;
 t_int "fib4" (fibonacci 0) 0;
 t_int "fib5" (fibonacci 15) 610;
 t_int "fib6" (fibonacci 31) 1346269;

(* for strings *)
 t_string "inorder1" (inorder_str bt1) "a";
 t_string "inorder2" (inorder_str bt2) "bac";
 t_string "inorder3" (inorder_str bt3) "";
 t_string "inorder4" (inorder_str bt4) "bcad";
 t_string "inorder5" (inorder_str bt5) "edcba";

(* exercise 3.3.6 *)
(* tests for size *)
 t_int "size1" (size bt1) 1;
 t_int "size2" (size bt2) 3;
 t_int "size3" (size bt3) 0;
 t_int "size4" (size bt4) 4;
 t_int "size5" (size bt5) 5;

(* tests for height *)
 t_int "height1" (height bt1) 1;
 t_int "height2" (height bt2) 2;
 t_int "height3" (height bt3) 0;
 t_int "height4" (height bt4) 3;
 t_int "height5" (height bt5) 5;

(* tests for increment_all *)
 t_list "inc1" (increment_all ls1) [2]; 
 t_list "inc2" (increment_all ls2) [2;3]; 
 t_list "inc3" (increment_all ls3) [0;-1;4]; 
 t_list "inc4" (increment_all ls4) [-99998;-3;-2;-1;0;1;2;3;4;5;6;7;8;9;10;11;12;13;10001;20001];
 t_list "inc5" (increment_all ls5) [5000001]; 
 t_list "incempty" (increment_all emptyL) []; 
 
(* tests for long_strings *)
 t_list "long_strings1" (long_strings sl1 0) [];
 t_list "long_strings2" (long_strings sl1 1) [];
 t_list "long_strings3" (long_strings sl2 0) ["1"];
 t_list "long_strings4" (long_strings sl2 1) [];
 t_list "long_strings5" (long_strings sl3 0) ["abcd";"bcd";"cd";"d"];
 t_list "long_strings6" (long_strings sl3 1) ["abcd";"bcd";"cd"];
 t_list "long_strings7" (long_strings sl3 2) ["abcd";"bcd"];
 t_list "long_strings8" (long_strings sl3 3) ["abcd"];
 t_list "long_strings9" (long_strings sl3 4) [];
 t_list "long_strings10" (long_strings [""] (-1)) [""];
 t_list "long_strings_empty" (long_strings [] 0) [];

(* tests for every_other *)
 t_list "every_other1" (every_other []) [];
 t_list "every_other2" (every_other ["1"]) ["1"];
 t_list "every_other3" (every_other ["123";"abc";"bcd"]) ["123";"bcd"];
 t_list "every_other4" (every_other [2;3;4;-1]) [2;4];
 t_list "every_other5" (every_other [1;1000;1000000]) [1;1000000];

(* tests for sum_all *)
 t_list "sum_all1" (sum_all []) [];
 t_list "sum_all2" (sum_all [[1]]) [1];
 t_list "sum_all3" (sum_all [[1;2;3];[2;3;4]]) [6;9];
 t_list "sum_all4" (sum_all [[-5;5]]) [0];
 t_list "sum_all5" (sum_all [[1;10;100;1000;10000;100000];[2;20;200;2000];[3]]) [111111;2222;3];
 
 ]
;;

run_test_tt_main suite

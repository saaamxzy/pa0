(* exercise 2.1 *)
let rec fibonacci (n : int) : int =
  if n == 0 then 0
	else if n <= 2 then 1
	else (fibonacci (n - 1)) + (fibonacci (n - 2))
;;

(*
  evaluation of (fibonacci 4) in substitution style:
  
  (fibonacci 4)
  => 
  if 4 == 0 then 0
	else if 4 <= 2 then 1
	else (fibonacci (4 - 1)) + (fibonacci (4 - 2))
  =>
  if false then 0
	else if 4 <= 2 then 1
	else (fibonacci (4 - 1)) + (fibonacci (4 - 2))
  =>
  if false then 0
	else false then 1
	else (fibonacci (4 - 1)) + (fibonacci (4 - 2))
  =>
  (fibonacci (4 - 1)) + (fibonacci (4 - 2))
  =>
  (fibonacci 3) + (fibonacci 2)
  =>
  (
  if 3 == 0 then 0
	else if 3 <= 2 then 1
	else (fibonacci (3 - 1)) + (fibonacci (3 - 2))
  ) +
  (
  if 2 == 0 then 0
	else if 2 <= 2 then 1
	else (fibonacci (2 - 1)) + (fibonacci (2 - 2))
  )
  =>
  ( (fibonacci 2) + (fibonacci 1) ) + 
  (if true then 1
	else (fibonacci (2 - 1)) + (fibonacci (2 - 2))
  )
  =>
  (1 + 1) + 1
  =>
  3

*)

(* exercise 3.3 *)
type btnode =
  | Leaf
  | Node of string * btnode * btnode
;;

let rec inorder_str (bt : btnode) : string =
  match bt with
    | Leaf -> ""
    | Node(s, left, right) ->
      (inorder_str left) ^ s ^ (inorder_str right)

(*
  exercise 3.3.3:

  let bt = Node("a", Node("b", Leaf, Leaf), Node("c", Leaf, Leaf));;
  visualization:
      a
     / \
    b   c
  
  (inorder_str bt)
  =>
  match Node("a", Node("b", Leaf, Leaf), Node("c", Leaf, Leaf)) with
    | Leaf -> ""
    | Node(s, left, right) ->
      (inorder_str left) ^ s ^ (inorder_str right)
  =>
  (inorder_str Node("b", Leaf, Leaf)) ^ "a" ^ (inorder_str Node("c", Leaf, Leaf)
  =>
  (
  match Node("b", Leaf, Leaf) with
    | Leaf -> ""
    | Node(s, left, right) ->
      (inorder_str left) ^ s ^ (inorder_str right)
  ) ^ "a" ^
  (
  match Node("c", Leaf, Leaf) with
    | Leaf -> ""
    | Node(s, left, right) ->
      (inorder_str left) ^ s ^ (inorder_str right)
  )
  =>
  (inorder_str Leaf) ^ "b" ^ (inorder_str Leaf)
  ^ "a" ^
  (inorder_str Leaf) ^ "c" ^ (inorder_str Leaf)
  =>
  (
  match Leaf with
    | Leaf -> ""
    | Node(s, left, right) ->
      (inorder_str left) ^ s ^ (inorder_str right)
  ) 
  ^ "b" ^
  (
  match Leaf with
    | Leaf -> ""
    | Node(s, left, right) ->
      (inorder_str left) ^ s ^ (inorder_str right)
  )
  ^ "a" ^
  (
  match Leaf with
    | Leaf -> ""
    | Node(s, left, right) ->
      (inorder_str left) ^ s ^ (inorder_str right)
  )
  ^ "c" ^
  (
  match Leaf with
    | Leaf -> ""
    | Node(s, left, right) ->
      (inorder_str left) ^ s ^ (inorder_str right)
  ) 
  =>
  "" ^ "b" ^ "" ^ "a" ^ "" ^ "c" ^ ""
  =>
  "bac"


*)

(* exercise 3.3.4 *)
let rec size (bt : btnode) : int =
  match bt with
    | Leaf -> 0
    | Node (s, left, right) ->
      1 + size(left) + size(right)
;; 

(* exercise 3.3.5 *)
let rec height (bt : btnode) : int =
  match bt with
    | Leaf -> 0
    | Node (s, left, right) ->
      1 + (max (height left) (height right))
;;

(* exercise 4.3.1 *)
let rec increment_all (l : int list) : int list =
  match l with
    | [] -> []
    | first::rest -> (first+1)::(increment_all rest)
;;

(* exercise 4.3.2 *)
let rec long_strings (l : string list) (n : int) : string list =
  match l with
    | [] -> []
    | first::rest ->
      if (String.length first) > n then first::(long_strings rest n)
      else (long_strings rest n)
;;

(* exercise 4.3.3 *)
let rec every_other (l : 'a list) : 'a list =
  match l with
    | [] -> []
    | [one] -> [one] 
    | first::second::rest ->
      first::(every_other rest)
;;

(* exercise 4.3.4 *)
(*
let rec sum2 (l : int list) int =
  match l with
    | [] -> 0
    | first::rest -> (first + (sum2 rest))
;;
*)

let rec sum (l : int list) : int =
  match l with
    | [] -> 0
    | first::rest -> first + (sum rest)
;;

let rec sum_all (l : int list list) : int list =
  match l with
    | [] -> []
    | first::rest -> (sum first)::(sum_all rest)
;;


(* You will fill your implementations in this file *)

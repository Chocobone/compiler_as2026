(* CSE322 Compiler Assignment 0 - Task 2 *)

exception NotImplemented

type 'a tree = Leaf of 'a | Node of 'a tree * 'a * 'a tree

(* sum t: return the sum of all values *)
let rec sum t = match t with
  | Leaf x -> x
  | Node (t1,x,t2) -> (sum t1) + x + (sum t2)

(* exist t n: return true if n exists in a tree *)
let rec exist t n = match t with
  | Leaf x -> x = n
  | Node (t1,x,t2) -> exist t1 n || exist t2 n || x = n

(* count t n: count n in a tree *)
let rec count t n = match t with
  | Leaf x -> if x = n then 1 else 0
  | Node (t1,x,t2) -> count t1 n + count t2 n + (if x = n then 1 else 0)

(* inorder t: return the list of values using inorder tree traversal *)
let rec inorder t = match t with
  | Leaf x -> [x]
  | Node (t1,x,t2) -> inorder t1 @ [x] @ inorder t2

(* depth t: return the depth of a tree*)
let rec depth t = match t with
  | Leaf x -> 0
  | Node (t1,x,t2) -> 1 + max (depth t1) (depth t2)

(* max t: return the maximum value in a tree*)
let rec max t = match t with
  | Leaf x -> x
  | Node (t1,x,t2) ->
    let max_t1 = max t1 in
    let max_t2 = max t2 in
    if max_t1 > max_t2 then max_t1 else max_t2

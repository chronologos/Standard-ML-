(* Recursive datatype definition of a btree *)
datatype 'a tree = 
  Empty |
  Node of 'a tree * 'a * 'a tree

(* initialize btrees *)
val btree = Node (Empty, 1, Empty)
val btree2 = Node(btree, 2, btree)

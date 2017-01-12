(* Recursive datatype definition of a btree *)
datatype 'a tree = 
Empty |
Node of 'a tree * 'a * 'a tree

(* initialize btrees *)
val btree = Node (Empty, 1, Empty)
val btree2 = Node(btree, 2, btree)

fun search (target, Empty) = NONE
  | search (target, Node(l,v,r)) = if v = target
                                   then SOME v
                                   else (case search (target, l)
                                           of SOME x => SOME x
                                            | NONE => search
                                            (target,r))

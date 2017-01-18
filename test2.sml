datatype foo = 
X of int
             | Y of string * bool
             | Z

val someList = [1,2,3,4,5]

fun myFun (X x) = x
  | myFun (Y(s,b)) = if b then size s else 42
  | myFun Z = 0

datatype 'a tree =
NODE of 'a * 'a tree * 'a tree
                 | LEAF

datatype 'a option = 
SOME of 'a
                   | NONE


datatype suit = Spades | Clubs | Hearts | Diamonds

fun outranks (Spades, Spades) = false
  | outranks (Spades, _) = true
  | outranks (Hearts, Spades) = false
  | outranks (Hearts, Hearts) = false
  | outranks (Hearts, _) = true
  | outranks (Diamonds, Clubs) = true
  | outranks (Diamonds, _) = false
  | outranks (Clubs, _) = false



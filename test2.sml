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

fun min (a, b) = if (a > b) then b else a

fun fib 1 = 1
  | fib 0 = 1
  | fib n = fib (n-1) + fib(n-2)

fun isPrime 1 = false
  | isPrime 2 = true
  | isPrime n = 
  let fun notDivisible (dividend, divisor) =
  if (divisor >= dividend) then true
  else
    if (dividend mod divisor) = 0 then false else notDivisible(dividend, divisor + 1)
  in
    notDivisible (n, 2)
  end

fun sumList [] = 0
  | sumList (x::xs) =
  let fun sumHelper ([], sum) = sum
    | sumHelper (x::xs, sum) = sumHelper(xs, sum + x)
  in
    sumHelper(x::xs, 0)
  end

datatype suit = Spades | Clubs | Hearts | Diamonds

fun outranks (Spades, Spades) = false
  | outranks (Spades, _) = true
  | outranks (Hearts, Spades) = false
  | outranks (Hearts, Hearts) = false
  | outranks (Hearts, _) = true
  | outranks (Diamonds, Clubs) = true
  | outranks (Diamonds, _) = false
  | outranks (Clubs, _) = false



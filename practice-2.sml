datatype expr =
  NUM of int
| PLUS of expr * expr
| MINUS of expr * expr
| TIMES of expr * expr
| DIV of expr * expr
| F of expr list * (int list -> int)

val z = PLUS(NUM(1), NUM(2));
fun eval (NUM(x)) = x 
  | eval (PLUS(x, y)) = (eval(x) + eval(y))
  | eval (MINUS(x, y)) = (eval(x) - eval(y))
  | eval (TIMES(x, y)) = (eval(x) * eval(y))
  | eval (DIV(x, y)) = (eval(x) div  eval(y))
  | eval (F(l, f:(int list -> int))) =
  let fun mapEval nil = nil
    | mapEval (x::xs) = (eval x) :: (mapEval xs)
  in
    f (mapEval l)
  end


fun testFun nil = 1
  | testFun [e] = e
  | testFun (h::t) = h + testFun(t)

val evalAns = eval(F([NUM(5),NUM(2)], testFun))

fun foldl f ans [] = ans 
  | foldl f ans (a::l) = foldl f (f(a,ans)) l

fun foldr f ans [] = ans
  | foldr f ans (a::l) = f(a, foldr f ans l)

fun flatten l: 'Z list =
  let fun appender (nil, res) = res
    | appender (x::xs, res) = (x :: appender(xs, res))
  in 
    foldr appender [] l
  end
    

val listoflists = [[1,2,3], [4], [5,6], [], [7]]
val flattenAns = flatten listoflists

(* tuple version *)
fun mapFold (f, []) = []
  | mapFold (f, h::t) = 
  let fun mapper (x, l) = (f x ) :: l
  in 
    foldr mapper [] (h::t)
  end

(* curried, non-tuple version *)
fun mapFold2 f [] = []
  | mapFold2 f (h::t) = foldr (fn (x, l) => (f x) :: l)  [] (h::t)

fun add1 x = x + 1
val mapAns1 = mapFold (add1, [1,2,3,4])
val mapAns2 = mapFold2 add1 [1,2,3,4]

fun filterFold f [] = []
  | filterFold f (h::t) = foldr (fn (x, l) => if f x then (x :: l) else l) [] (h::t)

val filterAns = filterFold (fn x => if x mod 2 = 0 then true else false) [1,2,3,4]

fun countFold f [] = 0
  | countFold f (h::t) = foldr (fn (x, count) => if f x then count + 1 else count) 0 (h::t)

val countAns = countFold (fn x => if x mod 2 = 0 then true else false) [1,2,3,4]

(* mapPartial: (’a -> ’b option) -> ’a list -> ’b list *)
fun mapPartial f [] = []
  | mapPartial f (h::t) = foldr (fn (x, l) => if isSome (f x) then (x::l) else l) [] (h::t)

val mapPartialAns = mapPartial (fn x => if x mod 2 = 0 then SOME(x) else NONE) [1,2,3,4]

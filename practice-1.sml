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

fun squareList [] = [] 
  | squareList [x] = [x*x]
  | squareList (x::xs) = (x*x) :: squareList xs;

squareList [1,2,3,4,5,6,~1]

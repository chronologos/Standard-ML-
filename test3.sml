fun map' (f, nil) = nil
  | map' (f, h::t) = (f h) :: map' (f, t)

fun add1 x = x + 1

val constantly = fn k => (fn a => k)

(* curried version of map *)
fun map f nil = nil
  | map f (h::t) = (f h) :: (map f t)

fun fact x = 
  let fun help n ans = if n <= 0 then ans
                      else help (n-1) (ans*n)
  in
    help x 1
  end

fun rev nil = nil
  | rev (x::xs) = (rev xs) @ [x]

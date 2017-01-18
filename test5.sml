(* another test of currying *)
fun max gt = 
  let fun lp curr [] = curr
    | lp curr (x::xs) = if gt(curr,x)
                        then lp curr xs
                        else lp x xs
  in
    lp
  end

val it = max op >
val itt = it 1
val ittt = itt [100,1000,1]


(*Well... every function in SML takes one argument.  Bu the lp function takes "curr" then returns a function which takes the second argument.  It pattern matches that argument against [] in one case, and a::l in the other.  That is, the declaration is short hand for*)

(*fun lp curr = fn lst => case lst of   [] => curr | a::l => if gt(a,curr) then lp a l else lp curr l*)

(*2.  ('a * 'a -> bool) -> 'a -> 'a list -> 'a*)
(*means that his function takes a ('a* 'a -> bool) function and returns a ('a -> 'a list -> 'a function).*)
(*That is, the returned value of max f has  type ('a -> 'a list -> 'a function) [for whatever 'a the function f operates on].  That type means that this function takes a 'a and returns a ('a -> list function)---i.e., if you call it on some 'a (which is what gets returned if the list is empty, and otherwise compared to the first element, etc), then it gives you back a function which takes a ('a list) and gives you back a 'a (the largest thing in the list, assuming it is not empty, and at least one element is greater than the starting point).*)

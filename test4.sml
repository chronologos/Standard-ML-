(* val reduce = fn : 'a * ('b * 'a -> 'a) * 'b list -> 'a  *)
fun reduce(unit, opn, nil) = unit
  | reduce(unit, opn, h::t) = 
  opn(h, reduce(unit, opn, t))

val l = [1,2,3];
reduce(0, op +, l);
reduce(1, op *, l);
reduce(nil, op ::, l);


(*red is bound to a closure consisting of the code for the function together
with the environment active at the point of definition, which will provide
bindings for unit and opn arising from the application of better reduce
to its arguments. Furthermore, the recursive calls to red no longer carry
bindings for unit and opn, saving the overhead of creating tuples on each
iteration of the loop.*)
fun better_reduce(unit, opn, l) =
  let
    fun red nil = unit
      | red (h::t) = opn (h, red t)
  in
    red l
  end


fun curried_reduce (unit, opn) nil = unit
  | curried_reduce (unit, opn) (h::t) =
  opn(h, curried_reduce (unit, opn) t)


(* unravelling the function *)
val constantly = fn k => (fn a => k);
fun constantly k a = k;

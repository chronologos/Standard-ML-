(* in repl, do:
 * CM.make "p3.cm"; 
 * before doing:
 * use "practice-3.sml" 
   functor is like parametrized structure *)

fun removeNL "" = ""
  | removeNL s = if String.isSuffix "\n" s 
                 then String.substring(s, 0, (String.size s - 1)) 
                 else s

functor F(M: ORD_MAP where type Key.ord_key = string)
         (S: ORD_SET where type Key.ord_key = string) :>
sig
  val proc: string list -> S.set M.map
end
  =
  struct

    (* helper fn: tokenize takes a line and breaks it into a list of words while
       removing trailing newlines*)
    fun tokenize "" = []
      | tokenize s = List.map removeNL (String.tokens (fn c => if c = #" " then true
                                                               else false) s)

    (* helper fn: reads in text file and generate map of words to sets
       generates map of sets for one file *)
    fun readTextFile(infile: string):S.set M.map =
      let
        val ins = TextIO.openIn infile
        fun helper(copt: string option) l =
          case copt of
               NONE => (TextIO.closeIn ins; l)
             | SOME(c) => (print(c); helper(TextIO.inputLine ins) (tokenize c @ l))

        fun mappify [] M = M
          | mappify (x::xs) M = mappify xs (M.insert(M, x,
          S.add(S.empty,infile)))
      in
        mappify (helper (TextIO.inputLine ins) []) M.empty
      end

    (* helper function: union two maps of sets *)
    fun mergeMaps m1 m2 = M.unionWith S.union (m1, m2)

    fun proc [] = M.empty 
      | proc (x::xs) =
      let fun helperProc [] m = m
        | helperProc (h::t) m = helperProc t (mergeMaps (readTextFile h) m)
      in
        helperProc (x::xs) M.empty
      end
end

(* test 1 *)
structure SetS = SplaySetFn (struct
  type ord_key = string
  val compare = String.compare
end)


structure MapM = SplayMapFn (struct
  type ord_key = string
  val compare = String.compare
end)

structure myThingy = F(MapM)(SetS)
val res = myThingy.proc ["b.txt", "a.txt"]
val resDisp = List.map (fn (k,v) => (k,SetS.listItems v)) (MapM.listItemsi res)

(* test 2 *)
structure SetS2 = BinarySetFn (struct
  type ord_key = string
  val compare = String.compare
end)

structure MapM2 = BinaryMapFn (struct
  type ord_key = string
  val compare = String.compare
end)

structure myThingy2 = F(MapM2)(SetS2)
val res2 = myThingy2.proc ["b.txt", "a.txt"]
val resDisp2 = List.map (fn (k,v) => (k,SetS2.listItems v)) (MapM2.listItemsi
res2)

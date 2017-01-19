structure SetS = SplaySetFn (struct
  type ord_key = string
  val compare = String.compare
end)

val a = SetS.empty
val b = SetS.add(a,"hello")

structure MapM = SplayMapFn (struct
  type ord_key = string
  val compare = String.compare
end)

val c = MapM.empty
val d = MapM.insert(c, "hello", b);

fun removeNL "" = ""
  | removeNL s = if String.isSuffix "\n" s 
                 then String.substring(s, 0, (String.size s - 1)) 
                 else s

fun tokenize "" = []
  | tokenize s = String.tokens (fn c => if c = #" " then true else false) s

(* tokenize that removes trailing newlines from strings *)
fun tokenize2 "" = []
  | tokenize2 s = List.map removeNL (String.tokens (fn c => if c = #" " then true
                                                           else false) s)

val s = tokenize "hello my name is ian"

fun readTextFile(infile: string) =
  let
    val ins = TextIO.openIn infile
    fun helper(copt: string option) l =
      case copt of
           NONE => (TextIO.closeIn ins; l)
         | SOME(c) => (print(c); helper(TextIO.inputLine ins) (tokenize2 c @ l))
    fun mappify [] M = M
      | mappify (x::xs) M = mappify xs (MapM.insert(M, x,
      SetS.add(SetS.empty,infile)))
  in
    (*helper (TextIO.inputLine ins) []*)
    mappify (helper (TextIO.inputLine ins) []) MapM.empty
  end
val ans = readTextFile("a.txt")
val ans2 = MapM.listItemsi(ans)
val ans3 = readTextFile("b.txt")
val ans4 = MapM.listItemsi(ans3)

fun mergeMaps m1 m2 = MapM.unionWith SetS.union (m1, m2)
val ans5 = mergeMaps ans ans3
val ans6 = MapM.listItemsi(ans5)

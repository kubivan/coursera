(*this is simple comment*)
fun incr(x : int) =
let 
  val some_local = 1
in
  x + some_local
end;

(* файний комент *)
fun sum_of_tuple(tup : int * int * int )  = 
  #1 tup;

fun sum_list(l : int list )=
  if null l then
  0
  else
    hd l + sum_list(tl l);

(* date: year *)


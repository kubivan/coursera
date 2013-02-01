(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
fun all_except_option2 (str : string, sl : string list ) =
let fun aux(str : string, sl : string list ) =
      case sl of
       [] => []
       | h::t => if same_string(str,h) = false then
                    h::aux(str,t)
                 else
                    aux(str,t)
in
     let val ans = aux(str, sl)
     in case ans of
        [] => NONE
        | _ => SOME (ans)
     end
end;

fun all_except_option(str : string, sl: string list)=
let
  fun exist(str, sl: string list)=
    case sl of
         [] => false
       | hd::tl => if same_string(hd,str) then true else exist(str, tl)
  fun aux(str : string, sl : string list ) =
    case sl of
         [] => []
       | h::t => if same_string(str,h) then aux(str,t) else h::aux(str,t)

in
  if exist(str, sl) = false then NONE
  else
    SOME(aux(str, sl))
end

fun get_substitutions1(names_list : string list list, name : string)=
  case names_list of
       [] => []
     | name_list_head::name_list_tail =>
         case all_except_option(name, name_list_head) of
              NONE => get_substitutions1(name_list_tail, name)
            | SOME names => names @ get_substitutions1(name_list_tail,name)

fun get_substitutions2(names_list : string list list, name : string)=
let
  fun aux(names_list : string list list, name : string, acc : string list)=
    case names_list of
         [] => acc
       | names_list_head::names_list_tail =>
           case all_except_option(name, names_list_head) of
                NONE => aux(names_list_tail, name, acc)
              | SOME names => aux(names_list_tail, name, names @ acc)
in
  aux(names_list, name, [])
end

fun similar_names(names_list : string list list, {first = f, middle = m, last = l})=
let
  val names = get_substitutions2(names_list, f)
  fun aux(names : string list)=
    case names of
         [] => []
       | h::t => {first = h , middle = m,last = l}::aux(t)

in
  {first = f, middle = m, last = l }::aux(names)
end

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw

exception IllegalMove

(* put your solutions for problem 2 here *)

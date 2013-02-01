(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

fun all_except_option(str, sl)=
let
  fun exist(str, sl)=
    case sl of
         [] => false
       | hd::tl => if same_string(hd,str) then true else exist(str, tl)
  fun aux(str, sl) =
    case sl of
         [] => []
       | h::t => if same_string(str,h) then aux(str,t) else h::aux(str,t)

in
  if exist(str, sl) = false then NONE
  else
    SOME(aux(str, sl))
end

fun get_substitutions1(names_list , name)=
  case names_list of
       [] => []
     | name_list_head::name_list_tail =>
         case all_except_option(name, name_list_head) of
              NONE => get_substitutions1(name_list_tail, name)
            | SOME names => names @ get_substitutions1(name_list_tail,name)

fun get_substitutions2(names_list, name)=
let
  fun aux(names_list, name, acc)=
    case names_list of
         [] => acc
       | names_list_head::names_list_tail =>
           case all_except_option(name, names_list_head) of
                NONE => aux(names_list_tail, name, acc)
              | SOME names => aux(names_list_tail, name, names @ acc)
in
  aux(names_list, name, [])
end

fun similar_names(names_list, {first = f, middle = m, last = l})=
let
  val names = get_substitutions2(names_list, f)
  fun aux(names )=
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

fun card_color c  =
  case c of
       (Diamonds, _) => Red
     | (Hearts, _) => Red
     | _ => Black

fun card_value c =
  case c of
       (_, Num num) => num
     | (_, Ace) => 11
     | _ => 10

fun remove_card (cs, c, ex) =
case cs of
     [] => raise ex
   | h::t => if h = c then c else remove_card(t, c, ex)

fun all_same_color cs =
  case cs of
       [] => true
     | head::(neck::rest) => (card_color(head) = card_color(neck)) andalso
     all_same_color (neck::rest)

fun sum_cards cs =
let fun aux(sc, acc) =
case sc of
     [] => acc
   | h::t => aux(t, h + acc)
in
  aux(cs,0)
end



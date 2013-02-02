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
   | h::t => if h = c then t else remove_card(t, c, ex)

fun all_same_color cs =
  case cs of
      head::(neck::rest) => (card_color(head) = card_color(neck)) andalso
       all_same_color (neck::rest)
    | _ => true

fun sum_cards cs =
let fun aux(sc, acc) =
case sc of
     [] => acc
   | h::t => aux(t, card_value h + acc)
in
  aux(cs,0)
end

fun score (held, goal)=
let 
  fun calc_preliminary ()=
  let val sum = sum_cards held
  in
    if sum > goal then 3*(sum - goal) else goal - sum
  end

  val prelim_score = calc_preliminary()
in
  if all_same_color held = true  then prelim_score else (prelim_score div 2)
end

fun officiate(cards, moves, goal)=
let
  fun do_move(cards, moves, held)=
    case (cards, moves, held) of
         (_, [], _) => (cards, moves, held)
       | (cards_head, moves_head::moves_tail, held)
       | moves_head::moves_tail => 
         case moves_head of 
             Draw =>
               (* do_draw *)
               case cards of 
                 top_card::cards_tail => 
                    if sum_cards (top_card::held) < goal
                    then
                      do_move(cards_tail, moves_tail, top_card::held)
                    else
                      (cards, moves_tail, held)
                (* cards is empty *)
                | [] => (cards, moves_tail, held)

           | Discard disc_card =>
               (* do_discard *)
               do_move(cards, moves_tail, remove_card(held, disc_card, IllegalMove)) 

  val (_,_,res_held) = do_move(cards, moves, [])
in
  (* score (res_held, goal) *)
  res_held
end

(* These are just two tests for problem 2; you will want more.

   Naturally these tests and your tests will use bindings defined 
   in your solution, in particular the officiate function, 
   so they will not type-check if officiate is not defined.
 *)

fun provided_test1 () = (* correct behavior: raise IllegalMove *)
    let val cards = [(Clubs,Jack),(Spades,Num(8))]
	val moves = [Draw,Discard(Hearts,Jack)]
    in
	officiate(cards,moves,42)
    end

fun provided_test2 () = (* correct behavior: return 3 *)
    let val cards = [(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)]
	val moves = [Draw,Draw,Draw,Draw,Draw]
    in
 	officiate(cards,moves,42)
    end

val res1 = provided_test1()
val res2 = provided_test2()









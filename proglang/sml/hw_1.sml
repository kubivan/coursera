
type date_t = int*int*int;

(* year month day*)
fun is_older(first : date_t, second : date_t)=
let 
  fun date_code(date : date_t) =
  let
    val year  = #1 date
    val month = #2 date
    val day   = #3 date 
  in
    year*365 + month*31 + day
  end;
in
  date_code first < date_code second
end;


(*how many dates in the list are in the given month *)
fun number_in_month( dates : date_t list, month : int)=
let
  (*returns 1 if date in month and 0 otherwise *)
  fun is_in_month(date: date_t, month : int)=
    if #2 date = month then 
      1
    else
      0;
in
  if null dates then
    0    
  else
    is_in_month(hd dates, month) + number_in_month(tl dates, month) 
end;

fun number_in_months(dates : date_t list, months : int list)=
  if null months then
    0
  else
    number_in_month(dates, hd months) + number_in_months(dates, tl months);

    
(*Write a function dates_in_month that takes a list of dates 
* and a month (i.e., an int) and returns a list holding the 
* dates from the argument list of dates that are in the month. 
* The returned list should contain dates in the order they were 
* originally given. *)
fun dates_in_month( dates : date_t list, month : int)=
let 
  (*returns true if date in month and false otherwise *)
  fun is_in_month(date: date_t, month : int)=
    if #2 date = month then 
      true
    else
      false;
in
  if null dates then
    []
  else
  if is_in_month(hd dates, month) then
    hd dates :: dates_in_month(tl dates, month)
  else 
    dates_in_month(tl dates, month)
end; 

(*Write a function dates_in_months that takes a list of dates and a list of months (i.e., an int list)
and returns a list holding the dates from the argument list of dates that are in any of the months in
the list of months. Assume the list of months has no number repeated. Hint: Use your answer to the
previous problem and SML's list-append operator (@).
*)
fun dates_in_months(dates : date_t list, months : int list)=
  if null months then
    []
  else
    dates_in_month(dates, hd months) @ dates_in_months(dates, tl months);


(*6. Write a function get_nth that takes a list of strings and an int n and returns the nth element of the
list where the head of the list is 1st. Do not worry about the case where the list has too few elements:
your function may apply hd or tl to the empty list in this case, which is okay.
*)
fun get_nth( strings : string list, n : int)=
let
  fun get_nth_rec(strings : string list, n : int, current : int) = 
    if n = current then
      hd strings
    else
      get_nth_rec(tl strings, n, current + 1)
in
  get_nth_rec(strings, n, 1)
end;


(*7. Write a function date_to_string that takes a date and returns a string of the form January 20, 2013
(for example). Use the operator ^ for concatenating strings and the library function Int.toString
for converting an int to a string. For producing the month part, do not use a bunch of conditionals.
Instead, use a list holding 12 strings and your answer to the previous problem. For consistency, put a
comma following the day and use capitalized English month names: January, February, March, April,
May, June, July, August, September, October, November, December.
*)
fun date_to_string( date : date_t)=
let
  val months = [ "January", "February", "March", "April", "May", "June", "July",
                 "August", "September", "October", "November", "December" ];
  val month  = get_nth(months, #2 date);
  val day    = Int.toString(#3 date);
  val year   = Int.toString(#1 date);
in
  month ^ " " ^ day ^ ", " ^ year
end;

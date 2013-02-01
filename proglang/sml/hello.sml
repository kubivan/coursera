
(*test commet *) 

fun listp l = 
case l of 
    [] => 0
    | x::xt => x + listp xt;


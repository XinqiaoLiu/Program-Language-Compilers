(* File: ml2.ml *)

(* Problem 1 *)
let closer_to_origin p1 p2 = match (p1,p2) with ((a1,a2),(b1,b2))->let a = a1*.a1+.a2*.a2 in let b = b1*.b1+.b2*.b2 in if a>b then 1 else
if a<b then 1 else 0;;

(* Problem 2 *)
let swap_eq p1 p2 = match (p1,p2) with ((a,b),(c,d))-> if a=d&&b=c then true else false;;

(* Problem 3 *)
let twist pp = match pp with ((a,b),(c,d))->((d,a),(c,b));;

(* Problem 4 *)
let triple_pairs x trp = raise(Failure "Function not implemented yet.")

(* Problem 5 *)
let triple_xprod trp pr = match (trp,pr) with ((a,b,c),(d,e))->(((a,d),(b,d),(c,d)),((a,e),(b,e),(c,e)));;

(*  Problem 6 *)
let two_funs fns ins = raise(Failure "Function not implemented yet.")

(*  Problem 7 *)
let triple_app (f,g,h) x = raise(Failure "Function not implemented yet.")

(*  Problem 8 *)
let same_arg_twice f x = raise(Failure "Function not implemented yet.")

(*  Problem 9 *)
let rev_app x f = raise(Failure "Function not implemented yet.")

(*  Problem 10 *)
let map_triple f (a,b,c) = raise(Failure "Function not implemented yet.")

(* Problem 11 *)
let rec ackermann m n = raise(Failure "Function not implemented yet.")

(* Problem 12 *)
let rec collatz n = raise(Failure "Function not implemented yet.")

(* Problem 13 *)
let rec delannoy (m, n) = raise(Failure "Function not implemented yet.")

(* Problem 14 *)
let rec naive_fibonacci n = raise(Failure "Function not implemented yet.")

(* Problem 15 *)
let rec sum_evens_less_eq n = raise(Failure "Function not implemented yet.")

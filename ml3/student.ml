open Common
(*open List*)

(*****************************)
(***** PROBLEMS FROM ML2 *****)
(*****************************)

(*****************************)
(****** PROBLEMS FOR MP2 *****)
(*****************************)
(***** Problem 1: Warmup (0 Points)  ******)
let consk (x, l) k = k(x::l);;
let concatk (s1, s2) k = k(s1^s2);;
let string_of_intk s k = k(string_of_int s);;
let truncatek r k = k(truncate r);;

(***** Problem 2: Basic CPS *****)
let diff_flipk p k = subk(1,p) (fun z->mulk(z,p) (fun y->mulk(2,y) k) );;

(***** Problem 3: Basic CPS *****)
let quadk (a, b, c) k = mulk(a,a) (fun y->mulk(2,y) (fun z->mulk(4,b) (fun x->addk(z,x) (fun s->addk(s,c) k))));;
(*mulk(4,b) (fun z->mulk(a,a) (fun y->mulk(2,y) (fun x->addk(x,c) k )));;*)

(***** Problem 4: Basic CPS *****)
let three_freezek (s, p) k = concatk(s,p) (fun z->concatk(z,z) (fun y->concatk(z,y) k));;

(***** Problem 5: Basic CPS *****)
let shiftk (s, q) k = float_addk(q,1.57) (fun z->float_mulk(z,z) (fun y->truncatek y (fun x->string_of_intk x (fun r1->concatk (s,r1) (fun r2->concatk (r2,s) k)))));;

(***** Problem 6a: Recursion & CPS ******)
let rec list_prod l = match l with []->1
|x::xs-> x*list_prod xs;;

(***** Problem 6b: Recursion & CPS ******)
let rec list_prodk l k = match l with []-> k 1
|x::xs -> list_prodk xs (fun z->mulk(x,z) k);;

(***** Problem 7a: Recursion & CPS *****)
let rec all_positive l = match l with []->true
|x::xs ->if x>0 then all_positive xs else false;;

(***** Problem 7b: Recursion & CPS *****)
let rec all_positivek l k = match l with []->k true
|x::xs-> gtk(x,0) (fun z-> if z then all_positivek xs k else k false);;

(***** Problem 8a: Recursion & CPS *****)
let rec even_count l = match l with []->0
|x::xs-> if x mod 2 = 0 then 1+even_count xs else even_count xs;;

(***** Problem 8b: Recursion & CPS *****)
let rec even_countk l k = match l with []-> k 0
|x::xs-> modk(x,2) (fun z-> eqk(z,0) (fun x-> if x then even_countk xs (fun y->addk(1,y) k) else even_countk xs k));;


(******** CONTINUATIONS For HIGHER-ORDER FUNCTIONS ********)


let rec find_all (p,l) = match l with []->[]
|x::xs -> if p x then x::find_all (p,xs) else find_all (p,xs);;

let rec find_allk (p,l) k = match l with []-> k []
|x::xs -> p x (fun z->if z then find_allk (p,xs) (fun y->k(x::y)) else find_allk (p,xs) k);;

let rec sum_all (p,l) = match l with []->0.0
|x::xs -> if p x then x+.sum_all (p,xs) else sum_all (p,xs);;

let rec sum_allk (p,l) k = match l with []-> k 0.0
|x::xs -> p x (fun z-> if z then sum_allk (p,xs) (fun y->float_addk(x,y) k) else sum_allk (p,xs) k);;


(********** EXTRA CREDIT **********)

(* Extra Credit, Problem 16a *)
let rec list_compose fs = match fs with []->0
|x::xs -> x (list_compose xs);;

(* Extra Credit, Problem 16b *)
let rec list_composek fsk k = match fsk with []-> k 0
|x::xs -> list_composek xs (fun z-> x z k);;

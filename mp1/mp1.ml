(* CS421 - Fall 2019
 * MP1
 *
 * Please keep in mind that there may be more than one
 * way to solve a problem.  You will want to change how a number of these start.
 *)

 (*Problem 1*)
 let rec product l =
   match l with []->1.0
   | x::xs->x*.product xs;;

 (*Problem 2*)
 let rec double_all l =
   match l with []->[]
   | x::xs -> (2.0*.x)::(double_all xs);;
 (*Problem 3*)
 let rec pair_with_all x l =
   match l with []->[]
   | a::xs -> (x,a)::(pair_with_all x xs);;

(*Problem 4*)
let rec interleave l1 l2 =
match l1 with []->l2
| x::xs -> x:: interleave l2 xs;;

(*Problem 5*)
let rec sub_list l1 l2 =
match (l1,l2) with (_,[])->true
| ([],_) -> false
| (x::xs,y::ys) -> if x=y then sub_list xs ys else sub_list xs l2;;

(*Problem 6*)
let rec even_count_fr l = match l with []->0
| x::xs -> let a = even_count_fr xs in if x mod 2 = 0 then a+1 else a;;
(*if x mod 2 = 0 then 1+even_count_fr xs else even_count_fr xs;;*)

(*Problem 7*)
let rec pair_sums l = match l with [] -> []
| (x,y)::xs -> x+y :: pair_sums xs;;

(*Problem 8*)
let rec remove_even list = match list with []->[]
|x::xs-> let a = remove_even xs in if x mod 2 = 0 then a else x::a;;
(*if x mod 2 = 0 then remove_even xs else x::remove_even xs;;*)

(*Problem 9*)
let rec sift p l = match l with []->([],[])
| x::xs -> let (l1,l2) = sift p xs in if p x = true then (x::l1,l2) else (l1,x::l2);;

(*Problem 10*)
let rec apply_even_odd l f g = match l with []->[]
| x::xs -> (f x)::apply_even_odd xs g f;;

(*Problem 11*)
let rec even_count_tr l =
  let rec tr l sum = match l with []->sum
  | x::xs -> tr xs (if x mod 2 = 0 then sum+1 else sum)in tr l 0;;

(*Problem 12*)
let rec count_element l m =
  let rec tr l m c = match l with []->c
  |x::xs -> tr xs m (if x = m then c+1 else c) in tr l m 0;;

(*Problem 13*)
let rec all_nonneg list =
  let rec tr list r = match list with []->r
  |x::xs -> if x>=0 then tr xs r else false in tr list true;;

(*Problem 14*)
let rec split_sum l f =
  let rec tr l f (s1,s2) = match l with []->(s1,s2)
  |x::xs -> tr xs f (if f x = true then (x+s1,s2) else (s1,x+s2)) in tr l f (0,0);;

(*Problem 15*)
let rec concat s list =
  let rec tr s list str = match list with []->str
  | [a] -> str^a
  | x::y::xs->tr s (y::xs) (str^x^s) in tr s list "";;

(*Problem 16*)
let even_count_fr_base = 0
let even_count_fr_rec x rec_val = if x mod 2 = 0 then (rec_val+1) else rec_val;;

(*Problem 17*)
let pair_sums_map_arg p = match p with (s1,s2)->s1+s2;;

(*Problem 18*)
let remove_even_base = []
let remove_even_rec n r = if n mod 2 = 0 then r else n::r;;

(*Problem 19*)
let even_count_tr_start = 0
let even_count_tr_step acc_val x = if x mod 2 = 0 then acc_val+1 else acc_val;;

(*Problem 20*)
let split_sum_start = (0,0)
let split_sum_step f = fun (s1,s2)->fun x->if f x then (s1+x,s2) else (s1,s2+x);;

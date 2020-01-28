(* File: ml3.ml *)

open Common

(* Problem 1 *)
let rec import_list lst = match lst with []->ConstExp NilConst
|(x1,x2)::xs->BinOpAppExp (ConsOp, BinOpAppExp (CommaOp, ConstExp (IntConst x1), ConstExp (IntConst x2)), import_list xs);;

(* Problem 2 *)
let pair_sums = let pair_sums = VarExp "pair_sums" in
let lst = VarExp "lst" in
let empty = ConstExp NilConst in
let emptylst = BinOpAppExp(EqOp,lst,empty) in
let x = VarExp "x" in
let hd_lst = MonOpAppExp(HdOp,lst) in
let fst_x = MonOpAppExp(FstOp,x) in
let snd_x = MonOpAppExp(SndOp,x) in
let tl_lst = MonOpAppExp(TlOp,lst) in
let plus = BinOpAppExp(IntPlusOp,fst_x,snd_x) in
let pair_sums_tl_lst = AppExp(pair_sums,tl_lst) in
let connect = BinOpAppExp(ConsOp,plus,pair_sums_tl_lst) in
let result = AppExp(pair_sums,import_list [(7,1);(4,2);(6,3)]) in
let let_x = LetInExp("x",hd_lst,connect) in
let if_ = IfExp(emptylst,empty,let_x) in
LetRecInExp("pair_sums","lst",if_,result);;

(* Problem 3 *)
let rec count_const_in_exp exp =  match exp with VarExp x->0
|ConstExp x->1
|MonOpAppExp (mon_op,e)->count_const_in_exp e
|BinOpAppExp (bin_op,e1,e2)->(count_const_in_exp e1) + (count_const_in_exp e2)
|IfExp (e1,e2,e3)->(count_const_in_exp e1) + (count_const_in_exp e2) + (count_const_in_exp e3)
|AppExp (e1,e2) -> (count_const_in_exp e1) + (count_const_in_exp e2)
|FunExp (string,e)-> count_const_in_exp e
|LetInExp (string,e1,e2) -> (count_const_in_exp e1) + (count_const_in_exp e2)
|LetRecInExp (string1,string2,e1,e2)->(count_const_in_exp e1) + (count_const_in_exp e2);;

(* Problem 4 *)
let rec freeVarsInExp exp = match exp with VarExp x->[x]
|ConstExp x->[]
|MonOpAppExp (op,e) -> freeVarsInExp e
|BinOpAppExp (op,e1,e2) -> (freeVarsInExp e1)@(freeVarsInExp e2)
|IfExp (e1,e2,e3) -> (freeVarsInExp e1)@(freeVarsInExp e2)@(freeVarsInExp e3)
|AppExp (e1,e2) -> (freeVarsInExp e1)@(freeVarsInExp e2)
|FunExp(str,e)-> List.filter (fun z->not(str=z)) (freeVarsInExp e)
|LetInExp (str,e1,e2) -> (freeVarsInExp e1) @ (List.filter (fun z->not(str=z)) (freeVarsInExp e2))
|LetRecInExp (str1,str2,e1,e2) -> (List.filter (fun z->not((str1=z)||(str2=z))) (freeVarsInExp e1)) @ (List.filter (fun z->not(str1=z)) (freeVarsInExp e2));;

(* Problem 5 *)
let rec cps_exp e k =  match e with VarExp x->VarCPS (k,x)
|ConstExp x-> ConstCPS (k,x)
|IfExp (e1,e2,e3) -> let v = freshFor ((freeVarsInContCPS k)@(freeVarsInExp e2) @(freeVarsInExp e3)) in
let cpse2 = cps_exp e2 k in
let cpse3 = cps_exp e3 k in
cps_exp e1 (FnContCPS (v, IfCPS (v,cpse2,cpse3)))
|AppExp (e1,e2) -> let v2 = freshFor (freeVarsInContCPS k @ freeVarsInExp e1) in
let v1 = freshFor (v2::freeVarsInContCPS k) in
let cpse1 = cps_exp e1 (FnContCPS (v1, AppCPS (k,v1,v2))) in
cps_exp e2 (FnContCPS (v2,cpse1))
|BinOpAppExp (bin_op,e1,e2) -> let v2 = freshFor (freeVarsInContCPS k @ freeVarsInExp e1) in
let v1 = freshFor (v2::freeVarsInContCPS k) in
let cpse1 = cps_exp e1 (FnContCPS (v1, BinOpAppCPS (k,bin_op,v1,v2))) in
cps_exp e2 (FnContCPS (v2,cpse1))
|MonOpAppExp (mon_op,e) -> let v = freshFor (freeVarsInContCPS k) in
cps_exp e (FnContCPS (v,MonOpAppCPS (k,mon_op,v)))
|FunExp (str,e) -> let cpse = cps_exp e (ContVarCPS Kvar) in
FunCPS (k,str,Kvar,cpse)
|LetInExp (str,e1,e2) -> let cpse2 = cps_exp e2 k in
let f = FnContCPS (str,cpse2) in
cps_exp e1 f
|LetRecInExp (str1,str2,e1,e2)->let cpse1 = cps_exp e1 (ContVarCPS Kvar) in
let cpse2 = cps_exp e2 k in
FixCPS (FnContCPS (str1,cpse2), str1, str2, Kvar, cpse1);;

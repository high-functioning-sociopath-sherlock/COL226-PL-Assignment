#directory "_build";; (* Consider this folder when looking for files *)
#load "Krivine.cmo";;
#load "Parser.cmo";;
#load "Lexer.cmo";;
open Krivine;;
open Parser;;
open Lexer;;


exception Not_implemented

let p1 =  App (Lambda (V "x", Mult (Integer 3, V "x")), Integer 4);;
  (*12*)
  
let s1 = "\\X 3*X(4)";;

let p2 = If_Then_Else
   (Cmp (Integer 7),
    App (Lambda (V "x", Plus (Integer 3, V "x")), Integer 31), 
    Integer 0);;
   (*34*)
let s2 = "if cmp 7 then \\X 3+X(31) else 0 fi";;

let p3 = If_Then_Else
    (Cmp (Integer 0),
    App (Lambda (V "x", Plus (Integer 3, V "x")), Integer 4),
        Integer 110);;
    (*110*)

let s3 = "if cmp 0 then \\X 3+X(31) else 110 fi";;

let p4 = App(Lambda(V "x", App(Lambda(V "y", And(V "x", V "y")), Bool true)), Bool false);;
(*false*)

let s4 = "\\X \\Y X/\\Y (T)(F)";;

let p5 = App(Lambda( V "x", App(Lambda(V "y", Or(V "x", V "y")), Bool true)), Bool false);;
(*true*)
let s5 = "\\X.\\Y.(X\\/Y)(T)(F)";;

let p6 = App(Lambda(V "x", Mult(V "x", App(Lambda(V "x", App(Lambda(V "y", Plus(V "x", V "y")), Integer 4)), Integer 3))), Integer 2);;
(*14*)
let s6 = "\\X.(X*\\X.\\Y.(X+Y)(4)(3))(2)"

let p7 = If_Then_Else(Cmp(App(Lambda( V "x", App(Lambda( V "y", Plus(V "x", V "y")), Integer 4)), Integer (-5))), Integer (-29), 
App(Lambda(V "x", Plus(V "x", App(Lambda(V "x", Plus(V "x", Integer 1)), Integer 7))), Integer 5));;
(*13*)
let s7 = "if cmp (\\X.(\\Y.(X+Y)(4))(~5)) then (~29) else \\X.(X+(\\X.(X+1)(7)))(5) fi"

let p8 = App(Lambda(V "x", App(Lambda(V "y", Plus(V "x", V "y")), Integer 4)), App(Lambda(V "x", Mult(V "x", Integer 2)), Integer 3));;
(*10*)
let s8 = "\\X.(\\Y.(X+Y)(4))(\\X.(X*2)(3))";;

let p9 = App(Lambda(V "x", App(Lambda(V "y", Mult(V "x", V "y")), V "x")), Integer 4);;
(*16*)
let s9 = "\\X.(\\Y.(X*Y)(X))(4)";;

let p10 = App(Lambda(V "x", Plus(V "x", App(Lambda(V "x", Mult(V "x", Integer 2)), App(Lambda(V "x", Plus(V "x", Integer 4)), Integer 3)))), Integer 20);;
(*34*)
let s10 = "\\X.(X+\\X.(X*2)(\\X.(X+4)(3)))(20)";;

let p11 = App(Lambda(V "x", App(Lambda(V "y", And(V "x", V "y")), V "x")), Bool true);;
(*true*)
let s11 = "\\X.(\\Y.(X/\\Y)(X))(T)";;

let p12 = If_Then_Else(Cmp(App(Lambda(V "x", Mult(V "x", Integer 2)), Integer 4)), App(Lambda(V "x", App(Lambda(V "y", Or(V "x", V "y")), V "x")), Bool false), Bool true);;
(*false*)
let s12 = "if cmp (\\X.(X*2)(4)) then \\X.(\\Y.(X\\/Y)(X))(F) else T fi";;

let p13 = App(Lambda(V "x", And(V "x", App(Lambda(V "x", And(V "x", Bool true)), App(Lambda(V "x", And(V "x", Bool true)), Bool true)))), Bool true);;
(*true*)
let s13 = "\\X.(X/\\(\\X.(X/\\T)(\\X.(X/\\T)(T))))(T)";;

let p14 = App(Lambda(V "x", And(V "x", App(Lambda(V "x", And(V "x", Bool true)), App(Lambda(V "x", And(V "x", Bool true)), Bool true)))), Bool false);;
(*false*)
let s14 = "\\X.(X/\\(\\X.(X/\\T)(\\X.(X/\\T)(T))))(F)";;

let p15 = If_Then_Else(Cmp(App(Lambda(V "x", Mult(V "x", App(Lambda(V "y", V "y"), V "x"))), Integer 1)), App(Lambda(V "x", Plus(V "x", App(Lambda(V "x", Plus(V "x", Integer 1)), Integer 3))), Integer 5), Integer (-1));;
(*9*)
let s15 = "if cmp (\\X.(X*\\Y.(Y)(X))(1)) then \\X.(X+\\X.(X+1)(3))(5) else (~1) fi"

let exp_parser s = Parser.exp_parser Lexer.read (Lexing.from_string s) ;;
(*Your code ends*)

(*Your code will go here*)
(*For thise who have implemented lexer parser, modify the testcases in your grammar and you will have to get those tet_cases at the time of the demo*)

let eval_secd inp = let comp = Krivine.compile (exp_parser inp) in
                    Krivine.secd [] [] comp []

let eval_krivine inp = let clos = Clos((exp_parser inp), []) in
                       Krivine.krivine clos []

let eval_krivine1 inp = Krivine.krivine (Clos(inp, [])) [];;
let eval_secd1 inp = Krivine.secd [] [] (Krivine.compile inp) [];;


let check_secd n inp out =
  print_string("T" ^ string_of_int(n) ^ " : ");
  try if (inp = out) 
    then print_string("Passed ") 
    else print_string("Failed ");
  with e -> print_endline("Failed : Wrong exception raised : " ^ (Printexc.to_string e))
;;

let check_krivine n inp out =
  print_string("T" ^ string_of_int(n) ^ " : ");
  try if (inp = out) 
    then print_string("Passed ") 
    else print_string("Failed ");
  with e -> print_endline("Failed : Wrong exception raised : " ^ (Printexc.to_string e))
;;


let print_heading a = print_endline("\n" ^ a ^ " :");;

(*SECD*)
print_heading "SECD test cases\n";;

(* check_secd 1 (eval_secd1 p1) (I 12);;
check_secd 2 (eval_secd1 p2) (I 34);;
check_secd 3 (eval_secd1 p3) (I 110);;
check_secd 4 (eval_secd1 p4) (B false);;
check_secd 5 (eval_secd1 p5) (B true);;
check_secd 6 (eval_secd1 p6) (I 14);;
check_secd 7 (eval_secd1 p7) (I 13);;
check_secd 8 (eval_secd1 p8) (I 10);;
check_secd 9 (eval_secd1 p9) (I 16);;
check_secd 10 (eval_secd1 p10) (I 34);;
check_secd 11 (eval_secd1 p11) (B true);;
check_secd 12 (eval_secd1 p12) (B false);;
check_secd 13 (eval_secd1 p13) (B true);;
check_secd 14 (eval_secd1 p14) (B false);;
check_secd 15 (eval_secd1 p15) (I 9);; 

print_heading "Krivine test cases";;

check_krivine 1 (eval_krivine1 p1) (Clos (Integer 12, []));;
check_krivine 2 (eval_krivine1 p2) (Clos (Integer 34, []));;
check_krivine 3 (eval_krivine1 p3) (Clos (Integer 110, []));; 
check_krivine 4 (eval_krivine1 p4) (Clos (Bool false, []));; 
check_krivine 5 (eval_krivine1 p5) (Clos (Bool true, []));; 
check_krivine 6 (eval_krivine1 p6) (Clos (Integer 14, []));; 
check_krivine 7 (eval_krivine1 p7) (Clos (Integer 13, []));; 
check_krivine 8 (eval_krivine1 p8) (Clos (Integer 10, []));; 
check_krivine 9 (eval_krivine1 p9) (Clos (Integer 16, []));; 
check_krivine 10 (eval_krivine1 p10) (Clos (Integer 34, []));; 
check_krivine 11 (eval_krivine1 p11) (Clos (Bool true, []));; 
check_krivine 12 (eval_krivine1 p12) (Clos (Bool false, []));;
check_krivine 13 (eval_krivine1 p13) (Clos (Bool true, []));; 
check_krivine 14 (eval_krivine1 p14) (Clos (Bool false, []));; 
check_krivine 15 (eval_krivine1 p15) (Clos (Integer 9, []));;   *)


check_secd 1 (eval_secd s1) (I 12);;
check_secd 2 (eval_secd s2) (I 34);;
check_secd 3 (eval_secd s3) (I 110);;
check_secd 4 (eval_secd s4) (B false);;
check_secd 5 (eval_secd s5) (B true);;
check_secd 6 (eval_secd s6) (I 14);;
check_secd 7 (eval_secd s7) (I 13);;
check_secd 8 (eval_secd s8) (I 10);;
check_secd 9 (eval_secd s9) (I 16);;
check_secd 10 (eval_secd s10) (I 34);;
check_secd 11 (eval_secd s11) (B true);;
check_secd 12 (eval_secd s12) (B false);;
check_secd 13 (eval_secd s13) (B true);;
check_secd 14 (eval_secd s14) (B false);;
check_secd 15 (eval_secd s15) (I 9);; 

print_heading "Krivine test cases";;

check_krivine 1 (eval_krivine s1) (Clos (Integer 12, []));;
check_krivine 2 (eval_krivine s2) (Clos (Integer 34, []));;
check_krivine 3 (eval_krivine s3) (Clos (Integer 110, []));; 
check_krivine 4 (eval_krivine s4) (Clos (Bool false, []));; 
check_krivine 5 (eval_krivine s5) (Clos (Bool true, []));; 
check_krivine 6 (eval_krivine s6) (Clos (Integer 14, []));; 
check_krivine 7 (eval_krivine s7) (Clos (Integer 13, []));; 
check_krivine 8 (eval_krivine s8) (Clos (Integer 10, []));; 
check_krivine 9 (eval_krivine s9) (Clos (Integer 16, []));; 
check_krivine 10 (eval_krivine s10) (Clos (Integer 34, []));; 
check_krivine 11 (eval_krivine s11) (Clos (Bool true, []));; 
check_krivine 12 (eval_krivine s12) (Clos (Bool false, []));;
check_krivine 13 (eval_krivine s13) (Clos (Bool true, []));; 
check_krivine 14 (eval_krivine s14) (Clos (Bool false, []));; 
check_krivine 15 (eval_krivine s15) (Clos (Integer 9, []));;  
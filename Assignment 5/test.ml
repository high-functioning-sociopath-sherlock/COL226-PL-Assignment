# directory "_build";;
# load "Krivine.cmo";;
# load "Parser.cmo";;
# load "Lexer.cmo";;
open Krivine;;
open Lexer;;
open Parser;;

exception Foo of string

let exp_parser s = Parser.exp_parser Lexer.read (Lexing.from_string s) ;;
(*let _ = exp_parser "\\X:tint.3*X(5)";;*)

let kri s = let clos = Clos((exp_parser s), []) in
            Krivine.krivine clos []

let secd1 s = let comp = Krivine.compile (exp_parser s) in
             Krivine.secd [] [] comp []

let _ = kri "\\X 3*X(5)";;
let _ = secd1 "\\X 3*X(5)";;


(match pcallee with 
                            | Call(n, a, b) -> let frame = Frame (n, a::[b], []) in 
                                               demo (frame::sframe) 
                            | _ -> raise(Foo "Error"))
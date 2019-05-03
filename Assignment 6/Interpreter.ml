# directory "_build";;
# load "StackFrame.cmo";;
# load "Parser.cmo";;
# load "Lexer.cmo";;
open StackFrame;;
open Lexer;;
open Parser;;

(* actual interpreter
let rec intr s = let () = print_string "Press 1 to call an stack\n" in
                 let () = print_string "Press 2 to set the value of variables\n" in
                 let () = print_string "Press 3 to return from the function\n" in
                 let i = read_int () in
                 if(i = 1)then 
                  let () = print_string "Functions that can be called are\n" in
                  (match s with 
                    | Func(sname, spara, slocal, svar, sfunction) -> (sprint_list sfunction)
                    | _ -> raise(Foo "must be of type Func"))
                  let st = read_line () in
                  if(searc_string_list st ls) then
                    intr st 
                  else
                    let () = print_string "Wrong input exiting" in
                    exit 0;
                 else 


(* Sample interpreter  *)
#require "str";;
let rec inter s =
                let () = print_string "1) call an stack \n" in
                let () = print_string "2) set the value of the variable \n" in
                let () = print_string "3) exit \n" in
                let i = read_int () in
                if(i = 1)then
                  let () = print_string "call an function \n" in
                  let () = print_string "Enter the name of the function \n" in
                  let name = read_line () in
                  let () = print_string "Enter the parameter values for the function \n" in
                  let ls = read_line () |> Str.split ( Str.regexp ", ") |> List.map int_of_string in
                  (name, ls)
                else 
                  if(i = 2)then
                  ("exit", [])
                  else
                  let () = print_string "Exiting the interpreter" in
                  ("exit", []);; *)
let exp_parser s = Parser.exp_parser Lexer.read (Lexing.from_string s) ;;


let rec demo  sframe =  let () = print_string "1) call an stack \n" in
                        let () = print_string "2) set the value of the variable \n" in
                        let () = print_string "3) exit \n" in
                        let i = read_int () in
                        if(i = 1)then
                          let callee = read_line() in
                          let pcallee = exp_parser callee in
                          (match pcallee with 
                              | Call(n, S(a), S(b)) -> let frame = Frame (n, Tup(a,0)::[Tup(b,0)], []) in 
                                                 demo (frame::sframe) 
                              | _ -> raise(Foo "Error"))                           
                        else
                          if( i=2 )then
                            let () = print_string "Enter the name of the variable" in
                            let n = read_line () in
                            let () = print_string "Enter the value" in
                            let val = read_int () in 
                            
                          else



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


(* Interpreter for the assignment 6 *)
let rec demo  sframe =  let () = print_string "1) call an stack \n" in
                        let () = print_string "2) set the value of the variable \n" in
                        let () = print_string "3) Return \n" in
                        let () = print_string "4) Show static link \n" in
                        let () = print_string "5) Show the variables value (Default value is 0) \n" in
                        let i = read_int () in
                        if(i = 1)then (* calling another function *)
                          let () = print_string "Enter the command eg. P(3,4) \n" in
                          let callee = read_line() in
                          let pcallee = exp_parser callee in
                          (match sframe with 
                              | [] -> (match pcallee with 
                                        | Cal (n) -> demo (add sframe n 0 0) 
                                        | _ -> raise(Foo "Can call only main procedure") )
                              | Frame(name, plist, llist, slink)::t ->  let slinkframes = getFrames slink sframe in 
                                                                        let varlist = getvariables slinkframes in
                                                                        (match pcallee with 
                                                                            | Call(n, S(a), S(b)) -> let a1 = get_val_from_tuple_list  a varlist in
                                                                                                    let a2 = get_val_from_tuple_list b varlist in
                                                                                                    demo (add sframe n a1 a2)
                                                                            | Call(n, S(a), I(b)) -> let a1 = get_val_from_tuple_list a varlist in
                                                                                                    demo (add sframe n a1 b)
                                                                            | Call(n, I(a), S(b)) -> let a2 = get_val_from_tuple_list b varlist in
                                                                                                    demo (add sframe n a a2)
                                                                            | Call(n, I(a), I(b)) -> demo (add sframe n a b) 
                                                                            | Cal (n) -> demo (add sframe n 0 0)
                                                                            | _ -> raise (Foo "Error ") )  )                       
                        else
                           if( i=2 )then  (* setting the variable of local variable  *)
                            (match sframe with 
                                | [] -> raise(Foo "Currently no frame")
                                | Frame(name, plist, llist, slink)::t ->   let () = print_string "Enter the name of the variable \n" in
                                                                                    let n = read_line () in
                                                                                    let () = print_string "Enter an integer value \n" in
                                                                                    let value = read_int () in
                                                                                    let ans = change n llist value in
                                                                                    demo sframe )
                          else
                            if( i = 3) then (* returning from a given function *)
                              let () = print_string "Returning the procedure \n" in
                              (match sframe with
                                | [] -> raise (Foo "Cannot return from the empty Frame")
                                | h::t -> demo t)
                            else
                              if(i = 4)then (* displaying the content of the static link *)
                                (match sframe with 
                                    | [] -> raise (Foo "currently no frame")
                                    | Frame(name, plist, llist, slink)::t -> sprint_list slink)
                              else
                                if( i = 5) then
                                  (match sframe with
                                    | [] -> raise (Foo "Error no frame present")
                                    | Frame(name, plist, llist, slink)::t -> let slinkframes = getFrames slink sframe in
                                                                             let varlist = getvariables slinkframes in
                                                                             tprint_list varlist )
                                else
                                  raise (Foo "Didn't entered the correct value")
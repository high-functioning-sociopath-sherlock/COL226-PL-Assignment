exception Foo of string

type funcall = S of string | I of int | Call of (string*funcall*funcall) | Cal of string | Assign of string*int

(* data structure to store the variable name with its integer value *)
type tuple = Tup of (string*int)
type varname = Var of string list 

(* function |-> function name, parameter passed, local variables, variables that can be accessed, function that can be called *)
type func = Func of (string*string list* string list* string list* string list)

(* Defining every function and variables that can be accessed from a given function *)
let main = Func("Main", [], "a"::"b"::["c"], "a"::"b"::["c"], "P"::"Q"::["Main"])
let p = Func("P", "x"::["y"], "z"::["a"], "x"::"y"::"z"::"a"::"b"::["c"], "Q"::"R"::"P"::["S"])
let q = Func("Q", "z"::["w"], "x"::["b"], "a"::"b"::"c"::"z"::"w"::["x"], "P"::"T"::"Q"::["U"])
let r = Func("R", "w"::["i"], "j"::["b"], "a"::"b"::"c"::"x"::"y"::"z"::"w"::"i"::"j"::["b"], "S"::"P"::"Q"::"v"::["R"])
let s = Func("S", "c"::["k"], "m"::["n"], "m"::"n"::"c"::"k"::"z"::"a"::"x"::"y"::"b"::["c"], "R"::"P"::"Q"::["S"])
let v = Func("V", "m"::["n"], ["c"], "c"::"m"::"n"::"j"::"b"::"w"::"i"::"x"::"y"::"z"::["a"], "R"::"S"::"P"::"Q"::["V"])
let t = Func("T", "a"::["y"], "i"::["j"], "i"::"j"::"a"::"y"::"x"::"b"::"z"::"w"::"a"::["c"], "U"::"Q"::"P"::"W"::["T"])
let w = Func("W", "m"::["p"], "j"::["h"], "j"::"h"::"m"::"p"::"i"::"j"::"a"::"y"::"x"::"b"::"z"::"w"::["c"], "T"::"U"::"Q"::"P"::["W"])
let u = Func("U", "c"::["z"], "p"::["g"], "p"::"g"::"c"::"z"::"x"::"b"::"z"::"w"::"a"::["c"], "T"::"Q"::"P"::["U"])

let pvar = "x"::"y"::"z"::"a"::"b"::["c"];;

(* stack frame data structure *)
type stackframe = Frame of ((string)*(tuple list)* (tuple list)*(string list))

(* to store the stack name that have been called till now *)
type calledstack = Stacks of string list

(* Function to print the content of tuple list *)
let rec tprint_list = function 
                      [] -> ()
                      | (Tup(e, v))::l -> print_string e ; print_string " "; print_int v; print_string ", "; tprint_list l

(* Function to print the content of string list *)
let rec sprint_list = function 
                      [] -> ()
                      | [h] -> print_string h
                      | e::l -> print_string e ; print_string " " ; sprint_list l
                      
(* Returns true if the element is present in the list *)
let rec search_string_list s ls = match ls with 
                                    | [] -> false
                                    | h::t -> if(h = s) then true else (search_string_list s t) 
 
(* function to add an frame to the stack already present *)
let add sframe name a1 a2 = match name with
                              | "main" -> let frame = Frame ("main", [], Tup("a", 0)::Tup("b", 0)::[Tup("c", 0)], ["main"] ) in
                                          frame::sframe
                              | "P" -> let frame = Frame ("P", Tup("x", a1)::[Tup("y", a2)], Tup("z", 0)::[Tup("a", 0)], "P"::["main"] ) in
                                        frame::sframe
                              | "Q" -> let frame = Frame ("Q", Tup("z", a1)::[Tup("w", a2)], Tup("x", 0)::[Tup("b", 0)], "Q"::["main"] ) in
                                       frame::sframe
                              | "R" -> let frame = Frame ("R", Tup("w", a1)::[Tup("i", a2)], Tup("j", 0)::[Tup("b", 0)], "R"::"P"::["main"] ) in
                                       frame::sframe
                              | "S" -> let frame = Frame ("S", Tup("c", a1)::[Tup("k", a2)], Tup("m", 0)::[Tup("n", 0)], "S"::"P"::["main"] ) in
                                       frame::sframe
                              | "V" -> let frame = Frame ("V", Tup("m", a1)::[Tup("n", a2)], [Tup("c", 0)], "V"::"R"::"P"::["main"] ) in
                                       frame::sframe
                              | "T" -> let frame = Frame ("T", Tup("a", a1)::[Tup("y", a2)], Tup("i", 0)::[Tup("j", 0)], "T"::"Q"::["main"] ) in
                                       frame::sframe
                              | "W" -> let frame = Frame ("W", Tup("m", a1)::[Tup("p", a2)], Tup("j", 0)::[Tup("h", 0)], "W"::"T"::"Q"::["main"] ) in
                                       frame::sframe
                              | "U" -> let frame = Frame ("U", Tup("c", a1)::[Tup("z", a2)], Tup("p", 0)::[Tup("g", 0)], "U"::"Q"::["main"] ) in
                                       frame::sframe
                              | _ -> raise (Foo "Entered name of the frame function is wrong")


(* helper function that search an tuple list and replace the current value with given value else return error. Can be used to give value to the parameter value also to the local variable*)
let rec change1 n ls a st = match ls with 
                          | [] -> raise (Foo "No element matched")
                          | h::tl -> (match h with 
                                        | Tup(s , va) -> if(s = n) then (Tup(s, a)::tl)@st else (change1 n tl a (h::st)) ) 

let change n ls a = change1 n ls a []
                                        
(* changing the variable of given name  *)
let uframe sframe n a = match sframe with 
                            | Frame (name , plist, llist, slink) -> change n llist a

(* Function that can be called from a given function *)
let retcallfun n = match n with 
                    | "main" -> "P"::"Q"::["Main"]
                    | "P" -> "Q"::"R"::"P"::["S"]
                    | "Q" -> "P"::"T"::"Q"::["U"]
                    | "R" -> "S"::"P"::"Q"::"v"::["R"]
                    | "S" -> "R"::"P"::"Q"::["S"]
                    | "V" -> "R"::"S"::"P"::"Q"::["V"]
                    | "T" -> "U"::"Q"::"P"::"W"::["T"]
                    | "W" -> "T"::"U"::"Q"::"P"::["W"]
                    | "U" -> "T"::"Q"::"P"::["U"]
                    | _ -> raise (Foo "Entered Function name is not correct")


(* Function that returns the variables that can be accessed from a given function *)
let retcallvar n = match n with 
                    | "main" -> "a"::"b"::["c"]
                    | "P" -> "x"::"y"::"z"::"a"::"b"::["c"]
                    | "Q" -> "a"::"b"::"c"::"z"::"w"::["x"]
                    | "R" -> "a"::"b"::"c"::"x"::"y"::"z"::"w"::"i"::"j"::["b"]
                    | "S" -> "m"::"n"::"c"::"k"::"z"::"a"::"x"::"y"::"b"::["c"]
                    | "V" -> "c"::"m"::"n"::"j"::"b"::"w"::"i"::"x"::"y"::"z"::["a"]
                    | "T" -> "i"::"j"::"a"::"y"::"x"::"b"::"z"::"w"::"a"::["c"]
                    | "W" -> "j"::"h"::"m"::"p"::"i"::"j"::"a"::"y"::"x"::"b"::"z"::"w"::["c"]
                    | "U" -> "p"::"g"::"c"::"z"::"x"::"b"::"z"::"w"::"a"::["c"]
                    | _ -> raise(Foo "Entered Function name is not correct")

let rec getval ls s = match ls with 
                        | [] -> raise (Foo "Error Variable not present")
                        | (Tup(n, vl))::tl -> if(n = s) then vl else (getval tl s)


(* functions to get the list of frames given any list of name of the function and stackframe list  *)
let rec getFrame s framelist = match framelist with 
                                | [] -> raise(Foo "Given call stack is wrong")
                                | Frame(name, plist, llist, slist)::t -> if( name = s ) then Frame(name, plist, llist, slist) else getFrame s t

let rec getFramelist slist framelist anslist= match slist with
                                                | [] -> anslist
                                                | h::t -> getFramelist t framelist ((getFrame h framelist)::anslist) 


let getFrames slist framelist = getFramelist slist framelist []

(* Returns true if the element is present in the tuple list *)
let rec search_tuple_list s ls = match ls with 
                                    | [] -> false
                                    | (Tup(name, value))::t -> if(name = s) then true else (search_tuple_list s t) 
 
(* Returns true if the element is present in the tuple list *)
let rec get_val_from_tuple_list s ls = match ls with 
                                    | [] -> raise(Foo "Variable not present in the list")
                                    | (Tup(name, value))::t -> if(name = s) then value else (get_val_from_tuple_list s t) 
 
(* function to append list such that no common element present in list2 is not added again in the final anslist *)
let rec app list1 list2 = match list1 with
                            | [] -> list2
                            | (Tup(name, value))::t ->  if(search_tuple_list name list2)then (app t list2) else app t (Tup(name, value)::list2)

(* function to get the list of all variables parameters removing the second variable list  *)
let rec getvarlist framelist anslist = match framelist with
                                        | [] -> anslist
                                        | Frame(name, plist, llist, slink)::t -> getvarlist t (app anslist (plist@llist))
                              
let getvariables framelist = getvarlist framelist []

(* gives true if the function can be called else false *)
let checkfuncall s fname = search_string_list s (retcallfun fname)

(* To reverse the list  *)
let rec revlis ilist olist = match ilist with
                              | [] -> olist
                              | h::t ->  revlis t (h::olist)

let revlist ilist = revlis ilist []


let rec reduceframestack name sframes aframes= match sframes with
                                                | [] -> ([], aframes)
                                                | Frame(s, plist, llist, slink)::t -> if(name = s) then (sframes, aframes) else reduceframestack name t (Frame(s, plist, llist, slink)::aframes) 

let rframestack name sframes = reduceframestack name sframes []


let rec framename name sframes slink = match sframes with 
                                        | [] -> print_string "in framename";raise(Foo "Error No variable found")
                                        | Frame(s, plist, llist, slinkk)::t -> if((search_string_list s slink)&&((search_tuple_list name plist)||(search_tuple_list name llist))) then s 
                                                                           else framename name t slink 

let changevariable slink sframe name valu = let fname = framename name sframe slink in
                                            let frametuple = rframestack fname  sframe in
                                            (match frametuple with 
                                                | (sframes , aframes ) -> (match sframes with 
                                                                              | [] -> print_string "in changevariable";raise (Foo "Error stack frame is empty")
                                                                              | Frame(s, plist, llist, slink)::t -> if(search_tuple_list name plist) then ( (revlist aframes)@(Frame(s, (change name plist valu), llist, slink)::t) )
                                                                                                                      else (print_string s ; (revlist aframes)@(Frame(s, plist, (change name llist valu), slink)::t) ) ) )

let frame1 = Frame ("main", [], Tup("a", 0)::Tup("b", 0)::[Tup("c", 0)], ["main"] )

let frame2 = Frame ("Q", Tup("z", 1)::[Tup("w", 2)], Tup("x", 0)::[Tup("b", 0)], "Q"::["main"] )

let frame3 = Frame ("P", Tup("x", 11)::[Tup("y", 22)], Tup("z", 0)::[Tup("a", 0)], "P"::["main"] )

let rec print_stack sframe =  match sframe with 
                                  | [] -> print_string ""
                                  | [Frame(name, plist, llist, slink)] -> print_string name
                                  | Frame(name, plist, llist, slink)::t -> print_string name ; print_string ", "; print_stack t 
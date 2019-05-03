exception Foo of string

type funcall = S of string | I of int | Call of (string*funcall*funcall)

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

(* stack frame data structure *)
type stackframe = Frame of (string*tuple list* tuple list)

(* to store the stack name that have been called till now *)
type calledstack = Stacks of string list

let rec sprint_list = function 
                      [] -> ()
                      | e::l -> print_string e ; print_string " " ; sprint_list l

let rec search_string_list s ls = match ls with 
                                    | [] -> false
                                    | h::t -> if(h = s) then true else (search_string_list s t) 
 
(* function to add an frame to the stack already present *)
let add sframe name a1 a2 = match name with
                              | "main" -> let frame = Frame ("main", [], Tup("a", 0)::Tup("b", 0)::[Tup("c", 0)] ) in
                                          frame::sframe
                              | "P" -> let frame = Frame ("P", Tup("x", a1)::[Tup("y", a2)], Tup("z", 0)::[Tup("a", 0)] ) in
                                        frame::sframe
                              | "Q" -> let frame = Frame ("Q", Tup("z", a1)::[Tup("w", a2)], Tup("x", 0)::[Tup("b", 0)] ) in
                                       frame::sframe
                              | "R" -> let frame = Frame ("R", Tup("w", a1)::[Tup("i", a2)], Tup("j", 0)::[Tup("b", 0)] ) in
                                       frame::sframe
                              | "S" -> let frame = Frame ("S", Tup("c", a1)::[Tup("k", a2)], Tup("m", 0)::[Tup("n", 0)] ) in
                                       frame::sframe
                              | "V" -> let frame = Frame ("V", Tup("m", a1)::[Tup("n", a2)], [Tup("c", 0)] ) in
                                       frame::sframe
                              | "T" -> let frame = Frame ("T", Tup("a", a1)::[Tup("y", a2)], Tup("i", 0)::[Tup("j", 0)] ) in
                                       frame::sframe
                              | "W" -> let frame = Frame ("W", Tup("m", a1)::[Tup("p", a2)], Tup("j", 0)::[Tup("h", 0)] ) in
                                       frame::sframe
                              | "U" -> let frame = Frame ("U", Tup("c", a1)::[Tup("z", a2)], Tup("p", 0)::[Tup("g", 0)] ) in
                                       frame::sframe


let uframe sframe n a = match sframe with 
                            | Frame (name , plist, llist) -> let rec change n ls = (match ls with 
                                                                                    | Tup(s, val)::tl -> if(s = n) then Tup(s, a)::tl else (change n tl)
                                                                                    | [] -> []) 
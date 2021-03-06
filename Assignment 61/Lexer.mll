{
  open Parser
	exception Foo of string
}

let sletter = ['a'-'z']
let lletter = ['A'-'Z']
let letter = (sletter | lletter)
let identifier = "main" | "P" | "Q" | "R" | "S" | "V" | "T" | "U" | "W"
let lett = "a" | "b" | "c" | "x" | "y" | "z" | "w" | "m" | "n" | "i" | "j" | "k" |"h" | "p" | "g"

let digit = ['0'-'9']
let digits = digit+
let ndigit = ['1'-'9']
let integers = ((ndigit+)(digit*) | '0')

rule read = parse
| eof                 { EOF }
| ' '                 { read lexbuf}

| '('                 { LP }
| ')'                 { RP }
| identifier as r     { ID r }
| lett as par         { PARAMETERS (String.make 1 par )}
| ','                 { COMMA }
| "="                 { EQUAL }

| integers as i       { INT (int_of_string i)}

| _                   { raise(Foo "Bad Input")}
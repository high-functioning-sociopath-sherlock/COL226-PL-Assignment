{
  open Parser
	exception Foo of string
}

let sletter = ['a'-'z']
let lletter = ['A'-'Z']
let letter = (sletter | lletter)
let identifier = (lletter+)(letter)*
let lett = (sletter+)(letter)*

let digit = ['0'-'9']
let digits = digit+
let ndigit = ['1'-'9']
let integers = ((ndigit+)(digit*) | '0')

rule read = parse
| eof                 { EOF }

| '('                 { LP }
| ')'                 { RP }
| identifier as r     { ID r }
| lett as rr          { PARAMETERS rr}
| ','                 { COMMA }

| integers as i       { INT (int_of_string i)}

| ' '                 { read lexbuf}

| _                   { raise(Foo "Bad Input")}

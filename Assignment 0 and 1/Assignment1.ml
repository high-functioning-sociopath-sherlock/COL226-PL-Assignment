open signature

exception Error of string;;


type  exptree =  N of int | Plus of exptree *  exptree 
                          | Minus of exptree *  exptree 
                          | Mult of exptree *  exptree 
                          | Div of exptree *  exptree 
                          | Rem of exptree *  exptree 
                          | Neg of  exptree 
                          | Abs of  exptree ;;

type opcode = CONST of int | PLUS | TIMES | MINUS | DIV | REM | ABS | UNARYMINUS ;;


(*This function calculates the value of the expression tree*)
let rec eval t = match t with 
          | N(a)-> a
          |Plus(t1, t2) -> (eval t1)+(eval t2)
          |Minus(t1, t2) -> (eval t1)-(eval t2)
          |Mult(t1, t2) -> (eval t1)*(eval t2)
          |Div(t1, t2) -> (eval t1)/(eval t2)
          |Rem(t1, t2) -> (eval t1) mod (eval t2)
          |Neg(t1) -> -1*(eval t1)
          |Abs(t1) -> let res = eval t1 in
                      if(res<0) then -1*res
                      else res;;


(*This function gives the postfix expression for corresponding exptree*)
let rec compile t  = match t with 
             N(a) -> CONST(a)::[]
            |Plus(t1, t2) -> (compiler t1) @ (compiler t2)@ [PLUS]
            |Minus(t1, t2) -> (compiler t1) @ (compiler t2)@ [MINUS]
            |Mult(t1, t2) -> (compiler t1) @ (compiler t2)@ [TIMES]
            |Div(t1, t2) -> (compiler t1) @ (compiler t2)@ [DIV]
            |Rem(t1, t2) -> (compiler t1) @ (compiler t2)@ [REM]
            |Neg(t1) -> (compiler t1) @ [UNARYMINUS]
            |Abs(t1) -> (compiler t1) @ [ABS];;


(*This function calculates the value of the given exptree*)
let rec stackmc st op = match op, st with 
                |[], _ -> st
                | CONST(a)::tl, _ -> stackmc (a::st) tl
                |PLUS::tl, x1::x2::t -> stackmc ((x1+x2)::t) tl
                |PLUS::tl, _ -> raise(Error "Stack is empty and operand cann't be fetched")

                |MINUS::tl, x1::x2::t -> stackmc ((x1-x2)::t) tl
                |MINUS::tl, _ -> raise(Error "Stack is empty and operand cann't be fetched")
                
                |TIMES::tl, x1::x2::t -> stackmc ((x1*x2)::t) tl
                |TIMES::tl, _ -> raise(Error "Stack is empty and operand cann't be fetched")
                
                |DIV::tl, x1::x2::t -> stackmc ((x1/x2)::t) tl
                |DIV::tl, _ -> raise(Error "Stack is empty and operand cann't be fetched")
                
                |REM::tl, x1::x2::t ->stackmc ((x1 mod x2)::t) tl
                |REM::tl, _ -> raise(Error "Stack is empty and operand cann't be fetched")
                
                |UNARYMINUS::tl, x1::t -> stackmc ((-1*x1)::t) tl
                |UNARYMINUS::tl, _ -> raise(Error "Stack is empty and operand cann't be fetched")6
                
                |ABS::tl, x1::t -> stackmc ((abs x1)::t) tl
                |ABS::tl, _ -> raise(Error "Stack is empty and operand cann't be fetched");;
                         
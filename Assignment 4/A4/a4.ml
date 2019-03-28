open A1
exception Not_implemented
exception Foo of string 

(*Function for checking Var(a) type for a has the give type t or not*)
let rec checktype g s t = match g with
                         | [] -> false
                         
                         | h::l -> (match h with 
                                   |(a, e1) -> let checkstring = (a = s) in
                                               let checktype = ( e1 = t)
                                               if(checkstring&&checktype) then true else checktype l s t
                                   | _ -> raise(Foo "Not of correct type") )
                        
(*Funtion give two list one of type and other exptree gives the checks all the corresponding matches 
and return true if all match are equal else it returns false*)
                                   

(* hastype : ((string * exptype) list) -> exptree -> exptype -> bool *)
let rec hastype g e t = match e with 
                       | Var(a) -> (checktype g a t)
                       | N(a) -> if(t = Tint)then true else false
                       | B(a) -> if(t = Tbool)then true else false

                       | Abs(a) -> hastype g a Tint
                       | Negative(a) -> hastype g a Tint
                       
                       | Not(a) -> hastype g a Tbool

                       | Add(a1, a2) -> let typea1 = (hastype g a1 Tint) in
                                        let typea2 = (hastype g a2 Tint) in
                                        (typea1&&typea2)
                       | Sub(a1, a2) -> let typea1 = (hastype g a1 Tint) in
                                        let typea2 = (hastype g a2 Tint) in
                                        (typea1&&typea2)
                       | Mult(a1, a2) -> let typea1 = (hastype g a1 Tint) in
                                         let typea2 = (hastype g a2 Tint) in
                                         (typea1&&typea2)                                        
                       | Div(a1, a2) -> let typea1 = (hastype g a1 Tint) in
                                        let typea2 = (hastype g a2 Tint) in
                                        (typea1&&typea2)
                       | Rem(a1, a2) -> let typea1 = (hastype g a1 Tint) in
                                        let typea2 = (hastype g a2 Tint) in
                                        (typea1&&typea2)

                       | Conjunction(a1, a2) -> let typea1 = (hastype g a1 Tbool) in
                                                let typea2 = (hastype g a2 Tbool) in
                                                (typea1&&typea2)                                        
                       | Disjunction(a1, a2) -> let typea1 = (hastype g a1 Tbool) in
                                                let typea2 = (hastype g a2 Tbool) in
                                                (typea1&&typea2)

                       | Equals(a1, a2) -> let typea1 = (hastype g a1 Tint) in
                                           let typea2 = (hastype g a2 Tint) in
                                           (typea1&&typea2)                                                
                       | GreaterTE(a1, a2) -> let typea1 = (hastype g a1 Tint) in
                                              let typea2 = (hastype g a2 Tint) in
                                              (typea1&&typea2)
                       | LessTE(a1, a2) -> let typea1 = (hastype g a1 Tint) in
                                           let typea2 = (hastype g a2 Tint) in
                                           (typea1&&typea2)
                       | GreaterT(a1, a2) -> let typea1 = (hastype g a1 Tint) in
                                             let typea2 = (hastype g a2 Tint) in
                                             (typea1&&typea2)
                       | LessT(a1, a2) -> let typea1 = (hastype g a1 Tint) in
                                          let typea2 = (hastype g a2 Tint) in
                                          (typea1&&typea2)
                       
                       | Tuple(a1, l) -> let checkint = (hastype g a1 Tint) in
                                         (match t with 
                                                | Ttuple(a) ->(let rec checklist g ae at = (match ae at with 
                                                                                                | [] , [] -> true
                                                                                                | he::te, ht::tt -> if(hastype g he ht)then (checklist g te tt) else false
                                                                                                | _ , _ -> false) in
                                                               checklist g l a ) 
                                                | _ -> false )

                       | Project((a1, a2), e1) -> let typea1 = (hastype g a1 Tint) in
                                                 let typea2 = (hastype g a2 Tint) in
                                                 (typea1&&typea2)
                       |FunctionAbstraction(s, e1) -> (hastype g e1 t)

                       |FunctionCall(e1, e2) -> (match t with 
                                                       | (te1, te2) -> (hastype g e1 te1)&&(hastype g e2 te2)
                                                       | _ -> false )
                      



(* yields : ((string * exptype) list) -> definition -> ((string * exptype) list) -> bool *)
let rec yields g d g_dash = match d with 
                            | Simple( s, e) -> hastype

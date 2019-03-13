%{
    open A1
%}

/*
- Tokens (token name and rules) are modified wrt to A2. Please make necessary changes in A3
- LP and RP are left and right parenthesis
- Write grammar rules to recognize
  - >= <= from GT EQ LT tokens
  - if then else fi
*/
/* Tokens are defined below.  */
%token <int> INT
%token <bool> BOOL
%token <string> ID
%token ABS TILDA NOT PLUS MINUS TIMES DIV REM CONJ DISJ EQ GT LT LP RP IF THEN ELSE FI COMMA PROJ EOF
%start main
%type <A1.exptree> main /* Return type */
%%

main:
    cmp_expression EOL                 { $1 }          /* $n on the rhs returns the value for nth symbol in the grammar on lhs */
    | EOF                              { NULL }
;

bool_disjunction:
    LP bool_disjunction RP                  { PAREN($1) }
    |bool_disjunction DISJ bool_conjunction { DISJ($1, $3)}
    |bool_conjunction                       { $1 }
;

bool_conjunction:
    LP bool_disjunction RP                  { PAREN($1) }
    |bool_conjunction CONJ bool_not         { CONJ($1, $3) }
    |NOT bool_not                           { NOT($1)}
;

bool_not:
     BOOL                                 { BCONST($1)}
     |LP cmp_expression RP
     | bool_disjunction                   { $1 }
;

constantbool:
     BOOL                                 { BCONST($1) }
     |cmp_expression                      { $1 }

cmp_expression:
    LP cmp_expression RP                  { PAREN($1,$3) }
    | cmp_expression LT EQ sub_expression { LTE($1, $3)}
    | cmp_expression GT EQ sub_expression { GTE($1, $3)}
    | cmp_expression EQ sub_expression    { EQS($1, $3)}
    | cmp_expression LT sub_expression    { LT($1, $3)}
    | cmp_expression GT sub_expression    { GT($1, $3)}
    | sub_expression                      { $1 }
;    

sub_expression:
    LP sub_expression RP                 { PAREN($1,$3) }
    |sub_expression MINUS add_expression { MINUS($1,$3) }
    |add_expression                     { $1 }
;

add_expression:
    LP sub_expression RP                 { PAREN($1,$3) }
    |add_expression PLUS rem_expression  { PLUS($1,$3) }
    |rem_expression                     { $1 }
;

rem_expression:
    LP sub_expression RP                 { PAREN($1,$3) }
    |rem_expression REM mult_expression  { REM($1,$3) }
    |mult_expression                     { $1 }
;

mult_expression:
     LP sub_expression RP                 { PAREN($1,$3) }
     |mult_expression TIMES div_expression { MULT($1,$3) } 
     |div_expression                      { $1 }
;

div_expression:
    LP sub_expression RP                 { PAREN($1,$3) }   
    |div_expression DIV abs_expression   { DIV($1,$3) }
    |ABS abs_expression                  { ABS($1) }
    |TILDA neg_expression                { UNARYMINUS($1) }
;

abs_expression:
    LP sub_expression RP                 { PAREN($1) }
    |ifthen_expression                   { $1 }
;

neg_expression:
    LP sub_expression RP                { PAREN($1) }
    |ifthen_expression                  { $1 }
;

ifthen_expression:
    LP bool_disjunction RP              { PAREN($1) }
    |IF bool_disjunction THEN bool_disjunction ELSE bool_disjunction FI { IFTE($1,$3,$4)}
    |constant
;

constant:
    ID                                  { VAR($1) }
    |INT                                 { NCONST($1) }
;
/*
DESIGN a grammar for a simple expression language, taking care to enforce precedence rules (e.g., BODMAS)
The language should contain the following types of expressions:  integers and booleans.
*/

main:
  INT EOF   { N($1) }
;

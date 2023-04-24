open DataTypes



val SymbolDict = ref (Dictionary.create())

fun makeVarList (l::L) t = 
      ( SymbolDict := Dictionary.update (!SymbolDict) l (t l);
        (t l)::(makeVarList L t))
  | makeVarList [] t = []

fun prependAll (l::L) L' = l::(prependAll L L')
  | prependAll []     L' = L'

(*
fun set id expr =
    case Dictionary.lookup (!SymbolDict) id of 
        (INTEGER s) => SETINT (id, expr)
      | (BOOLEAN b) => SETBOOL (id, expr)
*)





     


%%

%name Rational

%term TOKER_RATIONAL | TOKER_INTEGER | TOKER_BOOLEAN | TOKER_TRUE | TOKER_FALSE | TOKER_VAR | TOKER_IF | 
      TOKER_THEN | TOKER_ELSE | TOKER_FI | TOKER_WHILE | TOKER_DO | TOKER_OD | TOKER_PROCEDURE | 
      TOKER_PRINT | TOKER_READ | TOKER_CALL | TOKER_UMINUS | TOKER_INVERSE | TOKER_RAT_ADD | 
      TOKER_RAT_SUB | TOKER_RAT_MUL | TOKER_RAT_DIV | TOKER_MAKE_RAT | TOKER_RAT | TOKER_SHOW_RAT | 
      TOKER_SHOW_DECIMAL | TOKER_FROM_DECIMAL | TOKER_TO_DECIMAL | TOKER_ADD | TOKER_SUB | TOKER_MUL | 
      TOKER_DIV | TOKER_MOD | TOKER_NEGATION | TOKER_AND | TOKER_OR | TOKER_EQ | TOKER_NE | 
      TOKER_LT | TOKER_LE | TOKER_GT | TOKER_GE | TOKER_ASSIGN | TOKER_LPAREN | TOKER_RPAREN | 
      TOKER_LBRACE | TOKER_RBRACE | TOKER_SEMICOLON | TOKER_COMMA | TOKER_ID of string | 
      TOKER_BIGINT of string | TOKER_RATIONAL_NUM of string | TOKER_EOF 

%nonterm block of BLOCK | decseq of DEC | varlist of VAR_DEC list | var of VAR_DEC list | 
         var_row of string list | proclist of PROC_DEC list | proc of PROC_DEC | 
         cmdseq of CMD list | cmdlist of CMD list | cmd of CMD | expr of EXPR | 
         prog of PROGRAM

%left TOKER_OR
%left TOKER_AND
%right TOKER_NEGATION
%left TOKER_EQ TOKER_NE TOKER_LE TOKER_LT TOKER_GE TOKER_GT
%left TOKER_ADD TOKER_SUB TOKER_RAT_ADD TOKER_RAT_SUB
%left TOKER_MUL TOKER_DIV TOKER_MOD TOKER_RAT_MUL TOKER_RAT_DIV

%pos int
%eop TOKER_EOF 
%noshift TOKER_EOF 
%nodefault
%verbose

%keyword TOKER_RATIONAL TOKER_INTEGER TOKER_BOOLEAN TOKER_TRUE TOKER_FALSE TOKER_VAR TOKER_IF TOKER_THEN 
         TOKER_ELSE TOKER_FI TOKER_WHILE TOKER_DO TOKER_OD TOKER_PROCEDURE TOKER_PRINT TOKER_READ TOKER_CALL

%arg (fileName) : string

%start prog

%%

prog: block (PROGRAM(block))

block: decseq cmdseq (BLOCK (decseq,cmdseq))

decseq: varlist proclist (DEC(varlist,proclist))

varlist: var TOKER_SEMICOLON varlist (prependAll var varlist)
       | ([])

var: TOKER_INTEGER var_row (makeVarList var_row BIGINT)
   | TOKER_BOOLEAN var_row (makeVarList var_row BOOLEAN)
   | TOKER_RATIONAL var_row (makeVarList var_row RATIONAL)

var_row: TOKER_ID TOKER_COMMA var_row (TOKER_ID::var_row)
       | TOKER_ID ([TOKER_ID])

proclist: proc TOKER_SEMICOLON proclist (proc::proclist)
        | ([])

proc: TOKER_PROCEDURE TOKER_ID block (PROCEDURE(TOKER_ID,block))

cmdseq: TOKER_LBRACE cmdlist TOKER_RBRACE (cmdlist)

cmdlist: cmd TOKER_SEMICOLON cmdlist (cmd::cmdlist)
       | ([]) 

cmd: TOKER_ID TOKER_ASSIGN expr (ASSIGNMENT(TOKER_ID,expr))
   | TOKER_CALL TOKER_ID(CALL(TOKER_ID))
   | TOKER_READ TOKER_LPAREN TOKER_ID TOKER_RPAREN (READ(TOKER_ID))
   | TOKER_PRINT TOKER_LPAREN expr TOKER_RPAREN (PRINT(expr))
   | TOKER_IF expr TOKER_THEN cmdseq TOKER_ELSE cmdseq TOKER_FI (ITE(expr,cmdseq1,cmdseq2))
   | TOKER_WHILE expr TOKER_DO cmdseq TOKER_OD (WHILE(expr,cmdseq))

expr: expr TOKER_RAT_ADD expr %prec TOKER_RAT_ADD (RAT_ADD(expr1,expr2))
    | expr TOKER_RAT_SUB expr %prec TOKER_RAT_SUB (RAT_SUB(expr1,expr2))
    | expr TOKER_RAT_MUL expr %prec TOKER_RAT_MUL (RAT_MUL(expr1,expr2))
    | expr TOKER_RAT_DIV expr %prec TOKER_RAT_DIV (RAT_DIV(expr1,expr2))
    | TOKER_LPAREN expr TOKER_RPAREN (expr)
    | TOKER_ID (case Dictionary.lookup (!SymbolDict) TOKER_ID of (BIGINT bint) => (IREF TOKER_ID) | (RATIONAL rati) => (RREF TOKER_ID) | (BOOLEAN boo) => (BREF TOKER_ID))
    | TOKER_RATIONAL_NUM (RAT_NUM(TOKER_RATIONAL_NUM))
    | TOKER_UMINUS TOKER_RATIONAL_NUM (RAT_NUM("~"^TOKER_RATIONAL_NUM))
    | TOKER_MAKE_RAT TOKER_LPAREN expr TOKER_COMMA expr TOKER_RPAREN (MAKE_RAT(expr1,expr2))
    | TOKER_FROM_DECIMAL TOKER_LPAREN expr TOKER_RPAREN (FROM_DECIMAL(expr))
    | expr TOKER_ADD expr %prec TOKER_ADD (ADD(expr1,expr2))
    | expr TOKER_SUB expr %prec TOKER_SUB (SUB(expr1,expr2))
    | expr TOKER_MUL expr %prec TOKER_MUL (MUL(expr1,expr2))
    | expr TOKER_DIV expr %prec TOKER_DIV (DIV(expr1,expr2))
    | expr TOKER_MOD expr %prec TOKER_DIV (MOD(expr1,expr2))
    | TOKER_BIGINT (NUM(TOKER_BIGINT))
    | TOKER_UMINUS TOKER_BIGINT (NUM("~"^TOKER_BIGINT))
    | TOKER_NEGATION expr (NOT(expr))
    | expr TOKER_AND expr (AND(expr1,expr2))
    | expr TOKER_OR expr (OR(expr1,expr2))
    | expr TOKER_LT expr (LT(expr1,expr2))
    | expr TOKER_LE expr (LE(expr1,expr2))
    | expr TOKER_GT expr (GT(expr1,expr2))
    | expr TOKER_GE expr (GE(expr1,expr2))
    | expr TOKER_EQ expr (EQ(expr1,expr2))
    | expr TOKER_NE expr (NE(expr1,expr2))
    | TOKER_FALSE (FALSE)
    | TOKER_TRUE (TRUE)





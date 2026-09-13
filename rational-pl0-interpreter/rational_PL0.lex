structure T = Tokens

type pos = int
type svalue = T.svalue
type ('a,'b) token = ('a,'b) T.token
type lexresult = (svalue,pos) token
type lexarg = string
type arg = lexarg

val lin = ref 1;
val col = ref 0;
val eolpos = ref 0;

val eof = fn filename => (lin := 1; col := 0; T.TOKER_EOF (!lin, !col));

fun inc a = a := !a + 1

%%
%header (functor RationalLexFun(structure Tokens: Rational_TOKENS));
%arg (fileName: string);
alpha=[A-Za-z];
digit=[0-9];
ws = [\ \t];
sign = [~+];
eol = ("\013\010"|"\010"|"\013");

%%
{ws}* => (continue ());
"(*"(.*\n)*.*"*)" => (continue());
{eol} => (inc lin; eolpos:=yypos+size yytext; continue ());

"rational" => (col:=yypos-(!eolpos); T.TOKER_RATIONAL(!lin,!col));
"integer" => (col:=yypos-(!eolpos); T.TOKER_INTEGER(!lin,!col));
"boolean" => (col:=yypos-(!eolpos); T.TOKER_BOOLEAN(!lin,!col));
"tt" => (col:=yypos-(!eolpos); T.TOKER_TRUE(!lin,!col));
"ff" => (col:=yypos-(!eolpos); T.TOKER_FALSE(!lin,!col));
"var" => (col:=yypos-(!eolpos); T.TOKER_VAR(!lin,!col));
"if" => (col:=yypos-(!eolpos); T.TOKER_IF(!lin,!col));
"then" => (col:=yypos-(!eolpos); T.TOKER_THEN(!lin,!col));
"else" => (col:=yypos-(!eolpos); T.TOKER_ELSE(!lin,!col));
"fi" => (col:=yypos-(!eolpos); T.TOKER_FI(!lin,!col));
"while" => (col:=yypos-(!eolpos); T.TOKER_WHILE(!lin,!col));
"do" => (col:=yypos-(!eolpos); T.TOKER_DO(!lin,!col));
"od" => (col:=yypos-(!eolpos); T.TOKER_OD(!lin,!col));
"procedure" => (col:=yypos-(!eolpos); T.TOKER_PROCEDURE(!lin,!col));
"print" => (col:=yypos-(!eolpos); T.TOKER_PRINT(!lin,!col));
"read" => (col:=yypos-(!eolpos); T.TOKER_READ(!lin,!col));
"call" => (col:=yypos-(!eolpos); T.TOKER_CALL(!lin,!col));

"~" => (col:=yypos-(!eolpos); T.TOKER_UMINUS(!lin,!col));
"inverse" => (col:=yypos-(!eolpos); T.TOKER_INVERSE(!lin,!col));
".+." => (col:=yypos-(!eolpos); T.TOKER_RAT_ADD(!lin,!col));
".-." => (col:=yypos-(!eolpos); T.TOKER_RAT_SUB(!lin,!col));
".*." => (col:=yypos-(!eolpos); T.TOKER_RAT_MUL(!lin,!col));
"./." => (col:=yypos-(!eolpos); T.TOKER_RAT_DIV(!lin,!col));
"make_rat" => (col:=yypos-(!eolpos); T.TOKER_MAKE_RAT(!lin,!col));

"showRat" => (col:=yypos-(!eolpos); T.TOKER_SHOW_RAT(!lin,!col));
"showDecimal" => (col:=yypos-(!eolpos); T.TOKER_SHOW_DECIMAL(!lin,!col));
"fromDecimal" => (col:=yypos-(!eolpos); T.TOKER_FROM_DECIMAL(!lin,!col));
"toDecimal" => (col:=yypos-(!eolpos); T.TOKER_TO_DECIMAL(!lin,!col));

"+" => (col:=yypos-(!eolpos); T.TOKER_ADD(!lin,!col));
"-" => (col:=yypos-(!eolpos); T.TOKER_SUB(!lin,!col));
"*" => (col:=yypos-(!eolpos); T.TOKER_MUL(!lin,!col));
"/" => (col:=yypos-(!eolpos); T.TOKER_DIV(!lin,!col));
"%" => (col:=yypos-(!eolpos); T.TOKER_MOD(!lin,!col));

"!" => (col:=yypos-(!eolpos); T.TOKER_NEGATION(!lin,!col));
"&&" => (col:=yypos-(!eolpos); T.TOKER_AND(!lin,!col));
"||" => (col:=yypos-(!eolpos); T.TOKER_OR(!lin,!col));

"=" => (col:=yypos-(!eolpos); T.TOKER_EQ(!lin,!col));
"<>" => (col:=yypos-(!eolpos); T.TOKER_NE(!lin,!col));
"<" => (col:=yypos-(!eolpos); T.TOKER_LT(!lin,!col));
"<=" => (col:=yypos-(!eolpos); T.TOKER_LE(!lin,!col));
">" => (col:=yypos-(!eolpos); T.TOKER_GT(!lin,!col));
">=" => (col:=yypos-(!eolpos); T.TOKER_GE(!lin,!col));

":=" => (col:=yypos-(!eolpos); T.TOKER_ASSIGN(!lin,!col));
"{" => (col:=yypos-(!eolpos); T.TOKER_LBRACE(!lin,!col));
"}" => (col:=yypos-(!eolpos); T.TOKER_RBRACE(!lin,!col));
"(" => (col:=yypos-(!eolpos); T.TOKER_LPAREN(!lin,!col));
")" => (col:=yypos-(!eolpos); T.TOKER_RPAREN(!lin,!col));
";" => (col:=yypos-(!eolpos); T.TOKER_SEMICOLON(!lin,!col));
"," => (col:=yypos-(!eolpos); T.TOKER_COMMA(!lin,!col));

{digit}+ => (col:=yypos-(!eolpos);T.TOKER_BIGINT(yytext,!lin,!col));
{digit}*"."{digit}*"("{digit}+")" => (col:=yypos-(!eolpos);T.TOKER_RATIONAL_NUM(yytext,!lin,!col));
[A-Za-z][A-Za-z0-9]* => (col:=yypos-(!eolpos);T.TOKER_ID(yytext,!lin,!col));
. => (print ("Unknown token found at " ^ (Int.toString (!lin)) ^ ": <" ^ yytext ^ ">. Continuing.\n"); continue());

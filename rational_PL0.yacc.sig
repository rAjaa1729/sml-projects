signature Rational_TOKENS =
sig
type ('a,'b) token
type svalue
val TOKER_EOF:  'a * 'a -> (svalue,'a) token
val TOKER_RATIONAL_NUM: (string) *  'a * 'a -> (svalue,'a) token
val TOKER_BIGINT: (string) *  'a * 'a -> (svalue,'a) token
val TOKER_ID: (string) *  'a * 'a -> (svalue,'a) token
val TOKER_COMMA:  'a * 'a -> (svalue,'a) token
val TOKER_SEMICOLON:  'a * 'a -> (svalue,'a) token
val TOKER_RBRACE:  'a * 'a -> (svalue,'a) token
val TOKER_LBRACE:  'a * 'a -> (svalue,'a) token
val TOKER_RPAREN:  'a * 'a -> (svalue,'a) token
val TOKER_LPAREN:  'a * 'a -> (svalue,'a) token
val TOKER_ASSIGN:  'a * 'a -> (svalue,'a) token
val TOKER_GE:  'a * 'a -> (svalue,'a) token
val TOKER_GT:  'a * 'a -> (svalue,'a) token
val TOKER_LE:  'a * 'a -> (svalue,'a) token
val TOKER_LT:  'a * 'a -> (svalue,'a) token
val TOKER_NE:  'a * 'a -> (svalue,'a) token
val TOKER_EQ:  'a * 'a -> (svalue,'a) token
val TOKER_OR:  'a * 'a -> (svalue,'a) token
val TOKER_AND:  'a * 'a -> (svalue,'a) token
val TOKER_NEGATION:  'a * 'a -> (svalue,'a) token
val TOKER_MOD:  'a * 'a -> (svalue,'a) token
val TOKER_DIV:  'a * 'a -> (svalue,'a) token
val TOKER_MUL:  'a * 'a -> (svalue,'a) token
val TOKER_SUB:  'a * 'a -> (svalue,'a) token
val TOKER_ADD:  'a * 'a -> (svalue,'a) token
val TOKER_TO_DECIMAL:  'a * 'a -> (svalue,'a) token
val TOKER_FROM_DECIMAL:  'a * 'a -> (svalue,'a) token
val TOKER_SHOW_DECIMAL:  'a * 'a -> (svalue,'a) token
val TOKER_SHOW_RAT:  'a * 'a -> (svalue,'a) token
val TOKER_RAT:  'a * 'a -> (svalue,'a) token
val TOKER_MAKE_RAT:  'a * 'a -> (svalue,'a) token
val TOKER_RAT_DIV:  'a * 'a -> (svalue,'a) token
val TOKER_RAT_MUL:  'a * 'a -> (svalue,'a) token
val TOKER_RAT_SUB:  'a * 'a -> (svalue,'a) token
val TOKER_RAT_ADD:  'a * 'a -> (svalue,'a) token
val TOKER_INVERSE:  'a * 'a -> (svalue,'a) token
val TOKER_UMINUS:  'a * 'a -> (svalue,'a) token
val TOKER_CALL:  'a * 'a -> (svalue,'a) token
val TOKER_READ:  'a * 'a -> (svalue,'a) token
val TOKER_PRINT:  'a * 'a -> (svalue,'a) token
val TOKER_PROCEDURE:  'a * 'a -> (svalue,'a) token
val TOKER_OD:  'a * 'a -> (svalue,'a) token
val TOKER_DO:  'a * 'a -> (svalue,'a) token
val TOKER_WHILE:  'a * 'a -> (svalue,'a) token
val TOKER_FI:  'a * 'a -> (svalue,'a) token
val TOKER_ELSE:  'a * 'a -> (svalue,'a) token
val TOKER_THEN:  'a * 'a -> (svalue,'a) token
val TOKER_IF:  'a * 'a -> (svalue,'a) token
val TOKER_VAR:  'a * 'a -> (svalue,'a) token
val TOKER_FALSE:  'a * 'a -> (svalue,'a) token
val TOKER_TRUE:  'a * 'a -> (svalue,'a) token
val TOKER_BOOLEAN:  'a * 'a -> (svalue,'a) token
val TOKER_INTEGER:  'a * 'a -> (svalue,'a) token
val TOKER_RATIONAL:  'a * 'a -> (svalue,'a) token
end
signature Rational_LRVALS=
sig
structure Tokens : Rational_TOKENS
structure ParserData:PARSER_DATA
sharing type ParserData.Token.token = Tokens.token
sharing type ParserData.svalue = Tokens.svalue
end

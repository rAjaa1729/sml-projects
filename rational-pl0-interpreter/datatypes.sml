structure DataTypes =
struct 
      datatype PROGRAM     = PROGRAM of BLOCK
      and      BLOCK    = BLOCK of DEC * (CMD list)
      and      DEC      = DEC of (VAR_DEC list) * (PROC_DEC list)
      and      VAR_DEC  = RATIONAL of string | BIGINT of string | BOOLEAN of string
      and      PROC_DEC = PROCEDURE of string * BLOCK
      and      CMD      = ASSIGNMENT of string * EXPR | CALL of string | READ of string | 
                          PRINT of EXPR | ITE of EXPR * (CMD list) * (CMD list) | 
                          WHILE of EXPR * (CMD list)
      and      EXPR     = RAT_ADD of EXPR * EXPR | RAT_SUB of EXPR * EXPR | 
                          RAT_MUL of EXPR * EXPR | RAT_DIV of EXPR * EXPR |
                          MAKE_RAT of EXPR * EXPR | FROM_DECIMAL of EXPR |
                          RAT_NUM of string | RREF of string |
                          ADD of EXPR * EXPR | SUB of EXPR * EXPR | 
                          MUL of EXPR * EXPR | DIV of EXPR * EXPR | 
                          MOD of EXPR * EXPR | NUM of string | 
                          IREF of string | AND of EXPR * EXPR | 
                          OR of EXPR * EXPR | NOT of EXPR | 
                          LT of EXPR * EXPR | GT of EXPR * EXPR | 
                          GE of EXPR * EXPR | LE of EXPR * EXPR | 
                          EQ of EXPR * EXPR | NE of EXPR * EXPR | 
                          TRUE | FALSE | BREF of string 
      exception SemanticError
end;
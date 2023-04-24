functor RationalLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : Rational_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct
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





     



end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\009\000\002\000\008\000\003\000\007\000\014\000\119\000\
\\047\000\119\000\000\000\
\\001\000\004\000\050\000\005\000\049\000\018\000\048\000\024\000\047\000\
\\028\000\046\000\035\000\045\000\045\000\044\000\051\000\043\000\
\\052\000\042\000\053\000\041\000\000\000\
\\001\000\007\000\030\000\011\000\029\000\015\000\028\000\016\000\027\000\
\\017\000\026\000\048\000\130\000\051\000\025\000\000\000\
\\001\000\008\000\137\000\012\000\137\000\020\000\137\000\021\000\137\000\
\\022\000\071\000\023\000\070\000\030\000\137\000\031\000\137\000\
\\032\000\067\000\033\000\066\000\034\000\065\000\036\000\137\000\
\\037\000\137\000\038\000\137\000\039\000\137\000\040\000\137\000\
\\041\000\137\000\042\000\137\000\043\000\137\000\046\000\137\000\
\\049\000\137\000\050\000\137\000\000\000\
\\001\000\008\000\138\000\012\000\138\000\020\000\138\000\021\000\138\000\
\\022\000\071\000\023\000\070\000\030\000\138\000\031\000\138\000\
\\032\000\067\000\033\000\066\000\034\000\065\000\036\000\138\000\
\\037\000\138\000\038\000\138\000\039\000\138\000\040\000\138\000\
\\041\000\138\000\042\000\138\000\043\000\138\000\046\000\138\000\
\\049\000\138\000\050\000\138\000\000\000\
\\001\000\008\000\139\000\012\000\139\000\020\000\139\000\021\000\139\000\
\\022\000\139\000\023\000\139\000\030\000\139\000\031\000\139\000\
\\032\000\139\000\033\000\139\000\034\000\139\000\036\000\139\000\
\\037\000\139\000\038\000\139\000\039\000\139\000\040\000\139\000\
\\041\000\139\000\042\000\139\000\043\000\139\000\046\000\139\000\
\\049\000\139\000\050\000\139\000\000\000\
\\001\000\008\000\140\000\012\000\140\000\020\000\140\000\021\000\140\000\
\\022\000\140\000\023\000\140\000\030\000\140\000\031\000\140\000\
\\032\000\140\000\033\000\140\000\034\000\140\000\036\000\140\000\
\\037\000\140\000\038\000\140\000\039\000\140\000\040\000\140\000\
\\041\000\140\000\042\000\140\000\043\000\140\000\046\000\140\000\
\\049\000\140\000\050\000\140\000\000\000\
\\001\000\008\000\141\000\012\000\141\000\020\000\141\000\021\000\141\000\
\\022\000\141\000\023\000\141\000\030\000\141\000\031\000\141\000\
\\032\000\141\000\033\000\141\000\034\000\141\000\036\000\141\000\
\\037\000\141\000\038\000\141\000\039\000\141\000\040\000\141\000\
\\041\000\141\000\042\000\141\000\043\000\141\000\046\000\141\000\
\\049\000\141\000\050\000\141\000\000\000\
\\001\000\008\000\142\000\012\000\142\000\020\000\142\000\021\000\142\000\
\\022\000\142\000\023\000\142\000\030\000\142\000\031\000\142\000\
\\032\000\142\000\033\000\142\000\034\000\142\000\036\000\142\000\
\\037\000\142\000\038\000\142\000\039\000\142\000\040\000\142\000\
\\041\000\142\000\042\000\142\000\043\000\142\000\046\000\142\000\
\\049\000\142\000\050\000\142\000\000\000\
\\001\000\008\000\143\000\012\000\143\000\020\000\143\000\021\000\143\000\
\\022\000\143\000\023\000\143\000\030\000\143\000\031\000\143\000\
\\032\000\143\000\033\000\143\000\034\000\143\000\036\000\143\000\
\\037\000\143\000\038\000\143\000\039\000\143\000\040\000\143\000\
\\041\000\143\000\042\000\143\000\043\000\143\000\046\000\143\000\
\\049\000\143\000\050\000\143\000\000\000\
\\001\000\008\000\144\000\012\000\144\000\020\000\144\000\021\000\144\000\
\\022\000\144\000\023\000\144\000\030\000\144\000\031\000\144\000\
\\032\000\144\000\033\000\144\000\034\000\144\000\036\000\144\000\
\\037\000\144\000\038\000\144\000\039\000\144\000\040\000\144\000\
\\041\000\144\000\042\000\144\000\043\000\144\000\046\000\144\000\
\\049\000\144\000\050\000\144\000\000\000\
\\001\000\008\000\145\000\012\000\145\000\020\000\145\000\021\000\145\000\
\\022\000\145\000\023\000\145\000\030\000\145\000\031\000\145\000\
\\032\000\145\000\033\000\145\000\034\000\145\000\036\000\145\000\
\\037\000\145\000\038\000\145\000\039\000\145\000\040\000\145\000\
\\041\000\145\000\042\000\145\000\043\000\145\000\046\000\145\000\
\\049\000\145\000\050\000\145\000\000\000\
\\001\000\008\000\146\000\012\000\146\000\020\000\146\000\021\000\146\000\
\\022\000\146\000\023\000\146\000\030\000\146\000\031\000\146\000\
\\032\000\146\000\033\000\146\000\034\000\146\000\036\000\146\000\
\\037\000\146\000\038\000\146\000\039\000\146\000\040\000\146\000\
\\041\000\146\000\042\000\146\000\043\000\146\000\046\000\146\000\
\\049\000\146\000\050\000\146\000\000\000\
\\001\000\008\000\147\000\012\000\147\000\020\000\147\000\021\000\147\000\
\\022\000\071\000\023\000\070\000\030\000\147\000\031\000\147\000\
\\032\000\067\000\033\000\066\000\034\000\065\000\036\000\147\000\
\\037\000\147\000\038\000\147\000\039\000\147\000\040\000\147\000\
\\041\000\147\000\042\000\147\000\043\000\147\000\046\000\147\000\
\\049\000\147\000\050\000\147\000\000\000\
\\001\000\008\000\148\000\012\000\148\000\020\000\148\000\021\000\148\000\
\\022\000\071\000\023\000\070\000\030\000\148\000\031\000\148\000\
\\032\000\067\000\033\000\066\000\034\000\065\000\036\000\148\000\
\\037\000\148\000\038\000\148\000\039\000\148\000\040\000\148\000\
\\041\000\148\000\042\000\148\000\043\000\148\000\046\000\148\000\
\\049\000\148\000\050\000\148\000\000\000\
\\001\000\008\000\149\000\012\000\149\000\020\000\149\000\021\000\149\000\
\\022\000\149\000\023\000\149\000\030\000\149\000\031\000\149\000\
\\032\000\149\000\033\000\149\000\034\000\149\000\036\000\149\000\
\\037\000\149\000\038\000\149\000\039\000\149\000\040\000\149\000\
\\041\000\149\000\042\000\149\000\043\000\149\000\046\000\149\000\
\\049\000\149\000\050\000\149\000\000\000\
\\001\000\008\000\150\000\012\000\150\000\020\000\150\000\021\000\150\000\
\\022\000\150\000\023\000\150\000\030\000\150\000\031\000\150\000\
\\032\000\150\000\033\000\150\000\034\000\150\000\036\000\150\000\
\\037\000\150\000\038\000\150\000\039\000\150\000\040\000\150\000\
\\041\000\150\000\042\000\150\000\043\000\150\000\046\000\150\000\
\\049\000\150\000\050\000\150\000\000\000\
\\001\000\008\000\151\000\012\000\151\000\020\000\151\000\021\000\151\000\
\\022\000\151\000\023\000\151\000\030\000\151\000\031\000\151\000\
\\032\000\151\000\033\000\151\000\034\000\151\000\036\000\151\000\
\\037\000\151\000\038\000\151\000\039\000\151\000\040\000\151\000\
\\041\000\151\000\042\000\151\000\043\000\151\000\046\000\151\000\
\\049\000\151\000\050\000\151\000\000\000\
\\001\000\008\000\152\000\012\000\152\000\020\000\152\000\021\000\152\000\
\\022\000\152\000\023\000\152\000\030\000\152\000\031\000\152\000\
\\032\000\152\000\033\000\152\000\034\000\152\000\036\000\152\000\
\\037\000\152\000\038\000\152\000\039\000\152\000\040\000\152\000\
\\041\000\152\000\042\000\152\000\043\000\152\000\046\000\152\000\
\\049\000\152\000\050\000\152\000\000\000\
\\001\000\008\000\153\000\012\000\153\000\020\000\153\000\021\000\153\000\
\\022\000\153\000\023\000\153\000\030\000\153\000\031\000\153\000\
\\032\000\153\000\033\000\153\000\034\000\153\000\036\000\153\000\
\\037\000\153\000\038\000\153\000\039\000\153\000\040\000\153\000\
\\041\000\153\000\042\000\153\000\043\000\153\000\046\000\153\000\
\\049\000\153\000\050\000\153\000\000\000\
\\001\000\008\000\154\000\012\000\154\000\020\000\073\000\021\000\072\000\
\\022\000\071\000\023\000\070\000\030\000\069\000\031\000\068\000\
\\032\000\067\000\033\000\066\000\034\000\065\000\036\000\154\000\
\\037\000\154\000\038\000\062\000\039\000\061\000\040\000\060\000\
\\041\000\059\000\042\000\058\000\043\000\057\000\046\000\154\000\
\\049\000\154\000\050\000\154\000\000\000\
\\001\000\008\000\155\000\012\000\155\000\020\000\073\000\021\000\072\000\
\\022\000\071\000\023\000\070\000\030\000\069\000\031\000\068\000\
\\032\000\067\000\033\000\066\000\034\000\065\000\036\000\155\000\
\\037\000\155\000\038\000\062\000\039\000\061\000\040\000\060\000\
\\041\000\059\000\042\000\058\000\043\000\057\000\046\000\155\000\
\\049\000\155\000\050\000\155\000\000\000\
\\001\000\008\000\156\000\012\000\156\000\020\000\073\000\021\000\072\000\
\\022\000\071\000\023\000\070\000\030\000\069\000\031\000\068\000\
\\032\000\067\000\033\000\066\000\034\000\065\000\036\000\064\000\
\\037\000\156\000\038\000\062\000\039\000\061\000\040\000\060\000\
\\041\000\059\000\042\000\058\000\043\000\057\000\046\000\156\000\
\\049\000\156\000\050\000\156\000\000\000\
\\001\000\008\000\157\000\012\000\157\000\020\000\073\000\021\000\072\000\
\\022\000\071\000\023\000\070\000\030\000\069\000\031\000\068\000\
\\032\000\067\000\033\000\066\000\034\000\065\000\036\000\157\000\
\\037\000\157\000\038\000\157\000\039\000\157\000\040\000\157\000\
\\041\000\157\000\042\000\157\000\043\000\157\000\046\000\157\000\
\\049\000\157\000\050\000\157\000\000\000\
\\001\000\008\000\158\000\012\000\158\000\020\000\073\000\021\000\072\000\
\\022\000\071\000\023\000\070\000\030\000\069\000\031\000\068\000\
\\032\000\067\000\033\000\066\000\034\000\065\000\036\000\158\000\
\\037\000\158\000\038\000\158\000\039\000\158\000\040\000\158\000\
\\041\000\158\000\042\000\158\000\043\000\158\000\046\000\158\000\
\\049\000\158\000\050\000\158\000\000\000\
\\001\000\008\000\159\000\012\000\159\000\020\000\073\000\021\000\072\000\
\\022\000\071\000\023\000\070\000\030\000\069\000\031\000\068\000\
\\032\000\067\000\033\000\066\000\034\000\065\000\036\000\159\000\
\\037\000\159\000\038\000\159\000\039\000\159\000\040\000\159\000\
\\041\000\159\000\042\000\159\000\043\000\159\000\046\000\159\000\
\\049\000\159\000\050\000\159\000\000\000\
\\001\000\008\000\160\000\012\000\160\000\020\000\073\000\021\000\072\000\
\\022\000\071\000\023\000\070\000\030\000\069\000\031\000\068\000\
\\032\000\067\000\033\000\066\000\034\000\065\000\036\000\160\000\
\\037\000\160\000\038\000\160\000\039\000\160\000\040\000\160\000\
\\041\000\160\000\042\000\160\000\043\000\160\000\046\000\160\000\
\\049\000\160\000\050\000\160\000\000\000\
\\001\000\008\000\161\000\012\000\161\000\020\000\073\000\021\000\072\000\
\\022\000\071\000\023\000\070\000\030\000\069\000\031\000\068\000\
\\032\000\067\000\033\000\066\000\034\000\065\000\036\000\161\000\
\\037\000\161\000\038\000\161\000\039\000\161\000\040\000\161\000\
\\041\000\161\000\042\000\161\000\043\000\161\000\046\000\161\000\
\\049\000\161\000\050\000\161\000\000\000\
\\001\000\008\000\162\000\012\000\162\000\020\000\073\000\021\000\072\000\
\\022\000\071\000\023\000\070\000\030\000\069\000\031\000\068\000\
\\032\000\067\000\033\000\066\000\034\000\065\000\036\000\162\000\
\\037\000\162\000\038\000\162\000\039\000\162\000\040\000\162\000\
\\041\000\162\000\042\000\162\000\043\000\162\000\046\000\162\000\
\\049\000\162\000\050\000\162\000\000\000\
\\001\000\008\000\163\000\012\000\163\000\020\000\163\000\021\000\163\000\
\\022\000\163\000\023\000\163\000\030\000\163\000\031\000\163\000\
\\032\000\163\000\033\000\163\000\034\000\163\000\036\000\163\000\
\\037\000\163\000\038\000\163\000\039\000\163\000\040\000\163\000\
\\041\000\163\000\042\000\163\000\043\000\163\000\046\000\163\000\
\\049\000\163\000\050\000\163\000\000\000\
\\001\000\008\000\164\000\012\000\164\000\020\000\164\000\021\000\164\000\
\\022\000\164\000\023\000\164\000\030\000\164\000\031\000\164\000\
\\032\000\164\000\033\000\164\000\034\000\164\000\036\000\164\000\
\\037\000\164\000\038\000\164\000\039\000\164\000\040\000\164\000\
\\041\000\164\000\042\000\164\000\043\000\164\000\046\000\164\000\
\\049\000\164\000\050\000\164\000\000\000\
\\001\000\008\000\081\000\020\000\073\000\021\000\072\000\022\000\071\000\
\\023\000\070\000\030\000\069\000\031\000\068\000\032\000\067\000\
\\033\000\066\000\034\000\065\000\036\000\064\000\037\000\063\000\
\\038\000\062\000\039\000\061\000\040\000\060\000\041\000\059\000\
\\042\000\058\000\043\000\057\000\000\000\
\\001\000\009\000\128\000\010\000\128\000\013\000\128\000\049\000\128\000\
\\054\000\128\000\000\000\
\\001\000\009\000\109\000\000\000\
\\001\000\010\000\113\000\000\000\
\\001\000\012\000\074\000\020\000\073\000\021\000\072\000\022\000\071\000\
\\023\000\070\000\030\000\069\000\031\000\068\000\032\000\067\000\
\\033\000\066\000\034\000\065\000\036\000\064\000\037\000\063\000\
\\038\000\062\000\039\000\061\000\040\000\060\000\041\000\059\000\
\\042\000\058\000\043\000\057\000\000\000\
\\001\000\013\000\106\000\000\000\
\\001\000\014\000\118\000\047\000\118\000\000\000\
\\001\000\014\000\013\000\047\000\126\000\000\000\
\\001\000\020\000\073\000\021\000\072\000\022\000\071\000\023\000\070\000\
\\030\000\069\000\031\000\068\000\032\000\067\000\033\000\066\000\
\\034\000\065\000\036\000\064\000\037\000\063\000\038\000\062\000\
\\039\000\061\000\040\000\060\000\041\000\059\000\042\000\058\000\
\\043\000\057\000\046\000\083\000\000\000\
\\001\000\020\000\073\000\021\000\072\000\022\000\071\000\023\000\070\000\
\\030\000\069\000\031\000\068\000\032\000\067\000\033\000\066\000\
\\034\000\065\000\036\000\064\000\037\000\063\000\038\000\062\000\
\\039\000\061\000\040\000\060\000\041\000\059\000\042\000\058\000\
\\043\000\057\000\046\000\102\000\000\000\
\\001\000\020\000\073\000\021\000\072\000\022\000\071\000\023\000\070\000\
\\030\000\069\000\031\000\068\000\032\000\067\000\033\000\066\000\
\\034\000\065\000\036\000\064\000\037\000\063\000\038\000\062\000\
\\039\000\061\000\040\000\060\000\041\000\059\000\042\000\058\000\
\\043\000\057\000\046\000\107\000\000\000\
\\001\000\020\000\073\000\021\000\072\000\022\000\071\000\023\000\070\000\
\\030\000\069\000\031\000\068\000\032\000\067\000\033\000\066\000\
\\034\000\065\000\036\000\064\000\037\000\063\000\038\000\062\000\
\\039\000\061\000\040\000\060\000\041\000\059\000\042\000\058\000\
\\043\000\057\000\046\000\112\000\000\000\
\\001\000\020\000\073\000\021\000\072\000\022\000\071\000\023\000\070\000\
\\030\000\069\000\031\000\068\000\032\000\067\000\033\000\066\000\
\\034\000\065\000\036\000\064\000\037\000\063\000\038\000\062\000\
\\039\000\061\000\040\000\060\000\041\000\059\000\042\000\058\000\
\\043\000\057\000\049\000\131\000\000\000\
\\001\000\020\000\073\000\021\000\072\000\022\000\071\000\023\000\070\000\
\\030\000\069\000\031\000\068\000\032\000\067\000\033\000\066\000\
\\034\000\065\000\036\000\064\000\037\000\063\000\038\000\062\000\
\\039\000\061\000\040\000\060\000\041\000\059\000\042\000\058\000\
\\043\000\057\000\050\000\108\000\000\000\
\\001\000\044\000\036\000\000\000\
\\001\000\045\000\038\000\000\000\
\\001\000\045\000\039\000\000\000\
\\001\000\045\000\077\000\000\000\
\\001\000\045\000\078\000\000\000\
\\001\000\046\000\082\000\000\000\
\\001\000\047\000\117\000\000\000\
\\001\000\047\000\125\000\000\000\
\\001\000\047\000\015\000\000\000\
\\001\000\048\000\129\000\000\000\
\\001\000\048\000\035\000\000\000\
\\001\000\049\000\116\000\054\000\116\000\000\000\
\\001\000\049\000\120\000\000\000\
\\001\000\049\000\121\000\000\000\
\\001\000\049\000\122\000\000\000\
\\001\000\049\000\123\000\000\000\
\\001\000\049\000\124\000\050\000\031\000\000\000\
\\001\000\049\000\127\000\000\000\
\\001\000\049\000\132\000\000\000\
\\001\000\049\000\133\000\000\000\
\\001\000\049\000\134\000\000\000\
\\001\000\049\000\135\000\000\000\
\\001\000\049\000\136\000\000\000\
\\001\000\049\000\010\000\000\000\
\\001\000\049\000\021\000\000\000\
\\001\000\049\000\034\000\000\000\
\\001\000\051\000\017\000\000\000\
\\001\000\051\000\022\000\000\000\
\\001\000\051\000\037\000\000\000\
\\001\000\051\000\055\000\000\000\
\\001\000\052\000\080\000\053\000\079\000\000\000\
\\001\000\054\000\000\000\000\000\
\\001\000\054\000\115\000\000\000\
\"
val actionRowNumbers =
"\000\000\068\000\038\000\053\000\
\\077\000\071\000\071\000\071\000\
\\000\000\069\000\051\000\072\000\
\\056\000\002\000\058\000\061\000\
\\057\000\059\000\037\000\038\000\
\\000\000\070\000\055\000\045\000\
\\073\000\046\000\047\000\001\000\
\\001\000\071\000\052\000\062\000\
\\002\000\032\000\001\000\063\000\
\\074\000\001\000\035\000\009\000\
\\018\000\008\000\001\000\001\000\
\\048\000\049\000\075\000\029\000\
\\030\000\031\000\060\000\054\000\
\\043\000\050\000\039\000\001\000\
\\001\000\001\000\001\000\001\000\
\\001\000\001\000\001\000\001\000\
\\001\000\001\000\001\000\001\000\
\\001\000\001\000\001\000\001\000\
\\053\000\040\000\020\000\001\000\
\\001\000\010\000\019\000\053\000\
\\064\000\065\000\026\000\025\000\
\\024\000\023\000\028\000\027\000\
\\022\000\021\000\017\000\016\000\
\\015\000\014\000\013\000\006\000\
\\005\000\004\000\003\000\036\000\
\\007\000\041\000\044\000\033\000\
\\067\000\012\000\001\000\053\000\
\\042\000\034\000\011\000\066\000\
\\076\000"
val gotoT =
"\
\\001\000\004\000\002\000\003\000\003\000\002\000\004\000\001\000\
\\012\000\112\000\000\000\
\\000\000\
\\006\000\010\000\007\000\009\000\000\000\
\\008\000\012\000\000\000\
\\000\000\
\\005\000\014\000\000\000\
\\005\000\016\000\000\000\
\\005\000\017\000\000\000\
\\003\000\018\000\004\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\009\000\022\000\010\000\021\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\006\000\030\000\007\000\009\000\000\000\
\\001\000\031\000\002\000\003\000\003\000\002\000\004\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\011\000\038\000\000\000\
\\011\000\049\000\000\000\
\\005\000\050\000\000\000\
\\000\000\
\\000\000\
\\009\000\051\000\010\000\021\000\000\000\
\\000\000\
\\011\000\052\000\000\000\
\\000\000\
\\000\000\
\\011\000\054\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\011\000\073\000\000\000\
\\011\000\074\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\011\000\082\000\000\000\
\\011\000\083\000\000\000\
\\011\000\084\000\000\000\
\\011\000\085\000\000\000\
\\011\000\086\000\000\000\
\\011\000\087\000\000\000\
\\011\000\088\000\000\000\
\\011\000\089\000\000\000\
\\011\000\090\000\000\000\
\\011\000\091\000\000\000\
\\011\000\092\000\000\000\
\\011\000\093\000\000\000\
\\011\000\094\000\000\000\
\\011\000\095\000\000\000\
\\011\000\096\000\000\000\
\\011\000\097\000\000\000\
\\011\000\098\000\000\000\
\\008\000\099\000\000\000\
\\000\000\
\\000\000\
\\011\000\101\000\000\000\
\\011\000\102\000\000\000\
\\000\000\
\\000\000\
\\008\000\103\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\011\000\108\000\000\000\
\\008\000\109\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 113
val numrules = 50
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle General.Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(List.map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = string
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit ->  unit
 | TOKER_RATIONAL_NUM of unit ->  (string)
 | TOKER_BIGINT of unit ->  (string) | TOKER_ID of unit ->  (string)
 | prog of unit ->  (PROGRAM) | expr of unit ->  (EXPR)
 | cmd of unit ->  (CMD) | cmdlist of unit ->  (CMD list)
 | cmdseq of unit ->  (CMD list) | proc of unit ->  (PROC_DEC)
 | proclist of unit ->  (PROC_DEC list)
 | var_row of unit ->  (string list) | var of unit ->  (VAR_DEC list)
 | varlist of unit ->  (VAR_DEC list) | decseq of unit ->  (DEC)
 | block of unit ->  (BLOCK)
end
type svalue = MlyValue.svalue
type result = PROGRAM
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn (T 0) => true | (T 1) => true | (T 2) => true | (T 3) => true | (T 
4) => true | (T 5) => true | (T 6) => true | (T 7) => true | (T 8)
 => true | (T 9) => true | (T 10) => true | (T 11) => true | (T 12)
 => true | (T 13) => true | (T 14) => true | (T 15) => true | (T 16)
 => true | _ => false
val preferred_change : (term list * term list) list = 
nil
val noShift = 
fn (T 53) => true | _ => false
val showTerminal =
fn (T 0) => "TOKER_RATIONAL"
  | (T 1) => "TOKER_INTEGER"
  | (T 2) => "TOKER_BOOLEAN"
  | (T 3) => "TOKER_TRUE"
  | (T 4) => "TOKER_FALSE"
  | (T 5) => "TOKER_VAR"
  | (T 6) => "TOKER_IF"
  | (T 7) => "TOKER_THEN"
  | (T 8) => "TOKER_ELSE"
  | (T 9) => "TOKER_FI"
  | (T 10) => "TOKER_WHILE"
  | (T 11) => "TOKER_DO"
  | (T 12) => "TOKER_OD"
  | (T 13) => "TOKER_PROCEDURE"
  | (T 14) => "TOKER_PRINT"
  | (T 15) => "TOKER_READ"
  | (T 16) => "TOKER_CALL"
  | (T 17) => "TOKER_UMINUS"
  | (T 18) => "TOKER_INVERSE"
  | (T 19) => "TOKER_RAT_ADD"
  | (T 20) => "TOKER_RAT_SUB"
  | (T 21) => "TOKER_RAT_MUL"
  | (T 22) => "TOKER_RAT_DIV"
  | (T 23) => "TOKER_MAKE_RAT"
  | (T 24) => "TOKER_RAT"
  | (T 25) => "TOKER_SHOW_RAT"
  | (T 26) => "TOKER_SHOW_DECIMAL"
  | (T 27) => "TOKER_FROM_DECIMAL"
  | (T 28) => "TOKER_TO_DECIMAL"
  | (T 29) => "TOKER_ADD"
  | (T 30) => "TOKER_SUB"
  | (T 31) => "TOKER_MUL"
  | (T 32) => "TOKER_DIV"
  | (T 33) => "TOKER_MOD"
  | (T 34) => "TOKER_NEGATION"
  | (T 35) => "TOKER_AND"
  | (T 36) => "TOKER_OR"
  | (T 37) => "TOKER_EQ"
  | (T 38) => "TOKER_NE"
  | (T 39) => "TOKER_LT"
  | (T 40) => "TOKER_LE"
  | (T 41) => "TOKER_GT"
  | (T 42) => "TOKER_GE"
  | (T 43) => "TOKER_ASSIGN"
  | (T 44) => "TOKER_LPAREN"
  | (T 45) => "TOKER_RPAREN"
  | (T 46) => "TOKER_LBRACE"
  | (T 47) => "TOKER_RBRACE"
  | (T 48) => "TOKER_SEMICOLON"
  | (T 49) => "TOKER_COMMA"
  | (T 50) => "TOKER_ID"
  | (T 51) => "TOKER_BIGINT"
  | (T 52) => "TOKER_RATIONAL_NUM"
  | (T 53) => "TOKER_EOF"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 53) $$ (T 49) $$ (T 48) $$ (T 47) $$ (T 46) $$ (T 45) $$ (T 44)
 $$ (T 43) $$ (T 42) $$ (T 41) $$ (T 40) $$ (T 39) $$ (T 38) $$ (T 37)
 $$ (T 36) $$ (T 35) $$ (T 34) $$ (T 33) $$ (T 32) $$ (T 31) $$ (T 30)
 $$ (T 29) $$ (T 28) $$ (T 27) $$ (T 26) $$ (T 25) $$ (T 24) $$ (T 23)
 $$ (T 22) $$ (T 21) $$ (T 20) $$ (T 19) $$ (T 18) $$ (T 17) $$ (T 16)
 $$ (T 15) $$ (T 14) $$ (T 13) $$ (T 12) $$ (T 11) $$ (T 10) $$ (T 9)
 $$ (T 8) $$ (T 7) $$ (T 6) $$ (T 5) $$ (T 4) $$ (T 3) $$ (T 2) $$ (T 
1) $$ (T 0)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (fileName):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.block block1, block1left, block1right)) :: 
rest671)) => let val  result = MlyValue.prog (fn _ => let val  (block
 as block1) = block1 ()
 in (PROGRAM(block))
end)
 in ( LrTable.NT 11, ( result, block1left, block1right), rest671)
end
|  ( 1, ( ( _, ( MlyValue.cmdseq cmdseq1, _, cmdseq1right)) :: ( _, ( 
MlyValue.decseq decseq1, decseq1left, _)) :: rest671)) => let val  
result = MlyValue.block (fn _ => let val  (decseq as decseq1) = 
decseq1 ()
 val  (cmdseq as cmdseq1) = cmdseq1 ()
 in (BLOCK (decseq,cmdseq))
end)
 in ( LrTable.NT 0, ( result, decseq1left, cmdseq1right), rest671)
end
|  ( 2, ( ( _, ( MlyValue.proclist proclist1, _, proclist1right)) :: (
 _, ( MlyValue.varlist varlist1, varlist1left, _)) :: rest671)) => let
 val  result = MlyValue.decseq (fn _ => let val  (varlist as varlist1)
 = varlist1 ()
 val  (proclist as proclist1) = proclist1 ()
 in (DEC(varlist,proclist))
end)
 in ( LrTable.NT 1, ( result, varlist1left, proclist1right), rest671)

end
|  ( 3, ( ( _, ( MlyValue.varlist varlist1, _, varlist1right)) :: _ ::
 ( _, ( MlyValue.var var1, var1left, _)) :: rest671)) => let val  
result = MlyValue.varlist (fn _ => let val  (var as var1) = var1 ()
 val  (varlist as varlist1) = varlist1 ()
 in (prependAll var varlist)
end)
 in ( LrTable.NT 2, ( result, var1left, varlist1right), rest671)
end
|  ( 4, ( rest671)) => let val  result = MlyValue.varlist (fn _ => ([]
))
 in ( LrTable.NT 2, ( result, defaultPos, defaultPos), rest671)
end
|  ( 5, ( ( _, ( MlyValue.var_row var_row1, _, var_row1right)) :: ( _,
 ( _, TOKER_INTEGER1left, _)) :: rest671)) => let val  result = 
MlyValue.var (fn _ => let val  (var_row as var_row1) = var_row1 ()
 in (makeVarList var_row BIGINT)
end)
 in ( LrTable.NT 3, ( result, TOKER_INTEGER1left, var_row1right), 
rest671)
end
|  ( 6, ( ( _, ( MlyValue.var_row var_row1, _, var_row1right)) :: ( _,
 ( _, TOKER_BOOLEAN1left, _)) :: rest671)) => let val  result = 
MlyValue.var (fn _ => let val  (var_row as var_row1) = var_row1 ()
 in (makeVarList var_row BOOLEAN)
end)
 in ( LrTable.NT 3, ( result, TOKER_BOOLEAN1left, var_row1right), 
rest671)
end
|  ( 7, ( ( _, ( MlyValue.var_row var_row1, _, var_row1right)) :: ( _,
 ( _, TOKER_RATIONAL1left, _)) :: rest671)) => let val  result = 
MlyValue.var (fn _ => let val  (var_row as var_row1) = var_row1 ()
 in (makeVarList var_row RATIONAL)
end)
 in ( LrTable.NT 3, ( result, TOKER_RATIONAL1left, var_row1right), 
rest671)
end
|  ( 8, ( ( _, ( MlyValue.var_row var_row1, _, var_row1right)) :: _ ::
 ( _, ( MlyValue.TOKER_ID TOKER_ID1, TOKER_ID1left, _)) :: rest671))
 => let val  result = MlyValue.var_row (fn _ => let val  (TOKER_ID as 
TOKER_ID1) = TOKER_ID1 ()
 val  (var_row as var_row1) = var_row1 ()
 in (TOKER_ID::var_row)
end)
 in ( LrTable.NT 4, ( result, TOKER_ID1left, var_row1right), rest671)

end
|  ( 9, ( ( _, ( MlyValue.TOKER_ID TOKER_ID1, TOKER_ID1left, 
TOKER_ID1right)) :: rest671)) => let val  result = MlyValue.var_row
 (fn _ => let val  (TOKER_ID as TOKER_ID1) = TOKER_ID1 ()
 in ([TOKER_ID])
end)
 in ( LrTable.NT 4, ( result, TOKER_ID1left, TOKER_ID1right), rest671)

end
|  ( 10, ( ( _, ( MlyValue.proclist proclist1, _, proclist1right)) ::
 _ :: ( _, ( MlyValue.proc proc1, proc1left, _)) :: rest671)) => let
 val  result = MlyValue.proclist (fn _ => let val  (proc as proc1) = 
proc1 ()
 val  (proclist as proclist1) = proclist1 ()
 in (proc::proclist)
end)
 in ( LrTable.NT 5, ( result, proc1left, proclist1right), rest671)
end
|  ( 11, ( rest671)) => let val  result = MlyValue.proclist (fn _ => (
[]))
 in ( LrTable.NT 5, ( result, defaultPos, defaultPos), rest671)
end
|  ( 12, ( ( _, ( MlyValue.block block1, _, block1right)) :: ( _, ( 
MlyValue.TOKER_ID TOKER_ID1, _, _)) :: ( _, ( _, TOKER_PROCEDURE1left,
 _)) :: rest671)) => let val  result = MlyValue.proc (fn _ => let val 
 (TOKER_ID as TOKER_ID1) = TOKER_ID1 ()
 val  (block as block1) = block1 ()
 in (PROCEDURE(TOKER_ID,block))
end)
 in ( LrTable.NT 6, ( result, TOKER_PROCEDURE1left, block1right), 
rest671)
end
|  ( 13, ( ( _, ( _, _, TOKER_RBRACE1right)) :: ( _, ( 
MlyValue.cmdlist cmdlist1, _, _)) :: ( _, ( _, TOKER_LBRACE1left, _))
 :: rest671)) => let val  result = MlyValue.cmdseq (fn _ => let val  (
cmdlist as cmdlist1) = cmdlist1 ()
 in (cmdlist)
end)
 in ( LrTable.NT 7, ( result, TOKER_LBRACE1left, TOKER_RBRACE1right), 
rest671)
end
|  ( 14, ( ( _, ( MlyValue.cmdlist cmdlist1, _, cmdlist1right)) :: _
 :: ( _, ( MlyValue.cmd cmd1, cmd1left, _)) :: rest671)) => let val  
result = MlyValue.cmdlist (fn _ => let val  (cmd as cmd1) = cmd1 ()
 val  (cmdlist as cmdlist1) = cmdlist1 ()
 in (cmd::cmdlist)
end)
 in ( LrTable.NT 8, ( result, cmd1left, cmdlist1right), rest671)
end
|  ( 15, ( rest671)) => let val  result = MlyValue.cmdlist (fn _ => (
[]))
 in ( LrTable.NT 8, ( result, defaultPos, defaultPos), rest671)
end
|  ( 16, ( ( _, ( MlyValue.expr expr1, _, expr1right)) :: _ :: ( _, ( 
MlyValue.TOKER_ID TOKER_ID1, TOKER_ID1left, _)) :: rest671)) => let
 val  result = MlyValue.cmd (fn _ => let val  (TOKER_ID as TOKER_ID1)
 = TOKER_ID1 ()
 val  (expr as expr1) = expr1 ()
 in (ASSIGNMENT(TOKER_ID,expr))
end)
 in ( LrTable.NT 9, ( result, TOKER_ID1left, expr1right), rest671)
end
|  ( 17, ( ( _, ( MlyValue.TOKER_ID TOKER_ID1, _, TOKER_ID1right)) :: 
( _, ( _, TOKER_CALL1left, _)) :: rest671)) => let val  result = 
MlyValue.cmd (fn _ => let val  (TOKER_ID as TOKER_ID1) = TOKER_ID1 ()
 in (CALL(TOKER_ID))
end)
 in ( LrTable.NT 9, ( result, TOKER_CALL1left, TOKER_ID1right), 
rest671)
end
|  ( 18, ( ( _, ( _, _, TOKER_RPAREN1right)) :: ( _, ( 
MlyValue.TOKER_ID TOKER_ID1, _, _)) :: _ :: ( _, ( _, TOKER_READ1left,
 _)) :: rest671)) => let val  result = MlyValue.cmd (fn _ => let val 
 (TOKER_ID as TOKER_ID1) = TOKER_ID1 ()
 in (READ(TOKER_ID))
end)
 in ( LrTable.NT 9, ( result, TOKER_READ1left, TOKER_RPAREN1right), 
rest671)
end
|  ( 19, ( ( _, ( _, _, TOKER_RPAREN1right)) :: ( _, ( MlyValue.expr 
expr1, _, _)) :: _ :: ( _, ( _, TOKER_PRINT1left, _)) :: rest671)) =>
 let val  result = MlyValue.cmd (fn _ => let val  (expr as expr1) = 
expr1 ()
 in (PRINT(expr))
end)
 in ( LrTable.NT 9, ( result, TOKER_PRINT1left, TOKER_RPAREN1right), 
rest671)
end
|  ( 20, ( ( _, ( _, _, TOKER_FI1right)) :: ( _, ( MlyValue.cmdseq 
cmdseq2, _, _)) :: _ :: ( _, ( MlyValue.cmdseq cmdseq1, _, _)) :: _ ::
 ( _, ( MlyValue.expr expr1, _, _)) :: ( _, ( _, TOKER_IF1left, _)) ::
 rest671)) => let val  result = MlyValue.cmd (fn _ => let val  (expr
 as expr1) = expr1 ()
 val  cmdseq1 = cmdseq1 ()
 val  cmdseq2 = cmdseq2 ()
 in (ITE(expr,cmdseq1,cmdseq2))
end)
 in ( LrTable.NT 9, ( result, TOKER_IF1left, TOKER_FI1right), rest671)

end
|  ( 21, ( ( _, ( _, _, TOKER_OD1right)) :: ( _, ( MlyValue.cmdseq 
cmdseq1, _, _)) :: _ :: ( _, ( MlyValue.expr expr1, _, _)) :: ( _, ( _
, TOKER_WHILE1left, _)) :: rest671)) => let val  result = MlyValue.cmd
 (fn _ => let val  (expr as expr1) = expr1 ()
 val  (cmdseq as cmdseq1) = cmdseq1 ()
 in (WHILE(expr,cmdseq))
end)
 in ( LrTable.NT 9, ( result, TOKER_WHILE1left, TOKER_OD1right), 
rest671)
end
|  ( 22, ( ( _, ( MlyValue.expr expr2, _, expr2right)) :: _ :: ( _, ( 
MlyValue.expr expr1, expr1left, _)) :: rest671)) => let val  result = 
MlyValue.expr (fn _ => let val  expr1 = expr1 ()
 val  expr2 = expr2 ()
 in (RAT_ADD(expr1,expr2))
end)
 in ( LrTable.NT 10, ( result, expr1left, expr2right), rest671)
end
|  ( 23, ( ( _, ( MlyValue.expr expr2, _, expr2right)) :: _ :: ( _, ( 
MlyValue.expr expr1, expr1left, _)) :: rest671)) => let val  result = 
MlyValue.expr (fn _ => let val  expr1 = expr1 ()
 val  expr2 = expr2 ()
 in (RAT_SUB(expr1,expr2))
end)
 in ( LrTable.NT 10, ( result, expr1left, expr2right), rest671)
end
|  ( 24, ( ( _, ( MlyValue.expr expr2, _, expr2right)) :: _ :: ( _, ( 
MlyValue.expr expr1, expr1left, _)) :: rest671)) => let val  result = 
MlyValue.expr (fn _ => let val  expr1 = expr1 ()
 val  expr2 = expr2 ()
 in (RAT_MUL(expr1,expr2))
end)
 in ( LrTable.NT 10, ( result, expr1left, expr2right), rest671)
end
|  ( 25, ( ( _, ( MlyValue.expr expr2, _, expr2right)) :: _ :: ( _, ( 
MlyValue.expr expr1, expr1left, _)) :: rest671)) => let val  result = 
MlyValue.expr (fn _ => let val  expr1 = expr1 ()
 val  expr2 = expr2 ()
 in (RAT_DIV(expr1,expr2))
end)
 in ( LrTable.NT 10, ( result, expr1left, expr2right), rest671)
end
|  ( 26, ( ( _, ( _, _, TOKER_RPAREN1right)) :: ( _, ( MlyValue.expr 
expr1, _, _)) :: ( _, ( _, TOKER_LPAREN1left, _)) :: rest671)) => let
 val  result = MlyValue.expr (fn _ => let val  (expr as expr1) = expr1
 ()
 in (expr)
end)
 in ( LrTable.NT 10, ( result, TOKER_LPAREN1left, TOKER_RPAREN1right),
 rest671)
end
|  ( 27, ( ( _, ( MlyValue.TOKER_ID TOKER_ID1, TOKER_ID1left, 
TOKER_ID1right)) :: rest671)) => let val  result = MlyValue.expr (fn _
 => let val  (TOKER_ID as TOKER_ID1) = TOKER_ID1 ()
 in (
case Dictionary.lookup (!SymbolDict) TOKER_ID of (BIGINT bint) => (IREF TOKER_ID) | (RATIONAL rati) => (RREF TOKER_ID) | (BOOLEAN boo) => (BREF TOKER_ID)
)
end)
 in ( LrTable.NT 10, ( result, TOKER_ID1left, TOKER_ID1right), rest671
)
end
|  ( 28, ( ( _, ( MlyValue.TOKER_RATIONAL_NUM TOKER_RATIONAL_NUM1, 
TOKER_RATIONAL_NUM1left, TOKER_RATIONAL_NUM1right)) :: rest671)) =>
 let val  result = MlyValue.expr (fn _ => let val  (TOKER_RATIONAL_NUM
 as TOKER_RATIONAL_NUM1) = TOKER_RATIONAL_NUM1 ()
 in (RAT_NUM(TOKER_RATIONAL_NUM))
end)
 in ( LrTable.NT 10, ( result, TOKER_RATIONAL_NUM1left, 
TOKER_RATIONAL_NUM1right), rest671)
end
|  ( 29, ( ( _, ( MlyValue.TOKER_RATIONAL_NUM TOKER_RATIONAL_NUM1, _, 
TOKER_RATIONAL_NUM1right)) :: ( _, ( _, TOKER_UMINUS1left, _)) :: 
rest671)) => let val  result = MlyValue.expr (fn _ => let val  (
TOKER_RATIONAL_NUM as TOKER_RATIONAL_NUM1) = TOKER_RATIONAL_NUM1 ()
 in (RAT_NUM("~"^TOKER_RATIONAL_NUM))
end)
 in ( LrTable.NT 10, ( result, TOKER_UMINUS1left, 
TOKER_RATIONAL_NUM1right), rest671)
end
|  ( 30, ( ( _, ( _, _, TOKER_RPAREN1right)) :: ( _, ( MlyValue.expr 
expr2, _, _)) :: _ :: ( _, ( MlyValue.expr expr1, _, _)) :: _ :: ( _, 
( _, TOKER_MAKE_RAT1left, _)) :: rest671)) => let val  result = 
MlyValue.expr (fn _ => let val  expr1 = expr1 ()
 val  expr2 = expr2 ()
 in (MAKE_RAT(expr1,expr2))
end)
 in ( LrTable.NT 10, ( result, TOKER_MAKE_RAT1left, TOKER_RPAREN1right
), rest671)
end
|  ( 31, ( ( _, ( _, _, TOKER_RPAREN1right)) :: ( _, ( MlyValue.expr 
expr1, _, _)) :: _ :: ( _, ( _, TOKER_FROM_DECIMAL1left, _)) :: 
rest671)) => let val  result = MlyValue.expr (fn _ => let val  (expr
 as expr1) = expr1 ()
 in (FROM_DECIMAL(expr))
end)
 in ( LrTable.NT 10, ( result, TOKER_FROM_DECIMAL1left, 
TOKER_RPAREN1right), rest671)
end
|  ( 32, ( ( _, ( MlyValue.expr expr2, _, expr2right)) :: _ :: ( _, ( 
MlyValue.expr expr1, expr1left, _)) :: rest671)) => let val  result = 
MlyValue.expr (fn _ => let val  expr1 = expr1 ()
 val  expr2 = expr2 ()
 in (ADD(expr1,expr2))
end)
 in ( LrTable.NT 10, ( result, expr1left, expr2right), rest671)
end
|  ( 33, ( ( _, ( MlyValue.expr expr2, _, expr2right)) :: _ :: ( _, ( 
MlyValue.expr expr1, expr1left, _)) :: rest671)) => let val  result = 
MlyValue.expr (fn _ => let val  expr1 = expr1 ()
 val  expr2 = expr2 ()
 in (SUB(expr1,expr2))
end)
 in ( LrTable.NT 10, ( result, expr1left, expr2right), rest671)
end
|  ( 34, ( ( _, ( MlyValue.expr expr2, _, expr2right)) :: _ :: ( _, ( 
MlyValue.expr expr1, expr1left, _)) :: rest671)) => let val  result = 
MlyValue.expr (fn _ => let val  expr1 = expr1 ()
 val  expr2 = expr2 ()
 in (MUL(expr1,expr2))
end)
 in ( LrTable.NT 10, ( result, expr1left, expr2right), rest671)
end
|  ( 35, ( ( _, ( MlyValue.expr expr2, _, expr2right)) :: _ :: ( _, ( 
MlyValue.expr expr1, expr1left, _)) :: rest671)) => let val  result = 
MlyValue.expr (fn _ => let val  expr1 = expr1 ()
 val  expr2 = expr2 ()
 in (DIV(expr1,expr2))
end)
 in ( LrTable.NT 10, ( result, expr1left, expr2right), rest671)
end
|  ( 36, ( ( _, ( MlyValue.expr expr2, _, expr2right)) :: _ :: ( _, ( 
MlyValue.expr expr1, expr1left, _)) :: rest671)) => let val  result = 
MlyValue.expr (fn _ => let val  expr1 = expr1 ()
 val  expr2 = expr2 ()
 in (MOD(expr1,expr2))
end)
 in ( LrTable.NT 10, ( result, expr1left, expr2right), rest671)
end
|  ( 37, ( ( _, ( MlyValue.TOKER_BIGINT TOKER_BIGINT1, 
TOKER_BIGINT1left, TOKER_BIGINT1right)) :: rest671)) => let val  
result = MlyValue.expr (fn _ => let val  (TOKER_BIGINT as 
TOKER_BIGINT1) = TOKER_BIGINT1 ()
 in (NUM(TOKER_BIGINT))
end)
 in ( LrTable.NT 10, ( result, TOKER_BIGINT1left, TOKER_BIGINT1right),
 rest671)
end
|  ( 38, ( ( _, ( MlyValue.TOKER_BIGINT TOKER_BIGINT1, _, 
TOKER_BIGINT1right)) :: ( _, ( _, TOKER_UMINUS1left, _)) :: rest671))
 => let val  result = MlyValue.expr (fn _ => let val  (TOKER_BIGINT
 as TOKER_BIGINT1) = TOKER_BIGINT1 ()
 in (NUM("~"^TOKER_BIGINT))
end)
 in ( LrTable.NT 10, ( result, TOKER_UMINUS1left, TOKER_BIGINT1right),
 rest671)
end
|  ( 39, ( ( _, ( MlyValue.expr expr1, _, expr1right)) :: ( _, ( _, 
TOKER_NEGATION1left, _)) :: rest671)) => let val  result = 
MlyValue.expr (fn _ => let val  (expr as expr1) = expr1 ()
 in (NOT(expr))
end)
 in ( LrTable.NT 10, ( result, TOKER_NEGATION1left, expr1right), 
rest671)
end
|  ( 40, ( ( _, ( MlyValue.expr expr2, _, expr2right)) :: _ :: ( _, ( 
MlyValue.expr expr1, expr1left, _)) :: rest671)) => let val  result = 
MlyValue.expr (fn _ => let val  expr1 = expr1 ()
 val  expr2 = expr2 ()
 in (AND(expr1,expr2))
end)
 in ( LrTable.NT 10, ( result, expr1left, expr2right), rest671)
end
|  ( 41, ( ( _, ( MlyValue.expr expr2, _, expr2right)) :: _ :: ( _, ( 
MlyValue.expr expr1, expr1left, _)) :: rest671)) => let val  result = 
MlyValue.expr (fn _ => let val  expr1 = expr1 ()
 val  expr2 = expr2 ()
 in (OR(expr1,expr2))
end)
 in ( LrTable.NT 10, ( result, expr1left, expr2right), rest671)
end
|  ( 42, ( ( _, ( MlyValue.expr expr2, _, expr2right)) :: _ :: ( _, ( 
MlyValue.expr expr1, expr1left, _)) :: rest671)) => let val  result = 
MlyValue.expr (fn _ => let val  expr1 = expr1 ()
 val  expr2 = expr2 ()
 in (LT(expr1,expr2))
end)
 in ( LrTable.NT 10, ( result, expr1left, expr2right), rest671)
end
|  ( 43, ( ( _, ( MlyValue.expr expr2, _, expr2right)) :: _ :: ( _, ( 
MlyValue.expr expr1, expr1left, _)) :: rest671)) => let val  result = 
MlyValue.expr (fn _ => let val  expr1 = expr1 ()
 val  expr2 = expr2 ()
 in (LE(expr1,expr2))
end)
 in ( LrTable.NT 10, ( result, expr1left, expr2right), rest671)
end
|  ( 44, ( ( _, ( MlyValue.expr expr2, _, expr2right)) :: _ :: ( _, ( 
MlyValue.expr expr1, expr1left, _)) :: rest671)) => let val  result = 
MlyValue.expr (fn _ => let val  expr1 = expr1 ()
 val  expr2 = expr2 ()
 in (GT(expr1,expr2))
end)
 in ( LrTable.NT 10, ( result, expr1left, expr2right), rest671)
end
|  ( 45, ( ( _, ( MlyValue.expr expr2, _, expr2right)) :: _ :: ( _, ( 
MlyValue.expr expr1, expr1left, _)) :: rest671)) => let val  result = 
MlyValue.expr (fn _ => let val  expr1 = expr1 ()
 val  expr2 = expr2 ()
 in (GE(expr1,expr2))
end)
 in ( LrTable.NT 10, ( result, expr1left, expr2right), rest671)
end
|  ( 46, ( ( _, ( MlyValue.expr expr2, _, expr2right)) :: _ :: ( _, ( 
MlyValue.expr expr1, expr1left, _)) :: rest671)) => let val  result = 
MlyValue.expr (fn _ => let val  expr1 = expr1 ()
 val  expr2 = expr2 ()
 in (EQ(expr1,expr2))
end)
 in ( LrTable.NT 10, ( result, expr1left, expr2right), rest671)
end
|  ( 47, ( ( _, ( MlyValue.expr expr2, _, expr2right)) :: _ :: ( _, ( 
MlyValue.expr expr1, expr1left, _)) :: rest671)) => let val  result = 
MlyValue.expr (fn _ => let val  expr1 = expr1 ()
 val  expr2 = expr2 ()
 in (NE(expr1,expr2))
end)
 in ( LrTable.NT 10, ( result, expr1left, expr2right), rest671)
end
|  ( 48, ( ( _, ( _, TOKER_FALSE1left, TOKER_FALSE1right)) :: rest671)
) => let val  result = MlyValue.expr (fn _ => (FALSE))
 in ( LrTable.NT 10, ( result, TOKER_FALSE1left, TOKER_FALSE1right), 
rest671)
end
|  ( 49, ( ( _, ( _, TOKER_TRUE1left, TOKER_TRUE1right)) :: rest671))
 => let val  result = MlyValue.expr (fn _ => (TRUE))
 in ( LrTable.NT 10, ( result, TOKER_TRUE1left, TOKER_TRUE1right), 
rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.prog x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : Rational_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun TOKER_RATIONAL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.VOID,p1,p2))
fun TOKER_INTEGER (p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.VOID,p1,p2))
fun TOKER_BOOLEAN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.VOID,p1,p2))
fun TOKER_TRUE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.VOID,p1,p2))
fun TOKER_FALSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun TOKER_VAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun TOKER_IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun TOKER_THEN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun TOKER_ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun TOKER_FI (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun TOKER_WHILE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun TOKER_DO (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun TOKER_OD (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun TOKER_PROCEDURE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun TOKER_PRINT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VOID,p1,p2))
fun TOKER_READ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.VOID,p1,p2))
fun TOKER_CALL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.VOID,p1,p2))
fun TOKER_UMINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(
ParserData.MlyValue.VOID,p1,p2))
fun TOKER_INVERSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(
ParserData.MlyValue.VOID,p1,p2))
fun TOKER_RAT_ADD (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(
ParserData.MlyValue.VOID,p1,p2))
fun TOKER_RAT_SUB (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(
ParserData.MlyValue.VOID,p1,p2))
fun TOKER_RAT_MUL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(
ParserData.MlyValue.VOID,p1,p2))
fun TOKER_RAT_DIV (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(
ParserData.MlyValue.VOID,p1,p2))
fun TOKER_MAKE_RAT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(
ParserData.MlyValue.VOID,p1,p2))
fun TOKER_RAT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(
ParserData.MlyValue.VOID,p1,p2))
fun TOKER_SHOW_RAT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(
ParserData.MlyValue.VOID,p1,p2))
fun TOKER_SHOW_DECIMAL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 26
,(ParserData.MlyValue.VOID,p1,p2))
fun TOKER_FROM_DECIMAL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 27
,(ParserData.MlyValue.VOID,p1,p2))
fun TOKER_TO_DECIMAL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(
ParserData.MlyValue.VOID,p1,p2))
fun TOKER_ADD (p1,p2) = Token.TOKEN (ParserData.LrTable.T 29,(
ParserData.MlyValue.VOID,p1,p2))
fun TOKER_SUB (p1,p2) = Token.TOKEN (ParserData.LrTable.T 30,(
ParserData.MlyValue.VOID,p1,p2))
fun TOKER_MUL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 31,(
ParserData.MlyValue.VOID,p1,p2))
fun TOKER_DIV (p1,p2) = Token.TOKEN (ParserData.LrTable.T 32,(
ParserData.MlyValue.VOID,p1,p2))
fun TOKER_MOD (p1,p2) = Token.TOKEN (ParserData.LrTable.T 33,(
ParserData.MlyValue.VOID,p1,p2))
fun TOKER_NEGATION (p1,p2) = Token.TOKEN (ParserData.LrTable.T 34,(
ParserData.MlyValue.VOID,p1,p2))
fun TOKER_AND (p1,p2) = Token.TOKEN (ParserData.LrTable.T 35,(
ParserData.MlyValue.VOID,p1,p2))
fun TOKER_OR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 36,(
ParserData.MlyValue.VOID,p1,p2))
fun TOKER_EQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 37,(
ParserData.MlyValue.VOID,p1,p2))
fun TOKER_NE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 38,(
ParserData.MlyValue.VOID,p1,p2))
fun TOKER_LT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 39,(
ParserData.MlyValue.VOID,p1,p2))
fun TOKER_LE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 40,(
ParserData.MlyValue.VOID,p1,p2))
fun TOKER_GT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 41,(
ParserData.MlyValue.VOID,p1,p2))
fun TOKER_GE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 42,(
ParserData.MlyValue.VOID,p1,p2))
fun TOKER_ASSIGN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 43,(
ParserData.MlyValue.VOID,p1,p2))
fun TOKER_LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 44,(
ParserData.MlyValue.VOID,p1,p2))
fun TOKER_RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 45,(
ParserData.MlyValue.VOID,p1,p2))
fun TOKER_LBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 46,(
ParserData.MlyValue.VOID,p1,p2))
fun TOKER_RBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 47,(
ParserData.MlyValue.VOID,p1,p2))
fun TOKER_SEMICOLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 48,(
ParserData.MlyValue.VOID,p1,p2))
fun TOKER_COMMA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 49,(
ParserData.MlyValue.VOID,p1,p2))
fun TOKER_ID (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 50,(
ParserData.MlyValue.TOKER_ID (fn () => i),p1,p2))
fun TOKER_BIGINT (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 51,(
ParserData.MlyValue.TOKER_BIGINT (fn () => i),p1,p2))
fun TOKER_RATIONAL_NUM (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 
52,(ParserData.MlyValue.TOKER_RATIONAL_NUM (fn () => i),p1,p2))
fun TOKER_EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 53,(
ParserData.MlyValue.VOID,p1,p2))
end
end

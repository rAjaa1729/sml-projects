use "bigint.sml";
signature RATIONAL =
  sig
    type rational
    exception rat_error
 
    val make_rat: BigInt.bigint * BigInt.bigint -> rational 
    val gcd: BigInt.bigint * BigInt.bigint -> BigInt.bigint 
    val rat: BigInt.bigint -> rational 
    val reci: BigInt.bigint -> rational 
    val neg: rational -> rational

    val Rat_maker:BigInt.bigint * BigInt.bigint -> rational

    val create_non_recur: string -> rational

    val inverse : rational -> rational
    val equal : rational * rational -> bool (* equality *)
    val less : rational * rational -> bool (* less than *)
    val add : rational * rational -> rational (* addition *)
    val subtract : rational * rational -> rational (* subtraction *)
    val multiply  : rational * rational -> rational (* multiplication *)
    val divide : rational * rational -> rational (* division *)
    val showRat : rational -> string
    val showDecimal : rational -> string
    val fromDecimal : string -> rational
    val toDecimal : rational -> string
    val createdeno : int * int -> string
    val formtup : string -> rational


    val rat_subfun : string * string -> string
    val rat_lessthanfun : string * string -> string
    val rat_noteqfun : string * string -> string
    val rat_greaterthanfun : string * string -> string
    val rat_notfun : string * string -> string
    val rat_greaterthaneqfun : string * string -> string
    val rat_lessthaneqfun : string * string -> string
    val rat_funadd : string * string -> string
    val rat_mulfun : string * string -> string
    val rat_divfun : string * string -> string
    val bigint_add : string * string -> string
    val sub_bigint : string * string -> string
    val mul_bigint : string * string -> string
    val divide_bigint : string * string -> string
    val modulus_bigint : string * string -> string
    
end;
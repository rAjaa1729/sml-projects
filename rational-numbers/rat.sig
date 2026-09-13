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

    val create_non_recur: string -> rational

    val inverse : rational -> rational 
    val equal : rational * rational -> bool (* equality *)
    val less : rational * rational -> bool (* less than *)
    val add : rational * rational -> rational (* addition *)
    val subtract : rational * rational -> rational (* subtraction *)
    val multiply  : rational * rational -> rational (* multiplication *)
    val divide : rational * rational -> rational  (* division *)
    val showRat : rational -> string
    val showDecimal : rational -> string
    val fromDecimal : string -> rational
    val toDecimal : rational -> string
    val createdeno : int * int -> string
end;
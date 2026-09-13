
signature RATIONAL =
  sig
    type rational
    exception rat_error
    val make_rat: bigint * bigint -> rational option
    val rat: bigint -> rational option
    val reci: bigint -> rational option
    val neg: rational -> rational
    val inverse : rational -> rational option
    val equal : rational * rational -> bool (* equality *)
    val less : rational * rational -> bool (* less than *)
    val add : rational * rational -> rational (* addition *)
    val subtract : rational * rational -> rational (* subtraction *)
    val multiply  : rational * rational -> rational (* multiplication *)
    val divide : rational * rational -> rational option (* division *)
    val showRat : rational -> string
    val showDecimal : rational -> string
    val fromDecimal : string -> rational
    val toDecimal : rational -> string
end;
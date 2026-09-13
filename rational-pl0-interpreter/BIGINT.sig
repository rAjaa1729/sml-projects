(* signature of bigint *)
signature BIGINT =
sig
    type bigint = string
    (* exception BigintDivideBy_0 *)
    (* helper functions *)
    val inString: bigint -> string
    val tobigint: string -> bigint
    
    val Add_zero: int * bigint -> bigint
    val trim_zero: bigint -> bigint
    val abs : bigint -> bigint
    val stan_bigint: bigint -> bigint
    val compare: bigint * bigint -> bool
    val addUnsigned: bigint * bigint -> bigint
    val Add : bigint * bigint -> bigint

    val subUnsigned: bigint * bigint -> bigint
    val sub : bigint * bigint -> bigint
    val mul : bigint * bigint -> bigint
    val Divide : bigint * bigint -> bigint
    val rem : bigint * bigint -> bigint
end;

signature BIGINT =
sig
    type bigint = string
    val to_bigint : string -> bigint
    val to_str : bigint -> string
    val concat : string * bigint -> bigint
    val negative_check : bigint * bigint -> bool
    val is_neg2 : bigint -> bool
    val to_pos : bigint -> bigint
    val to_neg : bigint -> bigint
    val always_pos : bigint -> bigint
    val trim1 : bigint * int -> bigint
    val trim2 : bigint * int -> bigint
    val great : bigint * bigint -> bool
    val add_help : bigint * bigint * int -> bigint
    val mx : int * int -> int
    val add : bigint * bigint -> bigint
    val sub_help : bigint * bigint * int -> bigint
    val sub : bigint * bigint -> bigint
    val mult_help : bigint * int * int -> bigint
    val add_zero : bigint * int -> bigint
    val mult_help2 : bigint * bigint * int -> bigint
    val mult : bigint * bigint -> bigint
    val suitable : bigint * bigint * int -> int
    val div_help : bigint * bigint * bigint * bigint -> bigint * bigint
    val div_ : bigint * bigint -> bigint * bigint
    val gcd : bigint * bigint -> bigint
    val find_lst : string list * int * string -> int
    val dec_part : bigint * bigint * string list * bigint -> bigint * int
    val fin_dec : bigint * bigint -> bigint
    val lgh : string * int * bool * string * string * int -> string * string * int
    val zer_num : int -> string
    val to_rat : string -> bigint * bigint
    val fin_rat : string -> bigint * bigint
    val pos1 : bigint * int -> int
    val pos2 : bigint * int -> int
    val divd : bigint * bigint -> bigint
    val modd : bigint * bigint -> bigint
end
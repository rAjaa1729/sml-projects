signature STACK =
sig
    type 'a Stack
    exception EmptyStack
    exception Error of string
    val create : unit -> 'a Stack
    val push : 'a * 'a Stack -> 'a Stack 
    val pushAll : 'a list * 'a Stack -> 'a Stack
    val pop : 'a Stack -> 'a Stack
    val top : 'a Stack -> 'a
    val empty: 'a Stack -> bool
    val poptop : 'a Stack -> ('a * 'a Stack) option 
    val nth : 'a Stack * int -> 'a
    val drop : 'a Stack * int -> 'a Stack
    val depth : 'a Stack -> int
    val app : ('a -> unit) -> 'a Stack -> unit
    val map : ('a -> 'b) -> 'a Stack -> 'b Stack
    val mapPartial : ('a -> 'b option) -> 'a Stack -> 'b Stack
    val find : ('a -> bool) -> 'a Stack -> 'a option
    val filter : ('a -> bool) -> 'a Stack -> 'a Stack
    val foldr : ('a * 'b -> 'b) -> 'b -> 'a Stack -> 'b
    val foldl : ('a * 'b -> 'b) -> 'b -> 'a Stack -> 'b
    val exists : ('a -> bool) -> 'a Stack -> bool
    val all : ('a -> bool) -> 'a Stack -> bool
    val list2stack : 'a list -> 'a Stack (* Convert a list into a Stack *) 
    val stack2list: 'a Stack -> 'a list (* Convert a Stack into a list *) 
    val toString: ('a -> string) -> 'a Stack -> string
end

structure FunStack : STACK =
struct
    type 'a Stack = 'a list
    exception EmptyStack
    exception Error of string

    fun create () = []

    fun push (a,S) = (a::S)

    fun pushAll (L,S) = (L @ S)

    fun pop [] = raise EmptyStack 
      | pop (a::S) = S

    fun top [] = raise EmptyStack
      | top (a::S) = a

    fun empty [] = true | empty s = false

    fun poptop [] = NONE | poptop (a::S) = SOME (a,S)

    fun nth ((a::S),n) = if List.length (a::S) = n then a else nth (S,n)
      | nth ([],_) = raise EmptyStack

    fun depth S = List.length S

    fun drop ((a::S),b) = if depth (a::S) = (b-1) then S else a::(drop (S,b))
      | drop ([],_) = []

    fun app f S = List.app f S
    fun map f S = List.map f S
    fun mapPartial f S = List.mapPartial f S
    fun find f S = List.find f S
    fun filter f S = List.filter f S
    fun foldr f init S = List.foldr f init S
    fun foldl f init S = List.foldl f init S
    fun exists f S = List.exists f S
    fun all f S = List.all f S

    fun list2stack S = S
    fun stack2list S = S

    fun toString f (a::[]) = ("[" ^ (f a))
      | toString f (a::S) = ((toString f S) ^ ", " ^ (f a))
end

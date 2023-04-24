structure Dictionary =
struct

      datatype dict = Dict of (string * DataTypes.VAR_DEC) list

      exception NotFound

      fun create () = Dict [];

      fun update (Dict ts) name value =
            let
                  fun inup checked ((n,v) :: others) =
                  if n = name then 
                        (n,value) :: (checked @ others)
                  else
                        inup ((n,v) :: checked) others
                  | inup checked [] = (name,value) :: checked
            in
                  Dict (inup [] ts)
            end;

      fun exists (Dict ls) name =
            List.exists (fn (n,v) => (n=name)) ls

      fun lookup (Dict ((n,v)::others)) name =
            if n=name then v
            else lookup (Dict others) name
        | lookup (Dict []) name = raise NotFound;

end;
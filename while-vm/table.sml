(**
 * @author YAMATODANI Kiyoshi
 * @copyright 2010, Tohoku University.
 *
 * Edited by Aniruddha Deb
 *)
structure Table =
struct

  type ('a,'b) table = ('a*'b) list

  datatype Type = INT | BOOLEAN
  type SymbolTable = (string,(int * Type)) table

  exception NotFound

  fun create () = [];

  fun exists ls name =
      List.exists (fn (n, v) => (n = name)) ls

  fun lookup ((n, v) :: others) name =
      if n = name
      then v
      else lookup others name
    | lookup [] name = raise NotFound;

  fun size ls = List.length ls;

  fun isEmpty [] = true
    | isEmpty _ = false;

  fun update ts name value =
      let
	fun inup checked ((n, v) :: others) =
	    if n = name then
	      (n, value) :: (checked @ others)
	    else
	      inup ((n, v) :: checked) others
	  | inup checked [] = (name, value) :: checked
      in
	(inup [] ts)
      end;

  fun remove ts name =
      let
        fun rm checked ((n, v) :: others) =
            if n = name
            then rm checked others
	    else rm ((n, v) :: checked) others
	  | rm checked [] = checked
      in
	rm [] ts
      end

  fun aslist ls = ls;

  fun keys ((n, _) :: others) = n :: (keys others)
    | keys [] = [];

  fun items ((_, v) :: others) = v::(items others)
    | items [] = [];

  fun mapkeys ls f = map (fn (k, v) => (f k, v)) ls;

  fun mapitems ls f = map (fn (k, v) => (k, f v)) ls;

end;

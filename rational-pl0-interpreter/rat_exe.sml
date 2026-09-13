val outfile = ref("output.txt");
fun open_outstream(filename : string)=
    TextIO.openOut filename
fun writeonfile( filename : string , s : string)=
    let
        val os= TextIO.openAppend filename
    in
        TextIO.output(os, s);
        TextIO.closeOut(os)
    end

structure Table =
struct 

      type ('a,'b) table = ('a * 'b) list
      datatype Type = RATIONAL | BIGINT | BOOLEAN
      type SymbolTable = (string,(string*Type)) table

      exception NotFound

      fun create () = []

      fun update ts name value =
            let
                  fun pinup checking ((n,v) :: others) =
                  if n = name then 
                        (n,value) :: (checking @ others)
                  else
                        pinup ((n,v) :: checking) others
                  | pinup checking [] = (name,value) :: checking
            in
                  (pinup [] ts)
            end;

      fun exists ls name =
            List.exists (fn (n,v) => (n=name)) ls

      fun lookup ((n,v)::others) name =
            if n=name then v
            else lookup others name
        | lookup [] name = raise NotFound;

end;



structure hashmaplist =
struct 

      datatype map = Map of (string * DataTypes.BLOCK ) list

      exception NotFound

      fun create () = Map []

      fun update (Map ts) name value =
            let
                  fun inup checked ((n,v) :: others) =
                  if n = name then 
                        (n,value) :: (checked @ others)
                  else
                        inup ((n,v) :: checked) others
                  | inup checked [] = (name,value) :: checked
            in
                  Map (inup [] ts)
            end

      fun exists (Map ls) name =
            List.exists (fn (n,v) => (n=name)) ls

      fun lookup (Map ((n,v)::others)) name =
            if n=name then v
            else lookup (Map others) name
        | lookup (Map []) name = raise NotFound

end;

structure scoping =
struct

      type ('a,'b) scope_dict = ('a*'b) list
      datatype dict = Dict of (string,(string * Table.SymbolTable)) scope_dict

      exception NotFound

      fun create () = Dict [];

      fun update (Dict ts) name value =
            let
                  fun inup checked ((n,v) :: others) =
                  if n = name then 
                        (n,value) :: (checked @ others)
                  else
                        inup ((n,v) :: checked) others
                  | inup checked [] = ((name,value) :: checked)
            in
                  (Dict (inup [] ts))
            end;

      fun exists (Dict ls) name =
            List.exists (fn (n,v) => (n=name)) ls

      fun lookup (Dict ((n,v)::others)) name =
            if n=name then v
            else lookup (Dict others) name
        | lookup (Dict []) name = raise NotFound;

end;

val globalmap = ref (hashmaplist.create());
val scopelsit = ref (hashmaplist.create());
val globalscopelist = ref (scoping.create());



fun tableofsymbol2 tablelist ((DataTypes.RATIONAL hd)::var_lst) =
      tableofsymbol2 (Table.update tablelist hd ("0",Table.RATIONAL)) var_lst
  | tableofsymbol2 tablelist ((DataTypes.BOOLEAN hd)::var_lst) =
      tableofsymbol2 (Table.update tablelist hd ("0",Table.BOOLEAN)) var_lst
  | tableofsymbol2 tablelist ((DataTypes.BIGINT hd)::var_lst) =
      tableofsymbol2 (Table.update tablelist hd ("0",Table.BIGINT)) var_lst
  | tableofsymbol2 tablelist [] = tablelist

fun tableofsymbol ((DataTypes.RATIONAL hd)::var_lst) =
      tableofsymbol2 (Table.update (Table.create()) hd ("0",Table.RATIONAL)) var_lst
  | tableofsymbol ((DataTypes.BOOLEAN hd)::var_lst) =
      tableofsymbol2 (Table.update (Table.create()) hd ("0",Table.BOOLEAN)) var_lst
  | tableofsymbol ((DataTypes.BIGINT hd)::var_lst) =
      tableofsymbol2 (Table.update (Table.create()) hd ("0",Table.BIGINT)) var_lst
  | tableofsymbol [] = (Table.create())


fun lookout (id,scopecurrent) =
      if scopecurrent="null" then raise DataTypes.SemanticError
      else
            let
                  val (xx,yy)=scoping.lookup (!globalscopelist) scopecurrent
                  val result = List.map (fn x => x * 2) [1, 2, 3, 4]

            in
                  if Table.exists yy id  then (Table.lookup yy id)
                  else lookout (id,xx)
            end

fun parseblock (exp,scopecurrent,par_scope) = 
      case exp of
            (DataTypes.RAT_ADD(a,b)) => (Rational.rat_funadd(parseblock(a,scopecurrent,par_scope),parseblock(b,scopecurrent,par_scope)))
          | (DataTypes.RAT_MUL(a,b)) => (Rational.rat_mulfun(parseblock(a,scopecurrent,par_scope),parseblock(b,scopecurrent,par_scope)))
          | (DataTypes.RAT_DIV(a,b)) => (Rational.rat_divfun(parseblock(a,scopecurrent,par_scope),parseblock(b,scopecurrent,par_scope)))
          | (DataTypes.MAKE_RAT(a,b)) => (Rational.showDecimal(Rational.Rat_maker(parseblock(a,scopecurrent,par_scope),parseblock(b,scopecurrent,par_scope))))
          | (DataTypes.RAT_SUB(a,b)) => (Rational.rat_subfun(parseblock(a,scopecurrent,par_scope),parseblock(b,scopecurrent,par_scope)))
          | (DataTypes.FROM_DECIMAL(a)) => (parseblock(a,scopecurrent,par_scope))
          | (DataTypes.RREF(a)) => (#1 (lookout(a,scopecurrent)))
          | (DataTypes.ADD(a,b)) => (Rational.bigint_add(parseblock(a,scopecurrent,par_scope),parseblock(b,scopecurrent,par_scope)))
          | (DataTypes.SUB(a,b)) => (Rational.sub_bigint(parseblock(a,scopecurrent,par_scope),parseblock(b,scopecurrent,par_scope)))
          | (DataTypes.MUL(a,b)) => (Rational.mul_bigint(parseblock(a,scopecurrent,par_scope),parseblock(b,scopecurrent,par_scope)))
          | (DataTypes.RAT_NUM(a)) => (a)
          | (DataTypes.DIV(a,b)) => (Rational.divide_bigint(parseblock(a,scopecurrent,par_scope),parseblock(b,scopecurrent,par_scope)))
          | (DataTypes.MOD(a,b)) => (Rational.modulus_bigint(parseblock(a,scopecurrent,par_scope),parseblock(b,scopecurrent,par_scope)))
          | (DataTypes.NUM(a)) => (a)
          | (DataTypes.IREF(a)) => ((#1 (lookout(a,scopecurrent))))
          | (DataTypes.AND(a,b)) => if (parseblock(a,scopecurrent,par_scope)="true") andalso (parseblock(b,scopecurrent,par_scope)="true") then "true" else "false"
          | (DataTypes.LT(a,b)) => (Rational.rat_lessthanfun(parseblock(a,scopecurrent,par_scope),parseblock(b,scopecurrent,par_scope)))
          | (DataTypes.OR(a,b)) => if (parseblock(a,scopecurrent,par_scope)="true") orelse (parseblock(b,scopecurrent,par_scope)="true") then "true" else "false"
          | (DataTypes.NOT(a)) => if (parseblock(a,scopecurrent,par_scope)="true") then "false" else "true" 
          | (DataTypes.GT(a,b)) => (Rational.rat_greaterthanfun(parseblock(a,scopecurrent,par_scope),parseblock(b,scopecurrent,par_scope)))
          | (DataTypes.GE(a,b)) => (Rational.rat_greaterthaneqfun(parseblock(a,scopecurrent,par_scope),parseblock(b,scopecurrent,par_scope)))
          | (DataTypes.LE(a,b)) => (Rational.rat_lessthaneqfun(parseblock(a,scopecurrent,par_scope),parseblock(b,scopecurrent,par_scope)))
          | (DataTypes.EQ(a,b)) => (let val x=Rational.rat_noteqfun(parseblock(a,scopecurrent,par_scope),parseblock(b,scopecurrent,par_scope)) in (x) end)
          | (DataTypes.NE(a,b)) => (Rational.rat_notfun(parseblock(a,scopecurrent,par_scope),parseblock(b,scopecurrent,par_scope)))
          | (DataTypes.TRUE) => "true"
          | (DataTypes.FALSE) => "false"
          | (DataTypes.BREF(a)) => (let val x=(#1 (lookout(a,scopecurrent))) in (x) end)

fun updating(id,scopecurrent,valu,type_val) =
      if scopecurrent="null" then raise DataTypes.SemanticError
      else
            let
                  val (xx,yy)=scoping.lookup (!globalscopelist) scopecurrent 
                  val n_valu= if valu="tt" then "true" else if valu="ff" then "false" else valu
            in
                  if Table.exists yy id  then 
                  (let val x=Table.update yy id (n_valu,type_val) in ( globalscopelist := scoping.update (!globalscopelist) scopecurrent (xx,x)) end)
                  else let val y=updating(id,xx,n_valu,type_val) in () end
            end


fun updatevalueofid(id,exp,scopecurrent,par_scope) =
      let
            val (xx,yy)=lookout(id,scopecurrent)

            fun intcheckingonly(n) =
                  if n = 0 then
                        1
                  else
                  n * intcheckingonly(n-1);

      in
            (updating(id,scopecurrent,parseblock(exp,scopecurrent,par_scope),yy))
      end

fun readingidhelp(id,scopecurrent,par_scope,valu) =
      let      
            val (xx,yy)=lookout(id,scopecurrent)
            fun searchingtype(adj_list: (int * int list) list, start_check: int) =
            let
                  val visited = Array.tabulate(length adj_list, fn _ => false)
            in
                  visited
            end;

      in
            updating(id,scopecurrent,valu,yy)
      end

fun readingid(id,scopecurrent,par_scope) =
      let val x=valOf (TextIO.inputLine TextIO.stdIn)
            val y=substring(x,0,size(x)-1)

      in readingidhelp(id,scopecurrent,par_scope,y) end

fun evaluate_blk (blockofcode,scopecurrent,par_scope) = 
      ( globalmap := hashmaplist.update (!globalmap) scopecurrent blockofcode ;
        evaluate_blk_help(blockofcode,scopecurrent,par_scope) )

and evaluate_blk_help(blockofcode,scopecurrent,par_scope) =
      let 
            val DataTypes.BLOCK(decs,coms) = blockofcode 
            val y=declaration_evaluate(decs,scopecurrent,par_scope);
            
      in
      if scopecurrent="main" then 
            (commandseq_evaluate(coms,scopecurrent,par_scope))
      else 
            ()
      end

and declaration_evaluate (dec,scopecurrent,par_scope) = 
      let 
            val DataTypes.DEC(var_lst,proc_lst) = dec
      in
        ( variablelist_evaluate(var_lst,scopecurrent,par_scope);
          procedurelst_evaluate(proc_lst,scopecurrent,par_scope) )
      end

and variablelist_evaluate (var_lst,scopecurrent,par_scope) = 
      let 
            val x=tableofsymbol(var_lst)
      in
            ( globalscopelist := scoping.update (!globalscopelist) scopecurrent (par_scope,x)
                  )
      end


and procedurelst_evaluate (P::proc_lst,scopecurrent,par_scope) = 
      let 
            val DataTypes.PROCEDURE(nm,blockofcode) = P
      in
            ( evaluate_blk(blockofcode,nm,scopecurrent);
              procedurelst_evaluate(proc_lst,scopecurrent,par_scope) )
      end
  | procedurelst_evaluate ([],scopecurrent,par_scope) = ()

and commandseq_evaluate (C::coms,scopecurrent,par_scope) =
      ( command_evaluate(C,scopecurrent,par_scope);
        commandseq_evaluate(coms,scopecurrent,par_scope) )
  | commandseq_evaluate ([],scopecurrent,par_scope) = ()


and command_evaluate (C,scopecurrent,par_scope) =
      ( case C of
          (DataTypes.ASSIGNMENT(a,b)) => (updatevalueofid(a,b,scopecurrent,par_scope))
        | (DataTypes.PRINT(a)) => 
                        let
                              val x=parseblock(a,scopecurrent,par_scope)
                        in
                              if x="true" then (writeonfile(!outfile,("tt\n")))
                              else if x="false" then (writeonfile(!outfile,("ff\n")))
                              else (writeonfile(!outfile,(x^"\n")))
                        end
        | (DataTypes.ITE(exp,cmd1,cmd2)) => let
                                    fun ite(exp,cmd1,cmd2,scopecurrent,par_scope) =
                                          let
                                                val x=parseblock(exp,scopecurrent,par_scope)
                                          in
                                                if x="true" then commandseq_evaluate(cmd1,scopecurrent,par_scope)
                                                else commandseq_evaluate(cmd2,scopecurrent,par_scope)
                                          end
                                  in
                                          ite(exp,cmd1,cmd2,scopecurrent,par_scope)
                                  end
        | (DataTypes.CALL(a)) => let 
                        fun call(id,scopecurrent,par_scope) =
                              let val DataTypes.BLOCK(decs,coms) = hashmaplist.lookup (!globalmap) id
                              in commandseq_evaluate(coms,id,scopecurrent) 
                              end
                        in  call(a,scopecurrent,par_scope)
                        end
        | (DataTypes.READ(a)) => readingid(a,scopecurrent,par_scope)
        | (DataTypes.WHILE(exp,cmd1)) => let
                                 fun whileexecute(exp,cmd1,scopecurrent,par_scope) =
                                    let
                                          val x=parseblock(exp,scopecurrent,par_scope)
                                    in
                                          if (x="true") then
                                          ( commandseq_evaluate(cmd1,scopecurrent,par_scope);
                                          whileexecute(exp,cmd1,scopecurrent,par_scope) )
                                          else ()
                                    end
                                 in
                                    whileexecute(exp,cmd1,scopecurrent,par_scope)
                                 end )




fun interpret(filename,outfile_ )=
    let 
       val (DataTypes.PROGRAM prg) = Ratpl0.compile filename  
    in
        (outfile:=outfile_ ; evaluate_blk (prg,"main","void") )
    end

val it=interpret ("testdata/factorial_power_bool.pl0","outfile.txt");
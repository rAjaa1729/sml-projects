structure RationalLrVals =
    RationalLrValsFun(structure Token = LrParser.Token)

structure RationalLex =
    RationalLexFun(structure Tokens = RationalLrVals.Tokens);


structure RationalParser = JoinWithArg(
            structure ParserData = RationalLrVals.ParserData
            structure Lex=RationalLex
            structure LrParser=LrParser);

(* compiler.sml *)
signature RATPL0 =
sig
    exception Ratpl0Error;
    val compile : string -> DataTypes.PROGRAM
end

structure Ratpl0 : RATPL0 =
struct
    exception Ratpl0Error;

    fun compile (fileName) =
    let 
        val inStream = TextIO.openIn fileName;
        val grab : int -> string = fn 
            n => if TextIO.endOfStream inStream
                 then ""
                 else TextIO.inputN (inStream,n);
        val printError : string * int * int -> unit = fn
            (msg,line,col) =>
                print (fileName^"["^Int.toString line^":"
                      ^Int.toString col^"] "^msg^"\n");
        val (tree,rem) = RationalParser.parse
                    (15,
                    (RationalParser.makeLexer grab fileName),
                    printError,
                    fileName)
            handle RationalParser.ParseError => raise Ratpl0Error;

        val _ = TextIO.closeIn inStream;
    in 
        tree
    end

end;

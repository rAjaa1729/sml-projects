signature VMC =
sig

    type VMCModel 
    type Command

    val rules: Table.SymbolTable -> VMCModel -> VMCModel
    val toString: Table.SymbolTable -> VMCModel -> string
    val postfix: DataTypes.CMD list -> Command FunStack.Stack
    val execute: string -> unit

    val createCommandStack: string -> Command FunStack.Stack
    val getTable: string -> Table.SymbolTable

end

structure Vmc : VMC =
struct

    exception TypeError
    datatype Op = ADD | SUB | MUL | DIV | MOD | GT | GE | LT | LE | EQ | NE | AND | OR | NOT

    datatype Command = Literal of int * Table.Type | Variable of string
                     | Block of Command list | Expression of Command list | ITE | WH | SET | Operator of Op
                     | RD | PR

    fun int2boolean 0 = false | int2boolean n = true;
    fun boolean2int false = 0 | boolean2int true = 1;
    fun int_not 0 = 1 | int_not 1 = 0 | int_not n = 0

    type VMCModel = Command FunStack.Stack * int array * Command FunStack.Stack

    fun op_int operator op1 op2 =
        case operator of
              ADD => op1 + op2
            | SUB => op1 - op2 
            | MUL => op1 * op2 
            | DIV => op1 div op2
            | MOD => op1 mod op2
            | LT  => boolean2int (op1 < op2)
            | GT  => boolean2int (op1 > op2)
            | GE  => boolean2int (op1 >= op2)
            | LE  => boolean2int (op1 <= op2)
            | EQ  => boolean2int (op1 = op2)
            | NE  => boolean2int (op1 <> op2)
            | a => raise TypeError

    fun dereference table name mem =
    let
        val (pos,dtype) = Table.lookup table name
    in
        if dtype = Table.INT then (Literal ((Array.sub (mem,pos)),Table.INT))
        else (Literal ((Array.sub (mem,pos)),Table.BOOLEAN))
    end

    fun type_to_string Table.BOOLEAN = "BOOLEAN" | type_to_string Table.INT = "INT"

    fun boolean_to_string true = "TT" | boolean_to_string false = "FF"

    fun op_to_string ADD = "ADD"
      | op_to_string SUB = "SUB"
      | op_to_string MUL = "MUL"
      | op_to_string DIV = "DIV"
      | op_to_string MOD = "MOD"
      | op_to_string GT = "GT"
      | op_to_string GE = "GE"
      | op_to_string LT = "LT"
      | op_to_string LE = "LE"
      | op_to_string EQ = "EQ"
      | op_to_string NE = "NE"
      | op_to_string AND = "AND"
      | op_to_string OR = "OR"
      | op_to_string NOT= "NOT"

    fun command_to_string (Literal (v,T)) = "Literal ("^(Int.toString v)^","^(type_to_string T)^")"
      | command_to_string (Variable v) = "Variable "^v
      | command_to_string (Block B) = "Block ["^(stack_to_string B)^"]"
      | command_to_string (Expression E) = "Expression ["^(stack_to_string E)^"]"
      | command_to_string ITE = "ITE"
      | command_to_string WH = "WH"
      | command_to_string SET = "SET"
      | command_to_string (Operator op1) = "Operator " ^ (op_to_string op1)
      | command_to_string RD = "RD"
      | command_to_string PR = "PR"

    and stack_to_string (c::[]) = "("^(command_to_string c)^")" 
      | stack_to_string (c::C) = "("^(command_to_string c)^")."^(stack_to_string C)
      | stack_to_string [] = "<empty>"

    fun mem_to_string ((s,(v,T))::[]) M = "\t"^s^"("^(type_to_string T)^"): "^(Int.toString (Array.sub (M,v)))^"\n"
      | mem_to_string ((s,(v,T))::L) M = "\t"^s^"("^(type_to_string T)^"): "^(Int.toString (Array.sub (M,v)))^"\n"^(mem_to_string L M)
      | mem_to_string [] M = "\t<empty>\n"

    fun toString table (V,M,C) = "V: "^(stack_to_string V)^"\nM:\n"^(mem_to_string (Table.aslist table) M)^"C: "^(stack_to_string C)^"\n"

    fun rules table (V,M,(Variable op2)::(Expression E)::SET::C) = 
        ((Variable op2)::V,M,(List.concat [E,(SET::C)]))
      | rules table ((Literal (op1,T))::(Variable op2)::V,M,SET::C) =
        let 
            val (pos,dtype) = Table.lookup table op2 
            (*val u = print ("Assigning to memory\n")*)
            val u' = if dtype = T then Array.update (M,pos,op1) else ()
        in
            if dtype <> T then raise TypeError
            else (V,M,C)
        end
      | rules table (V,M,(Variable x)::RD::C) =
        let
            val (pos,dtype) = Table.lookup table x
            val prompt = print (x^"("^(type_to_string dtype)^"): ")
            val inp_val = valOf (Int.fromString (valOf (TextIO.inputLine TextIO.stdIn)))
            val fin_val = if dtype = Table.BOOLEAN then (boolean2int (int2boolean inp_val)) else inp_val
        in
            (Array.update (M,pos,fin_val);(V,M,C))
        end
      | rules table (V,M,(Expression e)::PR::C) = (V,M,List.concat [e,PR::C])
      | rules table ((Literal (l,T))::V,M,PR::C) = 
        let
            val str = if T = Table.BOOLEAN then boolean_to_string (int2boolean l) else Int.toString l
        in
            (print (str^"\n");(V,M,C))
        end
      | rules table (V,M,(Literal (m,T))::C) = ((Literal (m,T))::V,M,C)
      | rules table (V,M,(Variable x)::C) = ((dereference table x M)::V,M,C)
      | rules table ((Literal (op1,Table.BOOLEAN))::(Literal (op2,Table.BOOLEAN))::V,M,(Operator AND)::C) =
        let
            val op1_boolean = int2boolean op1
            val op2_boolean = int2boolean op2
        in
            ((Literal ((boolean2int (op1_boolean andalso op2_boolean)),Table.BOOLEAN))::V,M,C)
        end
      | rules table ((Literal (op1,Table.BOOLEAN))::(Literal (op2,Table.BOOLEAN))::V,M,(Operator OR)::C) =
        let
            val op1_boolean = int2boolean op1
            val op2_boolean = int2boolean op2
        in
            ((Literal ((boolean2int (op1_boolean orelse op2_boolean)),Table.BOOLEAN))::V,M,C)
        end
      | rules table ((Literal (op1,T1))::(Literal (op2,T2))::V,M,(Operator a)::C) =
        let
            val int_ans = op_int a op1 op2
            (*val u = print ("Comparing " ^ (Int.toString op1) ^ " , " ^ (Int.toString op2) ^ "\n")*)
        in
            if T1 <> T2 then raise TypeError
            else if a = EQ orelse a = NE orelse a = GT orelse a = GE orelse a = LT orelse a = LE then
                ((Literal (int_ans,Table.BOOLEAN))::V,M,C)
            else
                ((Literal (int_ans,Table.INT))::V,M,C)
        end
      | rules table (V,M,(Expression e)::(Block c)::(Block d)::ITE::C) =
        (V,M,List.concat [e,((Block c)::(Block d)::ITE::C)])
      | rules table ((Literal (b,Table.BOOLEAN))::V,M,(Block c)::(Block d)::ITE::C) =
        let
            val blk = if (int2boolean b) then c else d
            (* val u = print ("If condition: " ^ (Int.toString b) ^ "\n") *)
        in
            (V,M,List.concat [blk,C])
        end
      | rules table (V,M,(Expression e)::(Block c)::WH::C) =
        ((Block c)::(Expression e)::V,M,List.concat [e,WH::C])
      | rules table ((Literal (b,Table.BOOLEAN))::(Block c)::(Expression e)::V,M,WH::C) =
        if (int2boolean b) then (V,M,List.concat [c,((Expression e)::(Block c)::WH::C)])
        else (V,M,C)
      | rules table (V,M,[]) = (V,M,[])
      | rules table model = (print (toString table model); raise While.WhileError)
        (* core dump *)

    fun parse_expr S (DataTypes.NUM n) = FunStack.push ((Literal (n,Table.INT)),S)
      | parse_expr S (DataTypes.IREF s) = FunStack.push ((Variable s),S)
      | parse_expr S (DataTypes.BREF s) = FunStack.push ((Variable s),S)
      | parse_expr S (DataTypes.TRUE) = FunStack.push ((Literal (1,Table.BOOLEAN)), S)
      | parse_expr S (DataTypes.FALSE) = FunStack.push ((Literal (0,Table.BOOLEAN)), S)
      | parse_expr S (DataTypes.NOT e) = 
    let
        val s1 = FunStack.push ((Operator NOT),S)
        val s2 = parse_expr s1 e
    in s2 end
      | parse_expr S E =
    let
        val (s1,e1,e2) = case E of 
            (DataTypes.ADD (e1,e2)) => ((FunStack.push ((Operator ADD),S)),e1,e2)
          | (DataTypes.SUB (e1,e2)) => ((FunStack.push ((Operator SUB),S)),e1,e2)
          | (DataTypes.MUL (e1,e2)) => ((FunStack.push ((Operator MUL),S)),e1,e2)
          | (DataTypes.DIV (e1,e2)) => ((FunStack.push ((Operator DIV),S)),e1,e2)
          | (DataTypes.MOD (e1,e2)) => ((FunStack.push ((Operator MOD),S)),e1,e2)
          | (DataTypes.OR (e1,e2))  => ((FunStack.push ((Operator OR),S)),e1,e2)
          | (DataTypes.AND (e1,e2)) => ((FunStack.push ((Operator AND),S)),e1,e2)
          | (DataTypes.LT (e1,e2))  => ((FunStack.push ((Operator LT),S)),e1,e2)
          | (DataTypes.GT (e1,e2)) => ((FunStack.push ((Operator GT),S)),e1,e2)
          | (DataTypes.GE (e1,e2)) => ((FunStack.push ((Operator GE),S)),e1,e2)
          | (DataTypes.LE (e1,e2)) => ((FunStack.push ((Operator LE),S)),e1,e2)
          | (DataTypes.EQ (e1,e2)) => ((FunStack.push ((Operator EQ),S)),e1,e2)
          | (DataTypes.NE (e1,e2)) => ((FunStack.push ((Operator NE),S)),e1,e2)
        val s2 = parse_expr s1 e1
        val s3 = parse_expr s2 e2
    in s3 end

    fun parse_block S CL =
    let
        val S' = FunStack.stack2list (postfix (List.rev CL))
        val B = Block S'
    in FunStack.push (B,S) end

    and postfix_rec S ((DataTypes.RD s)::L) = 
            postfix_rec (FunStack.pushAll ([(Variable s), RD], S)) L
      | postfix_rec S ((DataTypes.PR e)::L) = 
    let
        val s1 = FunStack.push (PR,S)
        val s2 = parse_expr s1 e
    in
        postfix_rec s2 L
    end
      | postfix_rec S ((DataTypes.SETINT (s,e))::L) = 
    let
        val s1 = FunStack.push (SET,S)
        val s2 = FunStack.push (Expression (parse_expr [] e),s1)
        val s3 = FunStack.push ((Variable s),s2)
    in
        postfix_rec s3 L
    end
      | postfix_rec S ((DataTypes.SETBOOLEAN (s,e))::L) = 
    let
        val s1 = FunStack.push (SET,S)
        val s2 = FunStack.push (Expression (parse_expr [] e),s1)
        val s3 = FunStack.push ((Variable s),s2)
    in
        postfix_rec s3 L
    end
      | postfix_rec S ((DataTypes.WH (e,CL))::L) = 
    let
        val s1 = FunStack.push (WH,S)
        val s2 = parse_block s1 CL
        val s3 = (FunStack.push (Expression (parse_expr [] e),s2))
    in
        postfix_rec s3 L
    end
      | postfix_rec S ((DataTypes.ITE (e,CL1,CL2))::L) =
    let
        val s1 = FunStack.push(ITE,S)
        val s2 = parse_block s1 CL2
        val s3 = parse_block s2 CL1
        val s4 = (FunStack.push (Expression (parse_expr [] e),s3))
    in
        postfix_rec s4 L
    end
      | postfix_rec S [] = S (* yay *)
            
    and postfix L = postfix_rec (FunStack.create ()) L

    fun createSymbolTableRec table idx ((DataTypes.INT s)::L) = 
        createSymbolTableRec (Table.update table s (idx,Table.INT)) (idx+1) L
      | createSymbolTableRec table idx ((DataTypes.BOOLEAN s)::L) = 
        createSymbolTableRec (Table.update table s (idx,Table.BOOLEAN)) (idx+1) L
      | createSymbolTableRec table _ [] = table

    fun createSymbolTable ((DataTypes.INT s)::L) = 
        createSymbolTableRec (Table.update (Table.create ()) s (0,Table.INT)) (1) L
      | createSymbolTable ((DataTypes.BOOLEAN s)::L) = 
        createSymbolTableRec (Table.update (Table.create ()) s (0,Table.BOOLEAN)) (1) L

    fun allocate table = Array.array ((Table.size table),0)

    fun getTable filename = 
    let
        val (DataTypes.PROG (dcl,cml)) = While.compile filename
        val table = createSymbolTable dcl
    in
        table
    end

    fun createCommandStack filename = 
    let
        val (DataTypes.PROG (dcl, cml)) = While.compile filename
        val stack = postfix (List.rev cml)
    in
        stack
    end
    fun execute filename =
    let
        val (DataTypes.PROG (dec,cmd)) = While.compile filename
        val table = createSymbolTable dec
        val cmdStack = postfix (List.rev cmd)
        val memory = allocate table
        fun evalRec table (V,M,[]) = (V,M,[]) 
          | evalRec table model = 
            let 
                val updated = (rules table model)
            in
                evalRec table updated
            end
        val u = print (toString table ([],memory,cmdStack))
    in
        print (toString table (evalRec table ([],memory,cmdStack)))
    end
end


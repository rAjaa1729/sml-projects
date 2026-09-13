# VMC Machine

In this assignment, we implement a VMC (Value Memory Command) stack for evaluating and running While programs

## Design Overview

As specified in the assignment, the general working of the VMC machine is as follows:
1. Compile the procedure to generate the relevant AST (This was implemented in the previous section)
2. The AST has two parts: a declaration sequence and a command sequence: 
  - The declaration sequence is constant, and is stored in a symbol table (which post creation is not updated). 
  - The command stack is created from the command sequence
3. The Memory is created:
  - An array of length equal to the number of declarations is created
  - For each entry in the symbol table, an integer is mapped to it which points to the location in memory where the value corresponding to that symbol is stored
4. The AST machine is made to run with the created command stack, symbol table and memory (the value stack on start is empty). Reduction rules are applied on the state of the machine to reduce the two stacks
5. The machine terminates when it encounters a runtime error (no reduction rule is present to reduce the current state of the machine), a type error (caused as a result of assigning/operating on incompatiable types), or when the command stack is empty.

The reduction rules have undergone a slight modification from those specified in the PDF: this is to account for allowing blocks and expressions to be pushed on the stack. This allows for easier pattern matching and evaluation. The datatype `Command` is as follows:
```
datatype Command = Literal of int * Table.Type | Variable of string
                 | Block of Command list | Expression of Command list | ITE | WH | SET | Operator of Op
                 | RD | PR
```

Notice that Literal allows us to effectively preserve the type of a literal on the stack. This ensures type safety, and having the type as a parameter in the constructor rather than a separate constructor itself (say `IntLiteral` and `BoolLiteral`) makes implementing pattern matching and condition handling easier. 

The notations for the modified reduction rules are the same as in the assignment, however we introduce `(Expr E)` and `(Blk B)` as expressions and blocks respectively, making it easier to pattern match. `E` and `B` refer to lists of statements, and `E.C` or `B.C` refers to pushing all the statements in the list on the stack `C`.

The modified reduction rules for this datatype are:
```
⟨V,M,(Expr E).C⟩ −> ⟨V,M,E.C⟩
⟨V,M,m.C⟩ −> ⟨m.V,M,C⟩
⟨V,M,x.C⟩ −> ⟨#x.V,M,C⟩
⟨V,M,m.n.o.C⟩ −> ⟨m.n.V,M,o.C⟩
⟨V,M,m.x.o.C⟩ −> ⟨#x.m.V,M,x.o.C⟩
⟨V,M,x.m.o.C⟩ −> ⟨m.#x.V, M, o.C⟩
⟨V,M,x.y.o.C⟩ −> ⟨#y.#x.V, M, o.C⟩
⟨n.m.V,M,o.C⟩ −> ⟨p.V,M,C⟩ 
⟨V,M,x.(Expr E).SET.C⟩ −> ⟨x.V,M,E.SET.C⟩ 
⟨m.x.V,M,SET.C⟩ −> ⟨V,M{x←m},C⟩ 
⟨V,M,(Expr E).(Blk B).(Blk D).ITE.C⟩ −> ⟨V,M,E.(Blk B).(Blk D).ITE.C⟩ 
⟨f.V,M,(Blk B).(Blk D).ITE.C⟩ −> ⟨V,M,D.C⟩
⟨t.V,M,(Blk B).(Blk D).ITE.C⟩ −> ⟨V,M,B.C⟩
⟨V,M,(Expr E).(Blk B).WH.C⟩ −> ⟨(Blk B).(Expr E).V,M,E.WH.C⟩ 
⟨f.(Blk B).(Expr E).V, M, WH.C⟩ −> ⟨V,M,C⟩
⟨t.(Blk B).(Expr E).V, M, WH.C⟩ −> ⟨V,M,(Blk B).(Expr E).(Blk B).WH.C⟩
```

## Implementation Overview

Both the value and command stack are of type `Command FunStack.Stack`. The `Command` datatype allows nesting The datatype is implemented as specified in the assignment, with the specified signature. *The implementation for the stack has been changed from an abstract one (`:>`) to a concrete one (`:`) to make it easier to pattern match a stack in SML. This is crucial for implementing the reduction rules easily*.

The `Table` datatype is a modification of the `dict` datatype previously used, with a simpler type mapping. This is used to implement the `SymbolTable` type which is used to store all the runtime-specific data about numbers and values. 

The signature `VMC`, which the structure `Vmc` implements is defined as follows:
```
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
``` 

## Usage

open up the SML interpreter:

```sml```

Set the control print depth to a larger number 

```- Control.Print.printDepth := 100;```

Compile the lex/yacc files using CM.make:

```- CM.make "while.cm";```

After successfully compiling, execute the relevant While procedure file

```Vmc.execute "<filename>";```

If all goes well, you should see an output similar to the following (this is 
the output for `test4.while`

```
V: <empty>
M:
    z(INT): 0
    x(INT): 0
    y(INT): 0
C: (Variable x).(Expression [(Literal (5,INT))]).(SET).(Expression [(Literal (0,INT)).(Variable x).(Operator GT)]).(Block [(Variable x).(PR).(Variable y).(Expression [(Literal (0,INT))]).(SET).(Expression [(Variable x).(Variable y).(Operator LT)]).(Block [(Variable y).(PR).(Variable y).(Expression [(Literal (1,INT)).(Variable y).(Operator ADD)]).(SET)]).(WH).(Variable x).(Expression [(Literal (1,INT)).(Variable x).(Operator SUB)]).(SET)]).(WH)
5
0
1
2
3
4
4
0
1
2
3
3
0
1
2
2
0
1
1
0
V: <empty>
M:
    z(INT): 0
    x(INT): 0
    y(INT): 1
C: <empty>
```

Notice that we print the initial state of the VMC machine (using `toString` internally), and then print the final state post execution. Boolean types are also represented by 0 or 1 in the memory display. On error, a core dump is printed (which is `toString` applied on the state of the machine when the error occured), and `WhileError` is thrown.

## Acknowledgements and References

Most of the work for the AST generator used the framework and design for the pi-calculus parser
in the [User's Guide to ML-Lex and ML-Yacc](http://rogerprice.org/ug/ug.pdf) by 
[Roger Price](http://rogerprice.org/). The SML standard library at [https://smlfamily.github.io](https://smlfamily.github.io) was referred to for the library definitions of `Array` and `List`. The `Dictionary` structure, which was extended to create the `Table` structure was made by Yamatodani Kiyoshi of Tohoku University. 


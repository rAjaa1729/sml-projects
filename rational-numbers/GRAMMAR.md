Author :    Raja Kumar

DATE   :    15.03.2023

GRAMMAR FOR RATIONAL NUMBERS   

M : Set containing Non terminals


Q : Set containg terminals

M = { <rational\> , <bigint\> , <Denobigint\> , <Posbigint> ,<fractal\> , <decimal\> , <recur_decimal\> ,<nonzero\> <Digits\> }

Q = { . , ~ , + , / , "0","1","2","3","4","5","6","7","8","9",(,) }

Production Rule : : 


<rational\> : : = <bigint\> | <fractal\> | <decimal\> | <recur_decimal\>

<bigint\> : : = [~+]<Posbigint\> | <Posbigint\>

<Posbigint\> ::=  <Digits>+

<nonzero\>::= "1"|"2"|"3"|"4"|"5"|"6"|"7"|"8"|"9"

<denobigint\>::= <nonzero\><posbigint\>|<nonzero\>

<fractal\> ::= <bigint\> / <denobigint>


<decimal\> : : = <bigint\> . <Digits\>+
                        | . <Digits\>+ | [~+] . <Digits\>+

<recur_decimal\> : : = <decimal\> ( <Digits\>+ )


OVERLOADED GRAMMAR FOR RATIONAL NUMBER EXPRESSIONS

Extra NONTERMINALS = {<term\> , <expr\>} 

Extra Production Rules : :

<term\> : : = (<term\>) | <term\> / <term\> | <term\> * <term\> | <rational\>

<expr\> : : = (<expr\>) | <expr\> + <expr\> | <expr\> - <expr\> | <term\>

Design Implementation

In order to know the structure of functor::

1. First of all create the structure using functor  Rational
2.  Call the function using the structure you have created

Design Decision ::
1. i have taken rational bigint * bigint in the tuple form
2. Also in bigint structure i have taken bigint as type of string
3. "" is taken as "0" and also "~" is also taken as "0"


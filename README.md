# OCL#
2
​
3
This repository provides a set of tools (parser, type-checker and evaluator) for OCL#, a reformulation of the Object Constraint Language (OCL). 
4
​
5
OCL# aims to solve some of the shortcomings with the semantics of OCL. It provides the core primitives that can be used to implement the rest of  operations and iterators in OCL.
6
​
7
## Requirements
8
​
9
The OCL# toolkit is implemented in Prolog. OCL# has been tested in [SWI Prolog](https://www.swi-prolog.org/).
## Installation

No installation is required. You can load the OCL# toolkit by loading the file `ocls.ps`. You can also load the test suite by loading the extra file `unit-test.pl`. In SWI Prolog, you can do that using the following commands 

11
    swipl
    ?- [ocls].
15
    ?- [unit_test].​

## Using the toolkit

The parser reads OCL# specifications and produces a parse tree for syntactically correct specifications. It can be used to read complete **specifications** (a lists of invariants) or, for convenience, isolated **expressions**. 

    ?- parse_ocls( "context Class inv: (1 = 3)", X ).
    X = spec([invariant("Class", equals(int_const(1), int_const(3)))]).
    ?- parse_ocls_expr( "1 = 3", X ). 
    X = equals(int_const(1), int_const(3)).


The type-checker can be used to compute the type of an OCL# expression and check the type of subexpressions. For convenience, it can be used either on a parse tree or a string containing an OCL# expression (the expression is parsed and then type-checked).

    ?- type_check_ocls( equals(int_const(1), int_const(3)), X ).
    X = ctype(boolean, 1, 1, undef, undef).

    ?- type_check_ocls_expr( "1 = 3", X ).
    X = ctype(boolean, 1, 1, undef, undef).

The evaluator can be used to compute the result of an OCL# expression. For convenience, it can be used either on a parse tree or a string containing an OCL# expression (the expression is parsed and then evaluated).

    ?- eval_ocls( equals(int_const(1), int_const(3)), [], X ).
    X = [0].

    ?- eval_ocls_expr( "1 = 3", X ).
    X = [0].
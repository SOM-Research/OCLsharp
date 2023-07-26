# OCL#

This repository provides a set of tools (parser, type-checker and evaluator) for OCL#, a reformulation of the Object Constraint Language (OCL). 

OCL# aims to solve some of the shortcomings with the semantics of OCL. It provides the core primitives that can be used to implement the rest of  operations and iterators in OCL.

This repository is the companion to the research paper providing a description of OCL#:

> Friedrich Steimann, Robert Clarisó, Martin Gogolla (2023). "OCL Rebuilt, from the Ground Up". ACM/IEEE 26th International Conference on Model-Driven Engineering Languages and Systems (MODELS), IEEE, to appear.

## Requirements

The OCL# toolkit is implemented in Prolog, so a Prolog interpreter is required to run its components. OCL# has been tested in [SWI Prolog](https://www.swi-prolog.org/) version 9.0.4. It should run in both older or newer versions of SWI-Prolog as long as they support [Definite Clause Grammars (DCG)](https://www.swi-prolog.org/pldoc/man?section=DCG), which are used to parse OCL#.

## Installation

No installation is required. You can load the OCL# toolkit by loading the file `ocls.pl`. You can also load the test suite by loading the extra file `unit_test.pl`. In SWI Prolog, you can do that using the following commands 

    swipl
    ?- [ocls].
    ?- [unit_test].

## Using the toolkit

The **parser** reads OCL# specifications and produces a parse tree for syntactically correct specifications. It can be used to read complete **specifications** (a lists of invariants) or, for convenience, isolated **expressions**. 

    ?- parse_ocls( "context Class inv: (1 = 2 + 3)", X ).
    X = spec([invariant("Class", equals(int_const(1), plus(int_const(2), int_const(3))))]).
    ?- parse_ocls_expr( "1 = 2 + 3", X ). 
    X = equals(int_const(1), plus(int_const(2), int_const(3))).

The **type-checker** can be used to compute the type of an OCL# expression and check the type of subexpressions. For convenience, it can be used either on a parse tree or a string containing an OCL# expression (the expression is parsed and then type-checked).

    ?- type_check_ocls( equals(int_const(1), plus(int_const(2), int_const(3))), X ).
    X = ctype(boolean, 1, 1, undef, undef).

    ?- type_check_ocls_expr( "1 = 2 + 3", X ).
    X = ctype(boolean, 1, 1, undef, undef).

In OCL#, every expression evaluates to a collection, and the type of the collection is implicitly defined by the uniqueness and order (or lack thereof) of values in the collection. Hence, types in OCL# combine information about multiplicity and types in a single entity called `ctype`. A `ctype` describes: its *member type*, its *minimum* and *maximum multiplicity*, its *uniqueness* (either unique, non-unique or undefined) and the *order* of its elements (ordered, unordered or undefined). The member type can either be another `ctype`, a base type (ìnteger, boolean or any) or a class name. More details about the OCL# type system are available in the OCL# paper.

The **evaluator** can be used to compute the result of an OCL# expression. For convenience, it can be used either on a parse tree or a string containing an OCL# expression (the expression is parsed and then evaluated).

    ?- eval_ocls( equals(int_const(1), plus(int_const(2), int_const(3))), [], X ).
    X = [0].

    ?- eval_ocls_expr( "1 = 2 + 3", X ).
    X = [0].

You can find a more comprehensive list of examples of OCL# expressions in the test-suite file (`unit_test.pl`).

## Executing the test-suite

If you have loaded `unit_test.pl`, you can execute the test suite by invoking `unit_test_all`:

    ?- unit_test_all.
    Starting parser test
    --------------------
    - Boolean connectives and allInstances: Passed
    - nulls and including: Passed
      ...
    All tests
     Total : 115
     Passed: 115
     Failed: 0

    true.

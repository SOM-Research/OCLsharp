# OCL#

This repository provides a set of tools (parser, type-checker and evaluator) for OCL#, a reformulation of the Object Constraint Language (OCL). This reformulation is based around two languages:
- OCL#, an OCL-like language (OCL#) supports the definition of high-level constraints and is intendend as a replacement of OCL
- OCL- (OCL core), a smaller core language providing the static and dynamic semantics for OCL#

OCL# aims to solve some of the shortcomings with the semantics of OCL. It provides several primitives that can be used to implement the rest of operations and iterators in OCL.

This repository is the companion to the research paper providing a description of OCL#:

> Friedrich Steimann, Robert Clarisó, Martin Gogolla (2023). "OCL Rebuilt, from the Ground Up". ACM/IEEE 26th International Conference on Model-Driven Engineering Languages and Systems (MODELS), IEEE, to appear.

## Requirements

The OCL# toolkit is implemented in Prolog, so a Prolog interpreter is required to run its components. OCL# has been tested in [SWI Prolog](https://www.swi-prolog.org/) version 9.0.4. It should run in both older or newer versions of SWI-Prolog as long as they support [Definite Clause Grammars (DCG)](https://www.swi-prolog.org/pldoc/man?section=DCG), which are used to parse OCL#.

## Installation

No installation is required. The OCL# tookit is split into two sets of files: `oclc.pl` provides the sources for the OCL core language, with an optional file `unit_test_core.pl` providing the unit tests; and then, `ocls.pl` provides the sources for the OCL# language, with the unit tests available in `unit_test.pl`. For instances, in SWI Prolog, you can load the OCL# sources using the following commands: 

    swipl
    ?- [ocls].
    ?- [unit_test].

Similarly, you can load the files related to the OCL- language as follows:

    swipl
    ?- [oclc].
    ?- [unit_test_core].

## Using the toolkit

The toolkit for OCL# and OCL- offers several components: a **parser**, **typechecker** and **evaluator** for OCL# and OCL-; and a **translator** that rewrites OCL# into the simpler OCL- notation. The name of the methods includes the term `ocls` (for OCL#) or `oclc`(for OCL core) to distinguish for which notation they operate.

The **parsers** read OCL# or OCL- specifications and produce a parse tree for syntactically correct specifications. It can be used to read complete **specifications** (a lists of OCL# invariants or OCL- expressions) or, for convenience, isolated **expressions**. 

    ?- parse_ocls( "context Class inv: (1 = 2 + 3)", X ).
    X = spec([invariant("Class", equals(int_const(1), plus(int_const(2), int_const(3))))]).
    ?- parse_ocls_expr( "1 = 2 + 3", X ). 
    X = equals(int_const(1), plus(int_const(2), int_const(3))).

    ?- parse_oclc( "(1 = 2 + 3)", X ).
    X = spec([equals(int_const(1), plus(int_const(2), int_const(3)))]).
    ?- parse_oclc_expr( "1 = 2 + 3", X ). 
    X = equals(int_const(1), plus(int_const(2), int_const(3))).

The following is a compact description of the grammar for OCL#:

    Spec ::= | Invariant Spec;
    Invariant ::= 'context' IDENT 'inv' ':' Expression;
    Type ::= 'bool' | 'int' | 'OclAny' | IDENT | CollectionType '(' Type ')';
    CollectionType ::= 'Set' | 'Bag' | 'Sequence' | 'OrderedSet';
    UnaryOperator ::= '-' | 'not';
    BinaryOperator ::= '=' | '+' | '-' | '*' | '/' | 'and' | 'or';
    TypeCast ::= 'asBag' | 'asSet' | 'asSequence' | 'asOrderedSet';
    Expression ::= self | true | false | INT_VALUE | IDENT | 
                   'no' '(' Type ')' |
                   IDENT '.' 'allInstances' '(' ')' |
                   Expression '.' IDENT |
                   Expression '->' 'including' '(' Expression ')' |
                   Expression '->' 'iterate' '(' IDENT ';' IDENT '=' Expression '|' Expression ')' |
                   Expression '->' TypeCast '(' ')' |
                   '(' Expression ')' |
                   UnaryOperator Expression |
                   Expression BinaryOperator Expresion |
                   'if' Expression 'then' Expression 'else' Expression 'endif';         

The **type-checkers** can be used to compute the type of an OCL# or OCL- expression and check the type of subexpressions. For convenience, it can be used either on a parse tree or a string containing an OCL# or OCL- expression (the expression is parsed and then type-checked).

    ?- type_check_ocls( equals(int_const(1), plus(int_const(2), int_const(3))), X ).
    X = ctype(boolean, 1, 1, undef, undef).

    ?- type_check_ocls_expr( "1 = 2 + 3", X ).
    X = ctype(boolean, 1, 1, undef, undef).

    ?- type_check_oclc( equals(int_const(1), plus(int_const(2), int_const(3))), X ).
    X = ctype(boolean, 1, 1, undef, undef).

    ?- type_check_oclc_expr( "1 = 2 + 3", X ).
    X = ctype(boolean, 1, 1, undef, undef).

In OCL# and OCL-, every expression evaluates to a collection. The type of the collection (Set, Sequence, ...) is implicitly defined by the uniqueness and order (or lack thereof) of values in the collection. Hence, types in OCL# combine information about multiplicity and types in a single entity called `ctype`. A `ctype` describes: its *member type*, its *minimum* and *maximum multiplicity*, its *uniqueness* (either unique, non-unique or undefined) and the *order* of its elements (ordered, unordered or undefined). The member type can either be another `ctype`, a base type (ìnteger, boolean or any) or a class name. More details about the OCL# type system are available in the OCL# paper.

*Warning:* The current implementation of the type-checker is very verbose and will output information about the type of subexpressions to facilitate debugging. This information can be safely ignored if you are only interested in the type of the complete expression.

The **evaluator** can be used to compute the result of an OCL# or OCL- expression. For convenience, it can be used either on a parse tree or a string containing an OCL# or OCL- expression (the expression is parsed, type-checked and then evaluated).

    ?- eval_ocls( equals(int_const(1), plus(int_const(2), int_const(3))), [], X ).
    X = [0].

    ?- eval_ocls_expr( "1 = 2 + 3", X ).
    X = [0].

    ?- eval_oclc( equals(int_const(1), plus(int_const(2), int_const(3))), [], X ).
    X = [0].

    ?- eval_oclc_expr( "1 = 2 + 3", X ).
    X = [0].

You can find a more comprehensive list of examples of OCL# expressions in the test-suite file (`unit_test.pl`).

The **translator** rewrites the syntax tree of a OCL# complex expression as the syntax tree of an OCL- expression, that rely on a smaller and simpler core operators. OCL- is intended to provide the formal semantics for OCL. For example, OCL- does not include a single operand "minus" operation, so `-1` is rewritten as `0-1`.

    ?- translate_ocls_to_oclc( negative(int_const(1)), X ).
    X = minus(int_const(0), int_const(1)).

## Executing the test-suite

If you have loaded `unit_test.pl` or `unit_test_core.pl`, you can execute the test suite by invoking `unit_test_all`:

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

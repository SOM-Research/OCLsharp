:- set_prolog_flag(double_quotes, chars).
:- use_module(library(dcg/basics)).

% -- INTERFACE---------------------------------------------------------------

%
% OCL# parser - parsing complete specifications
%
% parse_ocls( Spec, Tree )
% 
% Parse an OCL specification stored in a string. No type checking is perfomed 
% at this stage.
% - Spec: (Input) OCL# Specification, between double quotes.
% - Tree: (Output) Parse tree of the specification.
% 
% 
% Example:
% ?- parse_ocls( "context C inv: true" , X)
% X = spec( [invariant("C", bool_const(1))] )

parse_ocls(Spec, Tree) :-
   ( phrase(spec(Tree), Spec) ->
     !
   ; write('Error: Parse error reading - '),
     atom_string(X, Spec),
     write(X), nl,
     fail
   ).

%
% OCL# expression parser - parsing an isolated expression
%
% parse_ocls_expr( ExprSpec, Tree )
% 
% Parse an OCL expression stored in a string. The method builds a fake invariant
% around the expression before passing it to the parser.
% No type checking is perfomed at this stage.
% - ExprSpec: (Input) An OCL# expression, between double quotes.
% - Tree: (Output) Parse tree of the specification.
% 
% 
% Example:
% ?- parse_ocls_expr( "true" , X)
% X = bool_const(1)

parse_ocls_expr(Spec, Tree) :-
  append("context C inv: ", Spec, Input),
  parse_ocls( Input, spec([invariant(_, Tree)]) ).


%
% OCL# type-checker - checking a specification in a parse tree
%
% type_check_ocls( Model, Tree, Type )
% 
% Compute the type of an expression (stored as a parse tree) and check 
% the types of subexpressions.
% - Tree   (Input)  Parse tree of the specification.
% - Type:  (Output) Type of the expression.
% 
% Example:
% ?- type_check_ocls( plus(int_const(1), int_const(2))] , X)
% X = ctype(integer, 0, 1, undef, undef)

type_check_ocls( Expr, Type ) :- 
  ( type_check_ocls([], Expr, TheType) ->
    Type = TheType
  ; write('Error: Type error in expression - '),
    write(Expr), nl,
    fail
  ).

%
% OCL# type-checker - checking a OCL# expression in a string
%
% type_check_ocls_expr( Spec, Type )
% 
% Compute the type of an expression (stored as a string) and check the 
% types of subexpressions. 
% The method builds a fake invariant around the expression before passing it 
% to the parser. As a result, type checking will fail for expressions 
% including the "self" keyword.
% - Model: (Input) List of variable definitions ([] if there are none).
% - Spec:  (Input) OCL# Specification, between double quotes.
% - Tree:  (Output) Parse tree of the specification.
% 
% Example:
% ?- type_check_ocls_expr( "1+2" , X)
% X = ctype(integer, 0, 1, undef, undef)

type_check_ocls_expr( Spec, Type ) :-
  parse_ocls_expr(Spec, Tree),
  type_check_ocls(Tree, Type).

%
% OCL# evaluator - evaluate a specification in a parse tree
%
% eval_ocls( ExprTree, Model, Result )
% 
% Evaluate an expression (provided as a parse tree) and compute the result.
% - Tree:  (Input)  Parse tree of the expression to be evaluated.
% - Model: (Input)  List of variable definitions ([] if there are none).
% - Result:(Output) Value of the result of the expression.
% 
% Example:
% ?- eval_ocls( plus(int_const(1), int_const(2)), [] , X)
% X = [3]

%
% OCL# evaluator - from a String
%
% eval_ocls_expr( Spec, Result )
% 
% Evaluate an expression (stored in a String) and compute the result.
% The method builds a fake invariant around the expression before passing it 
% to the parser. As a result, evaluation will fail for expressions including the
% "self" keyword.
% - Spec:  (Input)  OCL# Specification, between double quotes.
% - Result:(Output) Value of the result of the expression.
% 
% Example:
% ?- eval_ocls_expr( "1+2", X)
% X = [3]


eval_ocls_expr( Spec, Type ) :-
  parse_ocls_expr(Spec, Tree),
  eval_ocls(Tree, [], Type).

% -- IMPLEMENTATION --------------------------------------------------------

%
% OCL# parser 
%

% A OCL# specification is a list of invariants

spec(spec([invariant(Class, Body)|Rest])) --> 
   invariant(Class, Body), 
   blanks,
   spec(spec(Rest)).
spec(spec([])) --> 
   blanks.

% An invariant defines a class name and a constraint 

invariant(Class, Body) --> 
   "context", 
   blanks,
   ident(Class),
   blanks, 
   "inv", 
   blanks,
   ":", 
   blanks,
   expr(Class, Body).

% Identifier in OCL#
ident(String) --> 
   string_without(" \t\n\r.,-+*()><=:;|[]", StringCodes),
   { nth0(0, StringCodes, _),
     string_codes(String, StringCodes) }.

const_expr(Class, self(Class)) --> 
   "self".

const_expr(_, bool_const(1)) --> 
   "true".

const_expr(_, bool_const(0)) --> 
   "false".

const_expr(_, int_const(X)) -->
  integer(X).

const_expr(Class, Expr) -->
  "Set",
  blanks,
  "(",
  type(Type),
  ")",
  blanks,
  "{",
  expression_list(Class, Elements),
  { build_set(Type, Elements, Expr) }.

const_expr(Class, Expr) -->
  "Sequence",
  blanks,
  "(",
  type(Type),
  ")",
  blanks,
  "{",
  expression_list(Class, Elements),
  { build_sequence(Type, Elements, Expr) }.

const_expr(Class, Expr) -->
  "Bag",
  blanks,
  "(",
  type(Type),
  ")",
  blanks,
  "{",
  expression_list(Class, Elements),
  { build_bag(Type, Elements, Expr) }.

const_expr(Class, Expr) -->
  "OrderedSet",
  blanks,
  "(",
  type(Type),
  ")",
  blanks,
  "{",
  expression_list(Class, Elements),
  { build_ordered_set(Type, Elements, Expr) }.

expression_list(_, []) -->  
  blanks,
  "}".

expression_list(Class, [H|T]) -->
  expr(Class, H),
  blanks,
  rest_expression_list(Class, T). 

rest_expression_list(_, []) -->
  "}".
rest_expression_list(Class, T) -->
  ",",
  expression_list(Class, T).

ident_expr(_, ident(Name)) -->
  ident(Name).

paren_expr(Class, Exp) -->
%paren_expr(Class, paren(Exp)) -->
  "(",
  blanks,
  expr(Class, Exp),
  blanks,
  ")".

null_expr(_, null(Type)) -->
   "no",
   blanks,
   "(",
   blanks,
   type(Type),
   blanks,
   ")".

all_instances_expr(_, all_instances(Class)) --> 
   ident(Class),
   blanks,
   ".",
   blanks,
   "allInstances",
   blanks,
   "(",
   blanks,
   ")".

change_sign_expr(Class, negative(Exp)) -->
  "-",
  blanks,
  expr(Class, Exp).

not_expr(Class, not(Exp)) -->
  "not",
  blanks,
  expr(Class, Exp).

if_expr(Class, if_then_else(Exp1, Exp2, Exp3)) -->
  "if",
  blanks,
  expr(Class,Exp1),
  blanks,
  "then",
  blanks,
  expr(Class,Exp2),
  blanks,
  "else",
  blanks,
  expr(Class,Exp3), 
  blanks,
  "endif".

dot_expr(_, Exp1, field(Exp1, FieldName)) -->
  ".",
  blanks,
  ident(FieldName).

arrow_expr(Class, Exp1, Exp2) -->
  "->",
  blanks,
  after_arrow_expr(Class, Exp1, Exp2).

after_arrow_expr(Class, Exp1, Exp2) --> including_expr(Class, Exp1, Exp2).
after_arrow_expr(Class, Exp1, Exp2) --> iterate_expr(Class, Exp1, Exp2).
after_arrow_expr(Class, Exp1, Exp2) --> as_set_expr(Class, Exp1, Exp2).
after_arrow_expr(Class, Exp1, Exp2) --> as_bag_expr(Class, Exp1, Exp2).
after_arrow_expr(Class, Exp1, Exp2) --> as_sequence_expr(Class, Exp1, Exp2).
after_arrow_expr(Class, Exp1, Exp2) --> as_ordered_set_expr(Class, Exp1, Exp2).

including_expr(Class, Exp1, including(Exp1,Exp2)) -->
  "including",
  blanks,
  "(",
  blanks,
  expr(Class,Exp2),
  blanks,
  ")".

as_set_expr(_, Exp, as_set(Exp)) -->
  "asSet",
  blanks,
  "(",
  blanks,
  ")".  

as_bag_expr(_, Exp, as_bag(Exp)) -->
  "asBag",
  blanks,
  "(",
  blanks,
  ")".  

as_sequence_expr(_, Exp, as_sequence(Exp)) -->
  "asSequence",
  blanks,
  "(",
  blanks,
  ")".  

as_ordered_set_expr(_, Exp, as_ordered_set(Exp)) -->
  "asOrderedSet",
  blanks,
  "(",
  blanks,
  ")".  

iterate_expr(Class, Exp1, iterate(Exp1, Var1, Var2, Exp2, Exp3)) -->
  "iterate",
  blanks,
  "(",
  blanks,
  ident(Var1),
  blanks,
  ";",
  blanks,
  ident(Var2),
  blanks,
  "=",
  blanks,
  expr(Class, Exp2),
  blanks,
  "|",
  blanks,
  expr(Class, Exp3),
  blanks,
  ")".  

eq_expr(Class, equals(Exp1, Exp2)) -->
  boolean_expr(Class, Exp1),
  blanks,
  "=",
  blanks,
  expr(Class, Exp2).

boolean_expr(Class,Y) -->
  plus_minus_expr(Class,X),
  blanks,
  rest_boolean_expr(Class, X, Y).   

rest_boolean_expr(_, Exp, Exp) --> [].

rest_boolean_expr(Class, Exp1, and(Exp1, Exp2)) -->
   "and",
   blanks,
   expr(Class, Exp2).

rest_boolean_expr(Class, Exp1, or(Exp1, Exp2)) -->
   "or",
   blanks,
   expr(Class, Exp2).

plus_minus_expr(Class, Y) --> 
  mult_div_expr(Class, X),
  blanks,
  rest_plus_minus_expr(Class, X, Y).

rest_plus_minus_expr(_, Exp, Exp) --> [].

rest_plus_minus_expr(Class, Exp1, plus(Exp1, Exp2)) -->
   "+",
   blanks,
   expr(Class, Exp2).

rest_plus_minus_expr(Class, Exp1, minus(Exp1, Exp2)) -->
   "-",
   blanks,
   expr(Class, Exp2).

mult_div_expr(Class, Y) --> 
   left_expr(Class, X),
   blanks,
   rest_mult_div_expr(Class, X, Y).


rest_mult_div_expr(_, Exp, Exp) --> [].

rest_mult_div_expr(Class, Exp1, mult(Exp1, Exp2)) -->
   "*",
   blanks,
   expr(Class, Exp2).

rest_mult_div_expr(Class, Exp1, divide(Exp1, Exp2)) -->
   "/",
   blanks,
   expr(Class, Exp2).

left_expr(Class, Exp) --> 
   base_expr(Class, Inherit),
   blanks,
   rest_expr(Class, Inherit, Exp).

base_expr(Class, Exp) --> const_expr(Class, Exp).
base_expr(Class, Exp) --> ident_expr(Class, Exp).
base_expr(Class, Exp) --> null_expr(Class, Exp).
base_expr(Class, Exp) --> paren_expr(Class, Exp).
base_expr(Class, Exp) --> all_instances_expr(Class, Exp).
base_expr(Class, Exp) --> not_expr(Class, Exp).
base_expr(Class, Exp) --> if_expr(Class, Exp).
base_expr(Class, Exp) --> change_sign_expr(Class, Exp).

expr(Class, Exp) --> eq_expr(Class, Exp).
expr(Class, Exp) --> boolean_expr(Class, Exp).

rest_expr(Class, Inherited, Result) --> 
   dot_expr(Class, Inherited, Result1),
   rest_expr(Class, Result1, Result).
rest_expr(Class, Inherited, Result) --> 
   arrow_expr(Class, Inherited, Result1),
   rest_expr(Class, Result1, Result).
rest_expr(_, Result, Result) --> [].


% Types in OCL#

type(boolean) -->
  "bool".
type(integer) -->
  "int".
type(any) -->
  "OclAny".
type(class(Name)) -->
  ident(Name).
type(set(Type)) -->
  "Set",
  blanks,
  "(",
  blanks,
  type(Type),
  blanks,
  ")".
type(bag(Type)) -->
  "Bag",
  blanks,
  "(",
  blanks,
  type(Type),
  blanks,
  ")".
type(sequence(Type)) -->
  "Sequence",
  blanks,
  "(",
  blanks,
  type(Type),
  blanks,
  ")".
type(ordered_set(Type)) -->
  "OrderedSet",
  blanks,
  "(",
  blanks,
  type(Type),
  blanks,
  ")".

build_set(Type, Elements, Expr) :-
  reverse(Elements, Reversed),
  add_elements(as_set(null(Type)), Reversed, Expr).
build_bag(Type, Elements, Expr) :-
  reverse(Elements, Reversed),
  add_elements(as_bag(null(Type)), Reversed, Expr).
build_ordered_set(Type, Elements, Expr) :-
  reverse(Elements, Reversed),
  add_elements(as_ordered_set(null(Type)), Reversed, Expr).
build_sequence(Type, Elements, Expr) :-
  reverse(Elements, Reversed),
  add_elements(as_sequence(null(Type)), Reversed, Expr).

add_elements(Expr, [], Expr).
add_elements(SubExpr, [H|T], Expr) :-
  add_elements(SubExpr, T, Aux),
  Expr = including(Aux, H).

%
% OCL# type checking
%

% Definition of types in OCL#
% 
% A type in OCL# is defined as either
% - A base type
%    - int
%    - boolean
%    - any
%    - Class;
% - A ctype defined as 
%    - ctype(Member, Low, Up, Unique, Ordered)
%    - Low: minimum multiplicity
%    - Up: maximum multiplicity
%    - Unique: whether order is relevant
%    - Ordered: whether order is relevant

% Check if a type is a base type or a ctype
scalar(integer).
scalar(boolean).
scalar(any).
scalar(class(_)).
scalar(ctype(_, _, _, _, _)).

% Reified version
scalar(integer, 1).
scalar(boolean, 1).
scalar(any, 1).
scalar(class(_), 1).
scalar(ctype(_, _, _, _, _), 0).

% Accessing the components of a ctype
member(ctype(M, _, _, _, _), M).
low(ctype(_, L, _, _, _), L).
upp(ctype(_, _, U, _, _), U).
single(ctype(_, 1, 1, _, _)).
single(ctype(_, L, U, _, _), S) :-
  S is (L*U = 1).
unique(ctype(_, _, _, Un, _), Un).
unique(ctype(_, _, _, 1, _)).
ordered(ctype(_, _, _, _, Ord), Ord).
ordered(ctype(_, _, _, _, 1)).

% Create ctypes for singleton, option and  collection types

singleton_type(MemberType, Type) :- Type = ctype(MemberType, 1, 1, undef, undef).
option_type(MemberType, Type) :- Type = ctype(MemberType, 0, 1, undef, undef).
set_type(MemberType, Type) :- Type = ctype(MemberType, 0, "*", 1, 0).
bag_type(MemberType, Type) :- Type = ctype(MemberType, 0, "*", 0, 0).
sequence_type(MemberType, Type) :- Type = ctype(MemberType, 0, "*", 0, 1).
ordered_set_type(MemberType, Type) :- Type = ctype(MemberType, 0, "*", 1, 1).

nullable_type(integer, _) :- assertion(fail).
nullable_type(boolean, _) :- assertion(fail).
nullable_type(any,     _) :- assertion(fail).
nullable_type(ctype(M, _, U, Un, Ord), ctype(M, 0, U, Un, Ord)).

% Comparing multiplicities

multiplicity_min(X, Y, Min) :- 
   number(X),
   number(Y),
   Min is min(X, Y).
multiplicity_min("*", X, X) :- number(X).
multiplicity_min(X, "*", X) :- number(X).
multiplicity_min("*", "*", "*").

multiplicity_max(X, Y, Min) :- 
   number(X),
   number(Y),
   Min is max(X, Y).
multiplicity_max("*", X, "*") :- number(X).
multiplicity_max(X, "*", "*") :- number(X).
multiplicity_max("*", "*", "*").

multiplicity_add(X, Y, Add) :-
  number(X), 
  number(Y),
  Add is X + Y.
multiplicity_add("*", X, "*") :- number(X).
multiplicity_add(X, "*", "*") :- number(X).
multiplicity_add("*", "*", "*").

multiplicity_mult(X, Y, Mult) :-
  number(X), 
  number(Y),
  Mult is X + Y.
multiplicity_mult("*", 0, 0).
multiplicity_mult(0, "*", 0).
multiplicity_mult("*", X, "*") :- 
  number(X),
  X >= 1.
multiplicity_mult(X, "*", "*") :- 
  number(X),
  X >= 1.
multiplicity_mult("*", "*", "*").

multiplicity_less_eq_than(X, X, 1).
multiplicity_less_eq_than(_, "*", 1).
multiplicity_less_eq_than("*", X, 0) :- number(X).
multiplicity_less_eq_than(X, Y, 1) :- 
  number(X), 
  number(Y),
  (X =< Y).
multiplicity_less_eq_than(X, Y, 0) :- 
  number(X), 
  number(Y),
  (X > Y).

% subtype( SubType, SuperType ) - reified and non-reified versions


less_than_annotation(X, X, 1).
less_than_annotation(undef, _, 1).
less_than_annotation(X, undef, 0) :- number(X).
less_than_annotation(X, Y, 0) :- number(X), number(Y), (X > Y; X < Y).

subtype(Type, Type).
subtype(_, any).
subtype(ctype(T1, L1, U1, Un1, Or1),
        ctype(T2, L2, U2, Un2, Or2)) :-
  subtype(T1, T2),
  L1 >= L2,
  U1 =< U2,
  (Un1 = Un2 ; Un1 = undef ),
  (Or1 = Or2;  Or1 = undef ).

subtype(Type, Type, 1).
subtype(_, any, 1).
%subtype for class names

subtype(boolean, integer, 0).
subtype(integer, boolean, 0).
subtype(boolean, class(_), 0).
subtype(class(_), boolean, 0).
subtype(integer, class(_), 0).
subtype(class(_), integer, 0).
%subtype for class names
subtype(any, integer, 0).
subtype(any, boolean, 0).
subtype(any, class(_), 0).
subtype(any, ctype(_, _, _, _, _), 0).
subtype(ctype(_,_,_,_,_), integer, 0).
subtype(ctype(_,_,_,_,_), boolean, 0).
subtype(ctype(_,_,_,_,_), class(_), 0).
subtype(integer, ctype(_,_,_,_,_), 0).
subtype(boolean, ctype(_,_,_,_,_), 0).
subtype(class(_), ctype(_,_,_,_,_), 0).
subtype(ctype(T1, L1, U1, Un1, Or1),
        ctype(T2, L2, U2, Un2, Or2), 
        IsSubtype) :-
  subtype(T1, T2, S1),
  multiplicity_less_eq_than(L2, L1, Aux1),
  multiplicity_less_eq_than(U1, U2, Aux2),
  less_than_annotation(Un1, Un2, Aux3),
  less_than_annotation(Or1, Or2, Aux4),
  % TODO - Doublecheck the multiplicity/ordering
  IsSubtype is S1 * Aux1 * Aux2 * Aux3 * Aux4.

% Least upper bound

lub_annotation(X, X, X).
lub_annotation(undef, X, X).
lub_annotation(X, undef, X).
lub_annotation(X, Y) :- number(X), number(Y), (X > Y; X < Y), assertion(false).

lub_type(any, _, any).
lub_type(_, any, any).
lub_type(boolean, boolean, boolean).
lub_type(integer, integer, integer).
lub_type(boolean, integer, any).
lub_type(boolean, class(_), any).
lub_type(integer, boolean, any).
lub_type(boolean, class(_), any).
lub_type(class(_), boolean, any).
lub_type(class(_), integer, any).
% lub for class names
lub_type(Type1, Type2, Result) :-
   Type1 = ctype(T1, L1, U1, Un1, Or1),
   Type2 = ctype(T2, L2, U2, _, _),
   lub_type(T1, T2, Lub),
   multiplicity_min(L1, L2, LLub),
   multiplicity_max(U1, U2, ULub),
   % TODO - Doublecheck the multiplicity/ordering
   Result = ctype(Lub, LLub, ULub, Un1, Or1 ).

% Type check for binary expressions

type_check_binary_boolean_expr(Model, Exp1, Exp2, Type) :-
   type_check_ocls(Model, Exp1, Subtype1),
   type_check_ocls(Model, Exp2, Subtype2),
   % Subexpression 1 should be boolean with max multiplicity 1
   assertion(scalar(Subtype1, 0)),
   assertion(member(Subtype1, boolean)),
   assertion(upp(Subtype1, 1)),
   % Subexpression 2 should be boolean with max multiplicity 1
   assertion(scalar(Subtype2, 0)),
   assertion(member(Subtype2, boolean)),
   assertion(upp(Subtype2, 1)),
   low(Subtype1, Low1),
   low(Subtype2, Low2),
   Low is min(Low1, Low2),
   Type = ctype(boolean, Low, 1, undef, undef),
   write('- and: '), write(Exp1), write(' , '),  write(Exp2), nl, 
   write('  Type: '), write(Type), nl.

type_check_binary_integer_expr(Model, Exp1, Exp2, Type) :-
   type_check_ocls(Model, Exp1, Subtype1),
   type_check_ocls(Model, Exp2, Subtype2),
   % Subexpression 1 should be integer with max multiplicity 1
   assertion(scalar(Subtype1, 0)),
   assertion(member(Subtype1, integer)),
   assertion(upp(Subtype1, 1)),
   % Subexpression 2 should be integer with max multiplicity 1
   assertion(scalar(Subtype2, 0)),
   assertion(member(Subtype2, integer)),
   assertion(upp(Subtype2, 1)),
   low(Subtype1, Low1),
   low(Subtype2, Low2),
   Low is min(Low1, Low2),
   Type = ctype(integer, Low, 1, undef, undef),
   write('- Op: '), write(Exp1), write(' , '),  write(Exp2), nl, 
   write('  Type: '), write(Type), nl.

% Type casts (asSet / asBag / asSequence / asOrderedSet)

type_check_as_collection_expr(_, Subtype, Type, Un, Ord) :-
  scalar(Subtype, 1),  
  Type = ctype(Subtype, 0, "*", Un, Ord).

type_check_as_collection_expr(_, Subtype, Type, Un, Ord) :-
  scalar(Subtype, 0),
  member(Subtype, M),  
  Type = ctype(M, 0, "*", Un, Ord).

% Type checking type declarations

type_check_ocls(_, any, any).
type_check_ocls(_, boolean, boolean).
type_check_ocls(_, integer, integer).
type_check_ocls(_, class(Classname), class(Classname)).
type_check_ocls(Model, set(T), X) :-
  type_check_ocls(Model, T, MemberType),
  set_type(MemberType, X).
type_check_ocls(Model, bag(T), X) :-
  type_check_ocls(Model, T, MemberType),
  bag_type(MemberType, X).
type_check_ocls(Model, sequence(T), X) :-
  type_check_ocls(Model, T, MemberType),
  sequence_type(MemberType, X).
type_check_ocls(Model, ordered_set(T), X) :-
  type_check_ocls(Model, T, MemberType),
  ordered_set_type(MemberType, X).

% Basic type checking
type_check_ocls(_, spec([]), _).

type_check_ocls(Model, spec([H|T]), _) :- 
   type_check_ocls(Model, H, _),
   type_check_ocls(Model, spec(T), _).

type_check_ocls(Model, invariant(Class, Body), _) :- 
   write('* Invariant of class '), write(Class), nl, nl,
   type_check_ocls(Model, Body, Type),
   % Body should be boolean with max multiplicity 1
   assertion(scalar(Type, 0)),
   assertion(member(Type, boolean)),
   assertion(upp(Type, 1)),
   write('- Body: '), write(Body), nl, 
   write('  Type: '), write(Type), nl, nl.

type_check_ocls(_, int_const(I), Type) :-
   %TODO: Testing nullables
   %option_type(integer, Type),
   singleton_type(integer, Type),
   write('- Integer constant '), write(I), nl, write('  Type: '), write(Type), nl.

type_check_ocls(_, bool_const(B), Type) :-
   %TODO: Testing nullables
   %option_type(boolean, Type),
   singleton_type(boolean, Type),
   write('- Boolean constant '), write(B), nl, write('  Type: '), write(Type), nl.

type_check_ocls(_, self(Classname), Type) :-
   singleton_type(class(Classname), Type).

type_check_ocls(Model, null(T), Type) :-
   type_check_ocls(Model, T, Subtype),
   option_type(Subtype, Type),
   write('- Base type for no: '), write(T), nl,
   write('- Output type for no: '), write(Type), nl.

type_check_ocls(_, all_instances(Classname), Type) :-
   set_type(class(Classname), Type).

type_check_ocls(Model, not(Exp), Type) :-
   type_check_ocls(Model, Exp, Subtype),
   % Subexpression should be boolean with max multiplicity 1
   assertion(scalar(Subtype, 0)),
   assertion(member(Subtype, boolean)),
   assertion(upp(Subtype, 1)),
   low(Subtype, Low),
   Type = ctype(boolean, Low, 1, undef, undef),
   write('- Not '), write(Exp), nl, write('  Type: '), write(Type), nl.

type_check_ocls(Model, negative(Exp), Type) :-
   type_check_ocls(Model, Exp, Subtype),
   % Subexpression should be integer with max multiplicity 1
   assertion(scalar(Subtype, 0)),
   assertion(member(Subtype, integer)),
   assertion(upp(Subtype, 1)),
   low(Subtype, Low),
   Type = ctype(integer, Low, 1, undef, undef),
   write('- Minus '), write(Exp), nl, write('  Type: '), write(Type), nl.

type_check_ocls(Model, and(Exp1, Exp2), Type) :-
   type_check_binary_boolean_expr(Model, Exp1, Exp2, Type).
type_check_ocls(Model, or(Exp1, Exp2), Type) :-
   type_check_binary_boolean_expr(Model, Exp1, Exp2, Type).
  
type_check_ocls(Model, plus(Exp1, Exp2), Type) :-
  type_check_binary_integer_expr(Model, Exp1, Exp2, Type).
type_check_ocls(Model, minus(Exp1, Exp2), Type) :-
  type_check_binary_integer_expr(Model, Exp1, Exp2, Type).
type_check_ocls(Model, mult(Exp1, Exp2), Type) :-
  type_check_binary_integer_expr(Model, Exp1, Exp2, Type).
type_check_ocls(Model, divide(Exp1, Exp2), Type) :-
  type_check_binary_integer_expr(Model, Exp1, Exp2, Type).

type_check_ocls(Model, as_set(Exp), Type) :-
  type_check_ocls(Model, Exp, Subtype),
  % Unique, Non-ordered
  type_check_as_collection_expr(Exp, Subtype, Type, 1, 0).

type_check_ocls(Model, as_bag(Exp), Type) :-
  type_check_ocls(Model, Exp, Subtype),
  % Non-unique, Non-ordered
  type_check_as_collection_expr(Exp, Subtype, Type, 0, 0).

type_check_ocls(Model, as_sequence(Exp), Type) :-
  type_check_ocls(Model, Exp, Subtype),
  % Non-unique, Ordered
  type_check_as_collection_expr(Exp, Subtype, Type, 0, 1).

type_check_ocls(Model, as_ordered_set(Exp), Type) :-
  type_check_ocls(Model, Exp, Subtype),
  % Unique, Ordered
  type_check_as_collection_expr(Exp, Subtype, Type, 1, 1).

type_check_ocls(Model, if_then_else(Exp1, Exp2, Exp3), Type) :-
  type_check_ocls(Model, Exp1, Type1),
  type_check_ocls(Model, Exp2, Type2),
  type_check_ocls(Model, Exp3, Type3),
  % Condition should be a boolean with maximum multiplicity 1
  assertion(scalar(Type1, 0)),
  assertion(member(Type1, boolean)),
  assertion(upp(Type1, 1)),
  % No conditions on Exp2 or Exp3 
  lub_type(Type2, Type3, Type),
  write('- if then else type: '), write(Type), nl.

type_check_ocls(Model, equals(Exp1, Exp2), Type) :-
  type_check_ocls(Model, Exp1, Type1),
  type_check_ocls(Model, Exp2, Type2),
  % Either Type1 is a subtype of Type2 or vice-versa
  subtype(Type1, Type2, IsSubtype1),
  subtype(Type2, Type1, IsSubtype2),
  assertion(IsSubtype1 + IsSubtype2 >= 1),
  % Result is always a boolean type 
  singleton_type(boolean, Type).


%type_check_ocls(field(Exp1, FieldName), Type) :-

type_check_ocls(Model, ident(String), Type) :-
  lookup_variable( Model, String, Type, _ ).

type_check_ocls(Model, including(Exp1,Exp2), Type) :-
  type_check_ocls(Model, Exp1, Type1),
  type_check_ocls(Model, Exp2, Type2),
  % e1 cannot be an scalar
  assertion(scalar(Type1, 0)),
  Type1 = ctype(M1, L1, U1, Un1, Ord1),
  % e2 cannot be a scalar either
  assertion(scalar(Type2, 0)),
  Type2 = ctype(M2, L2, U2, _, _),
  % Define the resulting type (it has the same uniqueness/ordering as Type1)
  Type = ctype(M1, L, U, Un1, Ord1),
  % Either Type2 is a collection of elements of type member(Type1)
  % or it is an element of type member(Type1)
  (  
    ( subtype(Type2, M1, 1) ->
    multiplicity_add(U1, 1, U),
    ( Un1 = 1 -> L = L1; Un1 = 0 -> multiplicity_add(L1, 1, L)))
  ;
    ( % subtype(Type2, Type1, 1) ->
    subtype(M2, M1, 1)->  
    %Type2 = ctype(M2, L2, U2, _, _),
    multiplicity_add(U1, U2, U),
    ( Un1 = 1 -> L = L1; Un1 = 0 -> multiplicity_add(L1, L2, L)))
  ; write('Type2 '), write(Type2), nl,
    write('Type1 '), write(Type1), nl,
    assertion( false )
  ).

type_check_ocls(Model, iterate(Exp1, Var1, Var2, Exp2, Exp3), Type) :-
  
  % Check value of the initialization expression
  type_check_ocls(Model, Exp2, Type2),
  %TODO: Testing nullables
  %declare_variable(Model, Var2, Type2, _, AuxModel1),
  nullable_type(Type2, NullableType2),
  declare_variable(Model, Var2, NullableType2, _, AuxModel1),
  
  % Exp1 should not be a scalar
  type_check_ocls(Model, Exp1, Type1),
  assertion(scalar(Type1, 0)),
  member(Type1, MemberType),

  ( scalar(MemberType) ->
    write('Member type '), atom_string(MemberType,X), write(X), nl,
    singleton_type(MemberType, SingletonType),
    declare_variable(AuxModel1, Var1, SingletonType, _, AuxModel2)
  ; declare_variable(AuxModel1, Var1, MemberType, _, AuxModel2)
  ),

  % Check types of the accumulated expression
  type_check_ocls(AuxModel2, Exp3, Type3),
  % Type3 should be a subtype of Type2
  assertion(subtype(Type2, Type3, 1)),
  Type = Type2.

% Missing type checks
% - variable
% - .p


%
% OCL# expression evaluation
%
% eval_ocls(Tree, Result).


% Add elements to the different types of collections

collection_add(Type, Col, Elem, Result) :-
  Type = ctype(_, _, _, Un, Ord),
  ( Un = 1, Ord = 0 -> set_add( Col, Elem, Result )
  ; Un = 0, Ord = 0 -> bag_add( Col, Elem, Result )
  ; Un = 0, Ord = 1 -> sequence_add( Col, Elem, Result )
  ; Un = 1, Ord = 1 -> ordered_set_add( Col, Elem, Result )
  ).

collection_add_all(Type, Col1, Col2, Result) :-
  Type = ctype(_, _, _, Un, Ord),
  ( Un = 1, Ord = 0 -> set_add_all( Col1, Col2, Result )
  ; Un = 0, Ord = 0 -> bag_add_all( Col1, Col2, Result )
  ; Un = 0, Ord = 1 -> sequence_add_all( Col1, Col2, Result )
  ; Un = 1, Ord = 1 -> ordered_set_add_all( Col1, Col2, Result )
  ).

set_add( [], Elem , Result ) :- 
  Result = [ Elem ].
set_add( Set, Elem, Result ) :-
  ( lists:member(Elem, Set) ->
    Result = Set
  ; 
    append( Set, [Elem], Result )
  ).

set_add_all( Col1, [], Result ) :- 
  Result = Col1.
set_add_all( Col1, [Elem|Rest], Result ) :-
  set_add( Col1, Elem, Aux ),
  set_add_all( Aux, Rest, Result ). 

bag_add( [], Elem, Result ) :-
  Result = [ Elem ].
bag_add( Bag, Elem, Result ) :-
  Result = [ Elem | Bag ].

bag_add_all( Col1, [], Result ) :- 
  Result = Col1.
bag_add_all( Col1, [Elem|Rest], Result ) :-
  bag_add( Col1, Elem, Aux ),
  bag_add_all( Aux, Rest, Result ). 

sequence_add( [], Elem, Result ) :-
  Result = [ Elem ].
sequence_add( Seq, Elem, Result ) :-
  append( Seq, [ Elem ], Result ).

sequence_add_all( Col1, [], Result ) :- 
  Result = Col1.
sequence_add_all( Col1, [Elem|Rest], Result ) :-
  sequence_add( Col1, Elem, Aux ),
  sequence_add_all( Aux, Rest, Result ). 

ordered_set_add( [], Elem, Result ):-
  Result = [ Elem ].
ordered_set_add( OSet, Elem, Result ):-
  ( lists:member(Elem, OSet) ->
    Result = OSet 
  ;
    append( OSet, [ Elem ], Result )
  ).

ordered_set_add_all( Col1, [], Result ) :- 
  Result = Col1.
ordered_set_add_all( Col1, [Elem|Rest], Result ) :-
  ordered_set_add( Col1, Elem, Aux ),
  ordered_set_add_all( Aux, Rest, Result ). 

% Make a member into a set

make_set( List , Set ) :- list_to_set(List, Set).

% Normalize a given collection

normalize(Exp, Model, Result) :-
  eval_ocls(Exp, Model, Value),
  type_check_ocls(Model, Exp, Type),
  normalize_values(Value, Type, Result).


normalize_values([],_,[]).
normalize_values([Value|Rest], Type, Result) :-
  member(Type, MemberType),
  normalize_single_value(Value, MemberType, NormValue),
  normalize_values(Rest, Type, RestNormValue),
  Aux = [ NormValue | RestNormValue],
  % Collections where the order of insertions matter should not be changed
  % Other collections should have their orders sorted lexicographically
  ( ordered(Type) ->
    Result = Aux
  ; 
    msort(Aux, Result)
  ).  

normalize_single_value(Value, Type, Result) :-
  % Elements in a collection can be nested
  % Check if they are scalar (we are done!) or
  % if this value is a nested collection (it must be recursively normalized)
  ( scalar(Type) -> 
    Result = Value
  ;  
    normalize_values(Value, Type, Result)
  ).

eval_ocls( null(_), _, [] ).
eval_ocls( int_const(Value),  _,  [ Value ] ).
eval_ocls( bool_const(Value), _, [ Value ] ).

eval_ocls( not(Exp), Model, [ Result ] ):- 
  eval_ocls(Exp, Model, Value),
  ( Value = [] ->
    Result = []
    ;
    Value = [BoolValue],
    Result is 1-BoolValue 
    ).

eval_ocls( negative(Exp), Model, [ Result ] ):- 
  eval_ocls(Exp, Model, Value),
  ( Value = [] ->
    Result = []
    ;
    Value = [IntValue],
    Result is -IntValue 
    ).

eval_ocls( and(Exp1, Exp2), Model,[ Result ] ):- 
  eval_ocls(Exp1, Model, Value1),
  eval_ocls(Exp2, Model, Value2),
  ( Value1 = [] ->
    Result = []
    ;
    Value2 = [] ->
    Result = []
    ;
    Value1 = [BoolValue1],
    Value2 = [BoolValue2],
    Result is min(BoolValue1, BoolValue2) 
    ).

eval_ocls( or(Exp1, Exp2), Model, [ Result ] ):- 
  eval_ocls(Exp1, Model, Value1),
  eval_ocls(Exp2, Model, Value2),
  ( Value1 = [] -> 
    Result = []
    ;
    Value2 = [] ->
    Result = []
    ;
    Value1 = [BoolValue1],  
    Value2 = [BoolValue2],
    Result is max(BoolValue1, BoolValue2)    
  ).

eval_ocls( plus(Exp1, Exp2), Model, [ Result ] ):- 
  eval_ocls(Exp1, Model, Value1),
  eval_ocls(Exp2, Model, Value2),
  ( Value1 = [] -> 
    Result = []
    ;
    Value2 = [] ->
    Result = []
    ;
    Value1 = [IntValue1],
    Value2 = [IntValue2],
    Result is IntValue1 + IntValue2 
    ).


eval_ocls( minus(Exp1, Exp2), Model, [ Result ] ):- 
  eval_ocls(Exp1, Model, Value1),
  eval_ocls(Exp2, Model, Value2),
  ( Value1 = [] -> 
    Result = []
    ;
    Value2 = [] ->
    Result = []
    ;
    Value1 = [IntValue1],
    Value2 = [IntValue2],
    Result is IntValue1 - IntValue2 
    ).


eval_ocls( mult(Exp1, Exp2), Model, [ Result ] ):- 
  eval_ocls(Exp1, Model, Value1),
  eval_ocls(Exp2, Model, Value2),
  ( Value1 = [] -> 
    Result = []
    ;
    Value2 = [] ->
    Result = []
    ;
    Value1 = [IntValue1],
    Value2 = [IntValue2],
    Result is IntValue1 * IntValue2 
    ).


eval_ocls( divide(Exp1, Exp2), Model, [ Result ] ):- 
  eval_ocls(Exp1, Model, Value1),
  eval_ocls(Exp2, Model, Value2),
  ( Value1 = [] -> 
    Result = []
    ;
    Value2 = [] ->
    Result = []
    ;
    Value1 = [IntValue1],
    Value2 = [IntValue2],
    Result is div(IntValue1, IntValue2) 
    ).

eval_ocls( if_then_else(Exp1, Exp2, Exp3), Model, Result ) :-
  eval_ocls(Exp1, Model, Value1),
  ( Value1 = [BoolValue1],
    BoolValue1 = 1 ->
    eval_ocls(Exp2, Model, Value2),
    Result = Value2
  ; Value1 = [BoolValue1],
    BoolValue1 = 0 ->
    eval_ocls(Exp3, Model, Value3),
    Result = Value3
  ; Value1 = [] ->
    Result = []
  ).
  
eval_ocls( as_set(Exp), Model, Result ) :-
  eval_ocls(Exp, Model, Value),
  make_set(Value, Result).

eval_ocls( as_sequence(Exp), Model, Result ) :-
  eval_ocls(Exp, Model, Value),
  Result = Value.

eval_ocls( as_ordered_set(Exp), Model, Result ) :-
  eval_ocls(Exp, Model, Value),
  make_set(Value, Result).

eval_ocls( as_bag(Exp), Model, Result ) :-
  eval_ocls(Exp, Model, Value),
  Result = Value.

eval_ocls( including(Exp1, Exp2), Model, Result ) :-
  eval_ocls(Exp1, Model, Value1),
  eval_ocls(Exp2, Model, Value2),
  type_check_ocls(Model, Exp1, Type1),
  type_check_ocls(Model, Exp2, Type2),
  member(Type1, MemberType),
  % Check if we are adding a single element or several
  ( subtype(Type2, MemberType) ->
    % Adding a single element  
    collection_add(Type1, Value1, Value2, Result)
  ;
    % Adding all elements of a collection
    collection_add_all(Type1, Value1, Value2, Result)
  ).

eval_ocls( equals(Exp1, Exp2), Model, Result ) :-
  normalize(Exp1, Model, Norm1),
  normalize(Exp2, Model, Norm2),
  write('First  '), write(Norm1), nl,
  write('Second '), write(Norm2), nl,
  ( Norm1 = Norm2 -> 
    Result = [1]
  ; Result = [0] 
  ).

eval_ocls( iterate(Exp1, Var1, Var2, Exp2, Exp3), Model, Result ) :-
  % Get the types of the 
  type_check_ocls(Model, Exp1, ColType),
  member(ColType, MemberType),
  %TODO: Testing nullables
  %type_check_ocls(Model,Exp2, AccType),
  
  type_check_ocls(Model,Exp2, AuxType),
  nullable_type(AuxType, AccType),

  % Evaluate the collection
  eval_ocls(Exp1, Model, Col),

  % Initialize the accumulator 
  eval_ocls(Exp2, Model, FirstValue),

  % Start the iterate over all elements of the collection  
  eval_iterate(Col, Var1, MemberType, Var2, AccType, FirstValue, Exp3, Model, Result ).

eval_ocls( ident(Var), Model, Result ) :-
  lookup_variable(Model, Var, _, Result).

% all_instances
% self 
% invariant

% Empty collection - The result is the current value of the iterator
eval_iterate( [], _, _, _, _, CurrentValue, _, _, CurrentValue ).

% Non-empty collection 
% - Initialize the iterator variable with the current value
% - Evaluate the accumulator expression
% - Evaluate the rest of the list recursively, using the value of the accumulator as the 
%   new current value
% - Retrieve the value of the accumulator at the end of the collection - this is the result
eval_iterate( [ Elem | Rest ], ElemVar, ElemType, AccVar, AccType, CurrentValue, AccExp, 
              Model, Result ) :-
  % ElemVar is set to Elem
  ( scalar(ElemType, 1) ->
    singleton_type(ElemType, SingletonType),
    declare_variable(Model, ElemVar, SingletonType, [ Elem ], AuxModel1)    
  ; 
    declare_variable(Model, ElemVar, ElemType, Elem, AuxModel1)
  ),

  % Initialize the accumulator variable with the current value,
  declare_variable(AuxModel1, AccVar, AccType, CurrentValue, AuxModel2),

  eval_ocls( AccExp, AuxModel2, NewValue ),

  % Retrieve the value of the accumulator at the end of the collection - this is the result
  eval_iterate( Rest, ElemVar, ElemType, AccVar, AccType, NewValue, AccExp, Model, Result ).

declare_variable(Model, Var, Type, Value, NewModel) :-
  NewModel = [ var(Var, Type, Value) | Model ].

lookup_variable([], _, _, _) :- assertion(false).
lookup_variable( [var(Ident, VarType, Value) | Rest], Var, Type, Result ) :-
  ( Var = Ident -> 
    Result = Value,
    Type = VarType
  ; lookup_variable(Rest, Var, Type, Result)
  ).



  
  

    


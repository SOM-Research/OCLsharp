:- set_prolog_flag(double_quotes, chars).
:- use_module(library(dcg/basics)).

% OCL core (OCL-) module

% -- INTERFACE---------------------------------------------------------------

%
% OCL- parser - parsing OCL- specifications
%
% parse_oclc( Spec, Tree )
% 
% Parse an OCL specification stored in a string. No type checking is perfomed 
% at this stage.
% - Spec: (Input) OCL- Specification, between double quotes.
% - Tree: (Output) Parse tree of the specification.
% 
% 
% Example:
% ?- parse_ocls( "true" , X)
% X = spec( [bool_const(1)] )

parse_oclc(Spec, Tree) :-
   ( phrase(spec(Tree), Spec) ->
     !
   ; write('Error: Parse error reading - '),
     atom_string(X, Spec),
     write(X), nl,
     fail
   ).

%
% OCL- expression parser - parsing an isolated expression
%
% parse_oclc_expr( ExprSpec, Tree )
% 
% Parse an OCL- expression stored in a string. The method builds a fake invariant
% around the expression before passing it to the parser.
% No type checking is perfomed at this stage.
% - ExprSpec: (Input) An OCL- expression, between double quotes.
% - Tree: (Output) Parse tree of the expression.
% 
% 
% Example:
% ?- parse_oclc_expr( "true" , X)
% X = bool_const(1)

parse_oclc_expr( Spec, Tree ) :-
  parse_oclc( Spec, spec([Tree]) ).

%
% OCL- type-checker - checking a specification in a parse tree
%
% type_check_oclc( Model, Tree, Type )
% 
% Compute the type of an expression OCL- (stored as a parse tree) and check 
% the types of subexpressions.
% - Tree   (Input)  Parse tree of the specification.
% - Type:  (Output) Type of the expression.
% 
% Example:
% ?- type_check_oclc( plus(int_const(1), int_const(2))] , X)
% X = ctype(integer, 0, 1, undef, undef)

type_check_oclc( Expr, Type ) :- 
  ( type_check_oclc([], Expr, TheType) ->
    Type = TheType
  ; write('Error: Type error in expression - '),
    write(Expr), nl,
    fail
  ).

%
% OCL- type-checker - checking a OCL- expression in a string
%
% type_check_oclc_expr( Spec, Type )
% 
% Compute the type of an expression OCL- (stored as a string) and check the 
% types of subexpressions. 
% The method builds a fake invariant around the expression before passing it 
% to the parser. As a result, type checking will fail for expressions 
% including the "self" keyword.
% - Model: (Input) List of variable definitions ([] if there are none).
% - Spec:  (Input) OCL- Specification, between double quotes.
% - Tree:  (Output) Parse tree of the specification.
% 
% Example:
% ?- type_check_oclc_expr( "1+2" , X)
% X = ctype(integer, 0, 1, undef, undef)

type_check_oclc_expr( Spec, Type ) :-
  parse_oclc_expr(Spec, Tree),
  type_check_oclc(Tree, Type).

%
% OCL- evaluator - evaluate a specification in a parse tree
%
% eval_oclc( ExprTree, Model, Result )
% 
% Evaluate an expression (provided as a parse tree) and compute the result.
% - Tree:  (Input)  Parse tree of the expression to be evaluated.
% - Model: (Input)  List of variable definitions ([] if there are none).
% - Result:(Output) Value of the result of the expression.
% 
% Example:
% ?- eval_oclc( plus(int_const(1), int_const(2)), [] , X)
% X = [3]

%
% OCL# evaluator - from a String
%
% eval_oclc_expr( Spec, Result )
% 
% Evaluate an expression (stored in a String) and compute the result.
% - Spec:  (Input)  OCL- Specification, between double quotes.
% - Result:(Output) Value of the result of the expression.
% 
% Example:
% ?- eval_oclc_expr( "1+2", X)
% X = [3]

eval_oclc_expr( Spec, Type ) :-
  write("Parsing "),
  write(Spec), nl,
  parse_oclc_expr(Spec, Tree),
  eval_oclc(Tree, [], Type).

% -- IMPLEMENTATION --------------------------------------------------------

%
% OCL- spec 
%

% A OCL- specification is a list of expressions

spec(spec([Expr|Rest])) -->
   expr(_, Expr),
   blanks,
   spec(spec(Rest)).
spec(spec([])) -->
   blanks.

% Identifier in OCL-

ident(String) --> 
   string_without(" \t\n\r.,-+*()><=:;|[]?", StringCodes),
   { nth0(0, StringCodes, _),
     string_codes(String, StringCodes) }.

% Constant expressions

const_expr(_, bool_const(1)) --> 
   "true".

const_expr(_, bool_const(0)) --> 
   "false".

const_expr(_, int_const(X)) -->
  integer(X).

ident_expr(_, ident(Name)) -->
  ident(Name).

paren_expr(Class, Exp) -->
%paren_expr(Class, paren(Exp)) -->
  "(",
  blanks,
  expr(Class, Exp),
  blanks,
  ")".

all_expr(_, all(Class)) --> 
   "all",
   blanks,
   ident(Class).

null_expr(_, null(Type)) -->
   "no",
   blanks,
   type(Type).

%change_sign_expr(Class, negative(Exp)) -->
%  "-",
%  blanks,
%  expr(Class, Exp).

%not_expr(Class, not(Exp)) -->
%  "not",
%  blanks,
%  expr(Class, Exp).

if_expr(Class, Inherited, if_then_else(Inherited, Exp2, Exp3)) -->
  "?",
  blanks,
  expr(Class,Exp2),
  blanks,
  ":",
  blanks,
  expr(Class,Exp3).

as_expr(_, Inherited, as_type(Inherited, Type)) -->
   "as",
   blanks,
   type(Type).

dot_expr(_, Inherited, property(Inherited, FieldName)) -->
  ".",
  blanks,
  ident(FieldName).

merge_expr(Class, Inherited, merge(Inherited, Exp2)) -->
  "merge",
  blanks,
  expr(Class, Exp2). 

lift_expr(_, Inherited, lift(Inherited)) -->
  blanks,
  "lift".

lower_expr(_, Inherited, lower(Inherited)) -->
  blanks,
  "lower".

iterate_expr(Class, Inherited, iterate(Inherited, Var1, Var2, Exp2, Exp3)) -->
  "->",
  blanks,
  "[",
  blanks,
  ident(Var1),
  blanks,
  "|",
  blanks,
  ident(Var2),
  blanks,
  "<-",
  blanks,
  expr(Class, Exp2),
  blanks,
  "|",
  blanks,
  expr(Class, Exp3),
  blanks,
  "]".

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

rest_boolean_expr(Class, Exp1, exclusive_or(Exp1, Exp2)) -->
   "xor",
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
base_expr(Class, Exp) --> all_expr(Class, Exp).
base_expr(Class, Exp) --> null_expr(Class, Exp).
base_expr(Class, Exp) --> ident_expr(Class, Exp).
base_expr(Class, Exp) --> paren_expr(Class, Exp).

expr(Class, Exp) --> eq_expr(Class, Exp).
expr(Class, Exp) --> boolean_expr(Class, Exp).

rest_expr(Class, Inherited, Result) --> 
   as_expr(Class, Inherited, Result1),
   rest_expr(Class, Result1, Result).
rest_expr(Class, Inherited, Result) --> 
   dot_expr(Class, Inherited, Result1),
   rest_expr(Class, Result1, Result).
rest_expr(Class, Inherited, Result) -->
   merge_expr(Class, Inherited, Result1),
   rest_expr(Class, Result1, Result).
rest_expr(Class, Inherited, Result) -->
   lift_expr(Class, Inherited, Result1),
   rest_expr(Class, Result1, Result).
rest_expr(Class, Inherited, Result) -->
   lower_expr(Class, Inherited, Result1),
   rest_expr(Class, Result1, Result).
rest_expr(Class, Inherited, Result) -->
   if_expr(Class, Inherited, Result1),
   rest_expr(Class, Result1, Result).   
rest_expr(Class, Inherited, Result) -->
   iterate_expr(Class, Inherited, Result1),
   rest_expr(Class, Result1, Result).   
rest_expr(_, Result, Result) --> [].

% Types in OCL#

type(boolean) -->
  "bool".
type(integer) -->
  "int".
type(any) -->
  "any".
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
  "Seq",
  blanks,
  "(",
  blanks,
  type(Type),
  blanks,
  ")".
type(ordered_set(Type)) -->
  "OSet",
  blanks,
  "(",
  blanks,
  type(Type),
  blanks,
  ")".

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

oclc_type_check_binary_boolean_expr(Model, Exp1, Exp2, Type) :-
   type_check_oclc(Model, Exp1, Subtype1),
   type_check_oclc(Model, Exp2, Subtype2),
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

oclc_type_check_binary_integer_expr(Model, Exp1, Exp2, Type) :-
   type_check_oclc(Model, Exp1, Subtype1),
   type_check_oclc(Model, Exp2, Subtype2),
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

oclc_type_check_as_collection_expr(_, Subtype, Type, Un, Ord) :-
  scalar(Subtype, 1),  
  Type = ctype(Subtype, 0, "*", Un, Ord).

oclc_type_check_as_collection_expr(_, Subtype, Type, Un, Ord) :-
  scalar(Subtype, 0),
  member(Subtype, M),  
  Type = ctype(M, 0, "*", Un, Ord).

% Type checking type declarations

type_check_oclc(_, any, any).
type_check_oclc(_, boolean, boolean).
type_check_oclc(_, integer, integer).
type_check_oclc(_, class(Classname), class(Classname)).
type_check_oclc(Model, set(T), X) :-
  type_check_oclc(Model, T, MemberType),
  set_type(MemberType, X).
type_check_oclc(Model, bag(T), X) :-
  type_check_oclc(Model, T, MemberType),
  bag_type(MemberType, X).
type_check_oclc(Model, sequence(T), X) :-
  type_check_oclc(Model, T, MemberType),
  sequence_type(MemberType, X).
type_check_oclc(Model, ordered_set(T), X) :-
  type_check_oclc(Model, T, MemberType),
  ordered_set_type(MemberType, X).

% Basic type checking
type_check_oclc(_, spec([]), _).

type_check_oclc(Model, spec([H|T]), _) :- 
   type_check_oclc(Model, H, Type),
   % Outermost expression should be boolean with max multiplicity 1
   assertion(scalar(Type, 0)),
   assertion(member(Type, boolean)),
   assertion(upp(Type, 1)),
   write('- Expression: '), write(H), nl, 
   write('  Type: '), write(Type), nl, nl,
   type_check_oclc(Model, spec(T), _).

type_check_oclc(_, int_const(I), Type) :-
   singleton_type(integer, Type),
   write('- Integer constant '), write(I), nl, write('  Type: '), write(Type), nl.

type_check_oclc(_, bool_const(B), Type) :-
   singleton_type(boolean, Type),
   write('- Boolean constant '), write(B), nl, write('  Type: '), write(Type), nl.

type_check_oclc(Model, null(T), Type) :-
   type_check_oclc(Model, T, Subtype),
   option_type(Subtype, Type),
   write('- Base type for no: '), write(T), nl,
   write('- Output type for no: '), write(Type), nl.

type_check_oclc(_, all(Classname), Type) :-
   set_type(class(Classname), Type).

type_check_oclc(Model, and(Exp1, Exp2), Type) :-
   oclc_type_check_binary_boolean_expr(Model, Exp1, Exp2, Type).
type_check_oclc(Model, or(Exp1, Exp2), Type) :-
   oclc_type_check_binary_boolean_expr(Model, Exp1, Exp2, Type).
type_check_oclc(Model, exclusive_or(Exp1, Exp2), Type) :-
   oclc_type_check_binary_boolean_expr(Model, Exp1, Exp2, Type).   
  
type_check_oclc(Model, plus(Exp1, Exp2), Type) :-
  oclc_type_check_binary_integer_expr(Model, Exp1, Exp2, Type).
type_check_oclc(Model, minus(Exp1, Exp2), Type) :-
  oclc_type_check_binary_integer_expr(Model, Exp1, Exp2, Type).
type_check_oclc(Model, mult(Exp1, Exp2), Type) :-
  oclc_type_check_binary_integer_expr(Model, Exp1, Exp2, Type).
type_check_oclc(Model, divide(Exp1, Exp2), Type) :-
  oclc_type_check_binary_integer_expr(Model, Exp1, Exp2, Type).

%type_check_oclc(Model, as_type(Exp, class(X)), Type) :-
%  format('Type:~w~n', [X]),
%  atom_chars(X, AB),
%  format('Type:~w~n', [AB]),
%  format('Type:~w~n', ['Set']),
% compare(Z,X,"Set"),
% format('Type:~w~n', ["Set"]),
% compare(W,AB,"Set"),
%  format('Type:~w~n', [W]),
%  atom_codes(X, Y),
%  format('Type:~w~n', [Y]),
%  type_check_oclc(Model, Exp, Subtype),
% oclc_type_check_as_collection_expr(Exp, Subtype, Type, 1, 0).

type_check_oclc(Model, as_type(Exp, class(X)), Type) :-
  atom_chars(X, "Set"),
  type_check_oclc(Model, Exp, Subtype),
  oclc_type_check_as_collection_expr(Exp, Subtype, Type, 1, 0).

type_check_oclc(Model, as_type(Exp, class(X)), Type) :-
  atom_chars(X, "Seq"),
  type_check_oclc(Model, Exp, Subtype),
  oclc_type_check_as_collection_expr(Exp, Subtype, Type, 0, 1).

type_check_oclc(Model, as_type(Exp, class(X)), Type) :-
  atom_chars(X, "Bag"),
  type_check_oclc(Model, Exp, Subtype),
  oclc_type_check_as_collection_expr(Exp, Subtype, Type, 0, 0).

type_check_oclc(Model, as_type(Exp, class(X)), Type) :-
  atom_chars(X, "OSet"),
  type_check_oclc(Model, Exp, Subtype),
  oclc_type_check_as_collection_expr(Exp, Subtype, Type, 1, 1).

type_check_oclc(Model, if_then_else(Exp1, Exp2, Exp3), Type) :-
  type_check_oclc(Model, Exp1, Type1),
  type_check_oclc(Model, Exp2, Type2),
  type_check_oclc(Model, Exp3, Type3),
  % Condition should be a boolean with maximum multiplicity 1
  assertion(scalar(Type1, 0)),
  assertion(member(Type1, boolean)),
  assertion(upp(Type1, 1)),
  % No conditions on Exp2 or Exp3 
  lub_type(Type2, Type3, Type),
  write('- if then else type: '), write(Type), nl.

type_check_oclc(Model, equals(Exp1, Exp2), Type) :-
  type_check_oclc(Model, Exp1, Type1),
  type_check_oclc(Model, Exp2, Type2),
  % Either Type1 is a subtype of Type2 or vice-versa
  subtype(Type1, Type2, IsSubtype1),
  subtype(Type2, Type1, IsSubtype2),
  assertion(IsSubtype1 + IsSubtype2 >= 1),
  % Result is always a boolean type 
  singleton_type(boolean, Type).


%type_check_oclc(property(Exp1, FieldName), Type) :-

type_check_oclc(Model, ident(String), Type) :-
  lookup_variable( Model, String, Type, _ ).

type_check_oclc(Model, merge(Exp1,Exp2), Type) :-
  type_check_oclc(Model, Exp1, Type1),
  type_check_oclc(Model, Exp2, Type2),
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

type_check_oclc(Model, lift(Exp), Type) :-
  % Check the type of the expression being lowered
  type_check_oclc(Model, Exp, Subtype),
  singleton_type(Subtype, Type).

type_check_oclc(Model, lower(Exp), Type) :-
  % Check the type of the expression being lowered
  type_check_oclc(Model, Exp, Subtype),
  % This type cannot be an scalar and must have upper multiplicity equal to 1
  assertion(scalar(Subtype, 0)),
  assertion(upp(Subtype, 1)),
  member(Subtype, Type).

type_check_oclc(Model, iterate(Exp1, Var1, Var2, Exp2, Exp3), Type) :-
  
  % Check value of the initialization expression
  type_check_oclc(Model, Exp2, Type2),
  nullable_type(Type2, NullableType2),
  declare_variable(Model, Var2, NullableType2, _, AuxModel1),
  
  % Exp1 should not be a scalar
  type_check_oclc(Model, Exp1, Type1),
  assertion(scalar(Type1, 0)),
  member(Type1, MemberType),

  ( scalar(MemberType) ->
    write('Member type '), atom_string(MemberType,X), write(X), nl,
    singleton_type(MemberType, SingletonType),
    declare_variable(AuxModel1, Var1, SingletonType, _, AuxModel2)
  ; declare_variable(AuxModel1, Var1, MemberType, _, AuxModel2)
  ),

  % Check types of the accumulated expression
  type_check_oclc(AuxModel2, Exp3, Type3),
  % Type3 should be a subtype of Type2
  assertion(subtype(Type2, Type3, 1)),
  Type = Type2.

% Missing type checks
% - variable
% - .p


%
% OCL- expression evaluation
%
% eval_oclc(Tree, Model, Result).


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
    Result = [ Elem | Set ]
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
  eval_oclc(Exp, Model, Value),
  type_check_oclc(Model, Exp, Type),
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

eval_oclc( null(_), _, [] ).
eval_oclc( int_const(Value),  _,  [ Value ] ).
eval_oclc( bool_const(Value), _, [ Value ] ).

eval_oclc( and(Exp1, Exp2), Model,[ Result ] ):- 
  eval_oclc(Exp1, Model, Value1),
  eval_oclc(Exp2, Model, Value2),
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

eval_oclc( or(Exp1, Exp2), Model, [ Result ] ):- 
  eval_oclc(Exp1, Model, Value1),
  eval_oclc(Exp2, Model, Value2),
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

eval_oclc( plus(Exp1, Exp2), Model, [ Result ] ):- 
  eval_oclc(Exp1, Model, Value1),
  eval_oclc(Exp2, Model, Value2),
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


eval_oclc( minus(Exp1, Exp2), Model, [ Result ] ):- 
  eval_oclc(Exp1, Model, Value1),
  eval_oclc(Exp2, Model, Value2),
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


eval_oclc( mult(Exp1, Exp2), Model, [ Result ] ):- 
  eval_oclc(Exp1, Model, Value1),
  eval_oclc(Exp2, Model, Value2),
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


eval_oclc( divide(Exp1, Exp2), Model, [ Result ] ):- 
  eval_oclc(Exp1, Model, Value1),
  eval_oclc(Exp2, Model, Value2),
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

eval_oclc( if_then_else(Exp1, Exp2, Exp3), Model, Result ) :-
  eval_oclc(Exp1, Model, Value1),
  ( Value1 = [BoolValue1],
    BoolValue1 = 1 ->
    eval_oclc(Exp2, Model, Value2),
    Result = Value2
  ; Value1 = [BoolValue1],
    BoolValue1 = 0 ->
    eval_oclc(Exp3, Model, Value3),
    Result = Value3
  ; Value1 = [] ->
    Result = []
  ).

eval_oclc( as_type(Exp, class(X)), Model, Result ) :-
  atom_chars(X, "Set"),
  eval_oclc(Exp, Model, Value),
  make_set(Value, Result).

eval_oclc( as_type(Exp, class(X)), Model, Result ) :-
  atom_chars(X, "Seq"),
  eval_oclc(Exp, Model, Value),
  Result = Value.

eval_oclc( as_type(Exp, class(X)), Model, Result ) :-
  atom_chars(X, "OSet"),
  eval_oclc(Exp, Model, Value),
  make_set(Value, Result).

eval_oclc( as_type(Exp, class(X)), Model, Result ) :-
  atom_chars(X, "Bag"),
  eval_oclc(Exp, Model, Value),
  Result = Value.

eval_oclc( merge(Exp1, Exp2), Model, Result ) :-
  eval_oclc(Exp1, Model, Value1),
  eval_oclc(Exp2, Model, Value2),
  type_check_oclc(Model, Exp1, Type1),
  type_check_oclc(Model, Exp2, Type2),
  member(Type1, MemberType),
  % Check if we are adding a single element or several
  ( subtype(Type2, MemberType) ->
    % Adding a single element  
    collection_add(Type1, Value1, Value2, Result)
  ;
    % Adding all elements of a collection
    collection_add_all(Type1, Value1, Value2, Result)
  ).

eval_oclc( equals(Exp1, Exp2), Model, Result ) :-
  normalize(Exp1, Model, Norm1),
  normalize(Exp2, Model, Norm2),
  write('First  '), write(Norm1), nl,
  write('Second '), write(Norm2), nl,
  ( Norm1 = Norm2 -> 
    Result = [1]
  ; Result = [0] 
  ).

eval_oclc( iterate(Exp1, Var1, Var2, Exp2, Exp3), Model, Result ) :-
  % Get the types of the 
  type_check_oclc(Model, Exp1, ColType),
  member(ColType, MemberType),
  %TODO: Testing nullables
  %type_check_ocls(Model,Exp2, AccType),
  
  type_check_oclc(Model,Exp2, AuxType),
  nullable_type(AuxType, AccType),

  % Evaluate the collection
  eval_oclc(Exp1, Model, Col),

  % Initialize the accumulator 
  eval_oclc(Exp2, Model, FirstValue),

  % Start the iterate over all elements of the collection  
  eval_iterate(Col, Var1, MemberType, Var2, AccType, FirstValue, Exp3, Model, Result ).

eval_oclc( ident(Var), Model, Result ) :-
  lookup_variable(Model, Var, _, Result).

eval_oclc( lower(Exp), Model, Result ) :-
  % Check types
  type_check_oclc(Model, Exp, _),
  % Evaluate subexpression
  eval_oclc(Exp, Model, Aux),
  % Subexpression must be producing a collection with exactly one element
  assertion(length(Aux), 1),
  % The result is the element outside of a collection
  Aux = [Result].

eval_oclc( lift(Exp), Model, Result ) :-
  % Check types
  type_check_oclc(Model, Exp, _),
  % Evaluate subexpression
  eval_oclc(Exp, Model, Aux),
  % The result encapsulates the subexpression in a collection
  Result = [Aux].

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

  eval_oclc( AccExp, AuxModel2, NewValue ),

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



% ------
% Rewriting rules for translating OCL# into OCL-
% ------

translate_ocls_to_oclc( OCLs, OCLc ) :-
  % Indirect translation - rewrite OCL# code to simpler OCL# code
  recursive_expand_ocls(OCLs, Aux),
  % Direct translation - rewrite all OCL# constructs to OCL- constructs
  rewrite_ocls(Aux, OCLc).

% Expand OCL# code until there are no more changes
recursive_expand_ocls(X, Z) :-
  expand_ocls(X, Y),
  ( X = Y ->
    Z = Y ;
    recursive_expand_ocls(Y, Z)
  ).

% Step 1
% Some constructs are expanded into simpler OCL# constructs before the translation

% Base cases 
% No expansion needed and no child nodes - simply duplicate the input
expand_ocls( self(X), self(X) ).
expand_ocls( int_const(X), int_const(X) ).
expand_ocls( bool_const(X), bool_const(X) ). 
expand_ocls( null(X), null(X) ).
expand_ocls( ident(X), ident(X) ).
expand_ocls( all_instances(X), all_instances(X) ).

% Trivial cases
% No expansion but there are child nodes
% Expand the child nodes and rebuild the current nodes
expand_ocls( field(X, FieldName), field(ExpandX, FieldName) ) :-
  expand_ocls(X, ExpandX).
expand_ocls( negative(X), negative(ExpandX) ) :- 
  expand_ocls(X, ExpandX).
expand_ocls( as_sequence(X), as_sequence(ExpandX) ) :- 
  expand_ocls(X, ExpandX).  
expand_ocls( as_set(X), as_set(ExpandX) ) :- 
  expand_ocls(X, ExpandX).
expand_ocls( as_bag(X), as_bag(ExpandX) ) :- 
  expand_ocls(X, ExpandX).
expand_ocls( as_ordered_set(X), as_ordered_set(ExpandX) ) :- 
  expand_ocls(X, ExpandX).
expand_ocls( not(X), not(ExpandX) ) :- 
  expand_ocls(X, ExpandX).
expand_ocls( and(X,Y), and(ExpandX, ExpandY) ) :- 
  expand_ocls(X, ExpandX),
  expand_ocls(Y, ExpandY).
expand_ocls( or(X,Y), or(ExpandX, ExpandY) ) :- 
  expand_ocls(X, ExpandX),
  expand_ocls(Y, ExpandY).
expand_ocls( plus(X,Y), plus(ExpandX, ExpandY) ) :- 
  expand_ocls(X, ExpandX),
  expand_ocls(Y, ExpandY).
expand_ocls( minus(X,Y), minus(ExpandX, ExpandY) ) :- 
  expand_ocls(X, ExpandX),
  expand_ocls(Y, ExpandY).
expand_ocls( mult(X,Y), mult(ExpandX, ExpandY) ) :- 
  expand_ocls(X, ExpandX),
  expand_ocls(Y, ExpandY).
expand_ocls( divide(X,Y), divide(ExpandX, ExpandY) ) :- 
  expand_ocls(X, ExpandX),
  expand_ocls(Y, ExpandY).
expand_ocls( equals(X,Y), equals(ExpandX, ExpandY) ) :- 
  expand_ocls(X, ExpandX),
  expand_ocls(Y, ExpandY).
expand_ocls( if_then_else(X,Y,Z), if_then_else(ExpandX, ExpandY, ExpandZ) ) :-
  expand_ocls(X, ExpandX),
  expand_ocls(Y, ExpandY),
  expand_ocls(Z, ExpandZ).
expand_ocls( including(X,Y), including(ExpandX, ExpandY) ) :-
  expand_ocls(X, ExpandX),
  expand_ocls(Y, ExpandY).
expand_ocls( iterate(X, Var1, Var2, Y, Z), iterate(ExpandX, Var1, Var2, ExpandY, ExpandZ) ) :-
  expand_ocls(X, ExpandX),
  expand_ocls(Y, ExpandY),
  expand_ocls(Z, ExpandZ).

%% Pending expansion for concepts not in the original OCL#

expand_ocls( size(X), iterate(ExpandX, "x", "a", int_const(0), plus(ident(a), int_const(1))) ) :-
  expand_ocls(X, ExpandX).
expand_ocls( isEmpty(X), iterate(ExpandX, "x", "a", bool_const(1), bool_const(0)) ) :-
  expand_ocls(X, ExpandX).
expand_ocls( includes(X, Y), iterate(ExpandX, "x", "a", bool_const(0), or(ident("a"), equals(ident("x"), ExpandY))) ) :-
  expand_ocls(X, ExpandX),
  expand_ocls(Y, ExpandY).
expand_ocls( includesAll(X, Y), iterate(ExpandY, "x", "a", bool_const(1), and(ident("a"), includes(ExpandX, ident("x"))))  ) :-
  expand_ocls(X, ExpandX),
  expand_ocls(Y, ExpandY).
expand_ocls( count(X, Y), iterate(ExpandX, "x", "a", int_const(0), if_then_else(equals(ident("x"), ExpandY), plus(ident("a"), inst_const(1)), ident("a"))) ) :-
  expand_ocls(X, ExpandX),
  expand_ocls(Y, ExpandY).
% excluding() 
%% Pending typing
% Set{}, Bag{}, Sequence{}, OrderedSet{}

% Step 2 - Rewriting OCL# constructs as OCL- constructs

% Base cases 
% No change needed
rewrite_ocls( int_const(X), int_const(X) ).
rewrite_ocls( bool_const(X), bool_const(X) ). 
rewrite_ocls( null(X), null(X) ).
rewrite_ocls( ident(X), ident(X) ).

% Trivial cases
% Base rewrites

rewrite_ocls( self(_), ident("self") ).
rewrite_ocls( all_instances(X), all(RewriteX) ) :- 
  rewrite_ocls(X, RewriteX).
rewrite_ocls( field(X, FieldName), property(RewriteX, FieldName) ) :-
  rewrite_ocls(X, RewriteX).
rewrite_ocls( as_sequence(X), as_type(RewriteX, "Seq") ) :-
  rewrite_ocls(X, RewriteX).
rewrite_ocls( as_set(X), as_type(RewriteX, "Set") ) :-
  rewrite_ocls(X, RewriteX).
rewrite_ocls( as_bag(X), as_type(RewriteX, "Bag") ) :-
  rewrite_ocls(X, RewriteX).
rewrite_ocls( as_ordered_set(X), as_type(RewriteX, "OSet") ) :-
  rewrite_ocls(X, RewriteX).  
rewrite_ocls( negative(X), minus(int_const(0), RewriteX) ) :- 
  rewrite_ocls(X, RewriteX).
rewrite_ocls( not(X), exclusive_or(RewriteX, bool_const(1)) ) :- 
  rewrite_ocls(X, RewriteX).
rewrite_ocls( and(X,Y), and(RewriteX, RewriteY) ) :- 
  rewrite_ocls(X, RewriteX),
  rewrite_ocls(Y, RewriteY).
rewrite_ocls( or(X,Y), or(RewriteX, RewriteY) ) :- 
  rewrite_ocls(X, RewriteX),
  rewrite_ocls(Y, RewriteY).
rewrite_ocls( plus(X,Y), plus(RewriteX, RewriteY) ) :- 
  rewrite_ocls(X, RewriteX),
  rewrite_ocls(Y, RewriteY).
rewrite_ocls( minus(X,Y), minus(RewriteX, RewriteY) ) :- 
  rewrite_ocls(X, RewriteX),
  rewrite_ocls(Y, RewriteY).
rewrite_ocls( mult(X,Y), mult(RewriteX, RewriteY) ) :- 
  rewrite_ocls(X, RewriteX),
  rewrite_ocls(Y, RewriteY).
rewrite_ocls( divide(X,Y), divide(RewriteX, RewriteY) ) :- 
  rewrite_ocls(X, RewriteX),
  rewrite_ocls(Y, RewriteY).
rewrite_ocls( equals(X,Y), equals(RewriteX, RewriteY) ) :- 
  rewrite_ocls(X, RewriteX),
  rewrite_ocls(Y, RewriteY).
rewrite_ocls( if_then_else(X,Y,Z), if_then_else(RewriteX, RewriteY, RewriteZ) ) :-
  rewrite_ocls(X, RewriteX),
  rewrite_ocls(Y, RewriteY),
  rewrite_ocls(Z, RewriteZ).

%% Pending action according to type
rewrite_ocls( including(X,Y), including(RewriteX, RewriteY) ) :-
  rewrite_ocls(X, RewriteX),
  rewrite_ocls(Y, RewriteY).

%% Pending action according to type
rewrite_ocls( iterate(X, Var1, Var2, Y, Z), iterate(RewriteX, Var1, Var2, RewriteY, RewriteZ) ) :-
  rewrite_ocls(X, RewriteX),
  rewrite_ocls(Y, RewriteY),
  rewrite_ocls(Z, RewriteZ).









  
  

    


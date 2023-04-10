:- set_prolog_flag(double_quotes, chars).

test_list_parser(
  [
    parser_test(
      'Single invariant',
      "context C inv : x" , 
      spec([invariant(_, ident(_))])
    ),
    parser_test(
      'Two invariants',
      "context C inv : true context D inv : false", 
      spec([invariant(_, bool_const(1)), invariant(_, bool_const(0))])
    ),
    parser_test(
      'Arithmetic expression 2',
      "context C inv : 0 * 2 + (4 / 2 - 4) + 220 + 12 = -1",
      spec([invariant(_, equals(plus(mult(int_const(0), int_const(2)), plus(minus(divide(int_const(4), int_const(2)), int_const(4)), plus(int_const(220), int_const(12)))), negative(int_const(1))))])
    ),

    parser_test(
      'Arithmetic expression 1',
      "context C inv : 1 + 2 = 3",
      spec([invariant(_, equals(plus(int_const(1), int_const(2)),int_const(3)))])
    ),

    parser_test(
      'Boolean expression 1',
      "context C inv: not true and false or false or true",
      spec([invariant(_, and(not(bool_const(1)), or(bool_const(0), or(bool_const(0), bool_const(1)))))])
    ),

    parser_test(
      'nulls and including',
      "context Ab inv: no(Set(Set(int)))->including(no(Set(int)))",
      spec([invariant(_, including(null(set(set(integer))), null(set(integer))))])
    ),
    parser_test(
      'Boolean connectives and allInstances',
      "context Sample inv: true and false or (Sample.allInstances() = no(Sample))",
      spec([invariant(_, and(bool_const(1), or(bool_const(0), equals(all_instances(_), null(class(_))))))])
    )
  ]
).

test_list_type_check( [
    type_check_test(
      'Boolean test 3',
      "not(1 = 2) and false",
      %TODO: Nullable test
      %ctype(boolean, 0, 1, _, _)
      ctype(boolean, 1, 1 ,_, _)
    ),
    type_check_test(
      'Boolean test 2',
      "true and false or (true or false)",
      %TODO: Nullable test
      %ctype(boolean, 0, 1, _, _)
      ctype(boolean, 1, 1 ,_, _)
    ),
    type_check_test(
      'Boolean test 1',
      "true",
      %TODO: Nullable test
      %ctype(boolean, 0, 1, _, _)
      ctype(boolean, 1, 1 ,_, _)
    ),
    type_check_test(
      'Integer test 3',
      "1 - ( 2 / 1 )",
      %TODO: Nullable test
      %ctype(integer, 0, 1, _, _)
      ctype(integer, 1, 1 ,_, _)
    ),
    type_check_test(
      'Integer test 2',
      "1 + ( 2 * -3 )",
      %TODO: Nullable test
      %ctype(integer, 0, 1, _, _)
      ctype(integer, 1, 1 ,_, _)
    ),
    type_check_test(
      'Integer test 1',
      "1",
      %TODO: Nullable test
      %ctype(integer, 0, 1, _, _)
      ctype(integer, 1, 1 ,_, _)
    ),
    type_check_test(
      'no() test 4',
      "no(Set(bool))",
      ctype(ctype(boolean, 0, [*], 1, 0), 0, 1 ,_, _)
    ),
    type_check_test(
      'no() test 3',
      "no(OclAny)",
      ctype(any, 0, 1 ,_, _)
    ),
    type_check_test(
      'no test 2',
      "no(int)",
      ctype(integer, 0, 1 ,_, _)
    ),
    type_check_test(
      'no() test 1',
      "no(bool)",
      ctype(boolean, 0, 1 ,_, _)
    ),
    type_check_test(
      'if-then-else test 4',
      "if true then no(bool) else false endif",
      ctype(boolean, 0, 1, _, _)
    ),
    type_check_test(
      'if-then-else test 3',
      "if true then no(bool) else false endif",
      ctype(boolean, 0, 1, _, _)
    ),
    type_check_test(
      'if-then-else test 2',
      "if true then false else true endif",
      %TODO: Nullable test
      %ctype(boolean, 0, 1, _, _)
      ctype(boolean, 1, 1 ,_, _)
    ),
    type_check_test(
      'if-then-else test 1',
      "if (false = true) then 7+2/2 else 2*0 endif",
      %TODO: Nullable test
      %ctype(integer, 0, 1, _, _)
      ctype(integer, 1, 1 ,_, _)
    ),
    type_check_test(
      'asCollection test 3',
      "no(Sequence(Set(int)))->asBag()",
      ctype(ctype(ctype(integer, 0, [*], 1, 0), 0, [*], 0, 1), 0, [*], 0, 0)
    ),    
    type_check_test(
      'asCollection test 2',
      "true->asSequence()->asOrderedSet()",
      ctype(boolean, 0, [*],1, 1)
    ),    
    type_check_test(
      'asCollection test 1',
      "1->asSet()",
      ctype(integer, 0, [*] ,_, _)
    ),
    type_check_test(
      'All instances test',
      "C.allInstances()",
      ctype(class(_), 0, [*] ,_, _)
    ),
    type_check_test(
      'Class name test 1',
      "no(T)",
      ctype(class(_), 0, 1 ,_, _)
    ),
    type_check_test(
      'including test 2',
      "1->asSet()->including(2->asSet())",
      ctype(integer, 0, [*] ,_, _)
    ),
    type_check_test(
      'including test 1',
      "1->asSet()->including(2)",
      ctype(integer, 0, [*] ,_, _)
    ),
    type_check_test(
      'iterate 3',
      "no(bool)->asBag()->iterate(x;b=true|b or x)",
      %TODO: Nullable test
      %ctype(boolean, 0, 1, _, _)
      ctype(boolean, 1, 1, _, _)

    ),
    type_check_test(
      'iterate 2',
      "no(bool)->asSet()->iterate(x;b=true|true or x)",
      %TODO: Nullable test
      %ctype(boolean, 0, 1, _, _)
      ctype(boolean, 1, 1, _, _)

    ),
    type_check_test(
      'iterate 1',
      "1->asSequence()->iterate(a;b=0|1)",
      %TODO: Nullable test
      %ctype(integer, 0, 1, _, _)
      ctype(integer, 1, 1, _, _)
    )
  ]).

test_list_eval( [
    eval_test(
      'Boolean test 3',
      "not(1 = 2) and false",
      [0]
    ),
    eval_test(
      'Boolean test 2',
      "true and false or (true or false)",
      [1]
    ),
    eval_test(
      'Boolean test 1',
      "true",
      [1]
    ),
    eval_test(
      'Integer test 3',
      "1 - ( 2 / 1 )",
      [-1]
    ),
    eval_test(
      'Integer test 2',
      "1 + ( 2 * -3 )",
      [-5]
    ),
    eval_test(
      'Integer test 1',
      "3",
      [3]
    ),
    eval_test(
      'no() test 4',
      "no(Set(bool))",
      []
    ),
    eval_test(
      'no() test 3',
      "no(OclAny)",
      []
    ),
    eval_test(
      'no test 2',
      "no(int)",
      []
    ),
    eval_test(
      'no() test 1',
      "no(bool)",
      []
    ),
    eval_test(
      'if-then-else test 4',
      "if no(bool) then 75 else 2+33 endif",
      []
    ),
    eval_test(
      'if-then-else test 4',
      "if true then no(bool) else false endif",
      []
    ),
    eval_test(
      'if-then-else test 3',
      "if false then no(bool) else false endif",
      [0]
    ),
    eval_test(
      'if-then-else test 2',
      "if true then false else true endif",
      [0]
    ),
    eval_test(
      'if-then-else test 1',
      "if (false = true) then 7+1/2 else 2*0 endif",
      [0]
    ),
    eval_test(
      'asCollection test 3',
      "no(Sequence(Set(int)))->asBag()",
      []
    ),    
    eval_test(
      'asCollection test 2',
      "true->asSequence()->asOrderedSet()",
      [1]
    ),    
    eval_test(
      'asCollection test 1',
      "1->asSet()->including(2)",
      [2,1]
    ),
    eval_test(
      'asSet 3',
      "no(int)->asSet()->including(7)->including(8) = no(int)->asSet()->including(8)->including(7)",
      [1]
    ),
    eval_test(
      'asSet 2',
      "no(int)->asSet()->including(7)->including(7)->asSequence()",
      [7]
    ),
    eval_test(
      'asSet 1',
      "no(int)->asSequence()->including(7)->including(7)->asSet()",
      [7]
    ),
    eval_test(
      'equals 2',
      "1->asBag()->including(2) = 2->asBag()->including(1)",
      [1]
    ),
    eval_test(
      'equals 1',
      "8->asSet()->including(2) = 2->asBag()->including(8)->including(8)->asSet()",
      [1]
    ),
    eval_test(
      'Class name test 1',
      "no(T)",
      []
    ),
    eval_test(
      'including test 2',
      "1->asSet()->including(2->asSet())",
      [2,1]
    ),
    eval_test(
      'including test 1',
      "1->asSet()->including(2)",
      [2,1]
    ),
    eval_test(
      'iterate 3',
      "2->asBag()->including(3)->including(5)->including(3)->iterate(x;b=no(int)->asSequence()|b->including(x*3))",
      [9,15,9,6]
    ),
    eval_test(
      'iterate 2',
      "no(bool)->asSet()->iterate(x;b=true|false and x)",
      [1]
    ),
    eval_test(
      'iterate 1',
      "1->asSequence()->iterate(a;b=0|1)",
      [1]
    ),
    eval_test(
      'size() 5 - Full Set(Set(int))',
      "no(Set(int))->asSequence()->including(no(int)->asSet()->including(3))->including(no(int)->asSet()->including(3)->including(7)->including(5))->iterate(x;a=0|a+1)",
      [2]
    ),
    eval_test(
      'size() 4 - Empty Set(Set(int))',
      "no(Set(int))->asSequence()->iterate(x;a=0|a+1)",
      [0]
    ),
    eval_test(
      'size() 3 - Empty sequence(int)',
      "no(int)->asSequence()->iterate(x;a=0|a+1)",
      [0]
    ),
    eval_test(
      'size() 2 - Full bag(int)',
      "0->asBag()->iterate(x;a=0|a+1)",
      [1]
    ),
    eval_test(
      'size() 1 - Full OrderedSet(int)',
      "0->asOrderedSet()->including(3)->including(4)->iterate(x;a=0|a+1)",
      [3]
    ),
    eval_test(
      'isEmpty() 5 - Full Set(Set(int))',
      "no(Set(int))->asSequence()->including(no(int)->asSet()->including(3))->including(no(int)->asSet()->including(3)->including(7)->including(5))->iterate(x;a=true|false)",
      [0]
    ),
    eval_test(
      'isEmpty() 4 - Empty Set(Set(int))',
      "no(Set(int))->asSequence()->iterate(x;a=true|false)",
      [1]
    ),
    eval_test(
      'isEmpty() 3 - Empty sequence(int)',
      "no(int)->asSequence()->iterate(x;a=true|false)",
      [1]
    ),
    eval_test(
      'isEmpty() 2 - Full bag(int)',
      "0->asBag()->iterate(x;a=true|false)",
      [0]
    ),
    eval_test(
      'isEmpty() 1 - Full OrderedSet(int)',
      "0->asOrderedSet()->including(3)->including(4)->iterate(x;a=true|false)",
      [0]
    ),
    eval_test(
      'includes() 6 - Full Set(Set(int)) - the element is included',
      "no(Set(int))->asSequence()->including(no(int)->asSet()->including(3))->including(no(int)->asSet()->including(3)->including(7)->including(5))->iterate(x;a=false|a or (x = no(int)->asSet()->including(3)))",
      [1]
    ),
    eval_test(
      'includes() 5 - Full Set(Set(int)) - the element is not included',
      "no(Set(int))->asSequence()->including(no(int)->asSet()->including(3))->including(no(int)->asSet()->including(3)->including(7)->including(5))->iterate(x;a=false| a or (x = no(int)))",
      [0]
    ),
    eval_test(
      'isEmpty() 4 - Empty Set(Set(int))',
      "no(Set(int))->asSequence()->iterate(x;a=false| a or (x = no(int)))",
      [0]
    ),
    eval_test(
      'includes() 3 - Empty sequence(int)',
      "no(int)->asSequence()->iterate(x;a=false|a or (x = 7))",
      [0]
    ),
    eval_test(
      'includes() 2 - Full bag(int) - the element is not included',
      "0->asBag()->iterate(x;a=false|a or (x = 7))",
      [0]
    ),
    eval_test(
      'includes() 1 - Full bag(int) - the element is included',
      "0->asBag()->including(2)->including(5)->iterate(x;a=false|a or (x = 2))",
      [1]
    ),

    eval_test(
      'count() 6 - Full Set(Set(int)) - the element is included',
      "no(Set(int))->asSequence()->including(no(int)->asSet()->including(3))->including(no(int)->asSet()->including(3)->including(7)->including(5))->iterate(x;a=0|if (x = no(int)->asSet()->including(3)) then a+1 else a endif)",
      [1]
    ),
    eval_test(
      'count() 5 - Full Set(Set(int)) - the element is not included',
      "no(Set(int))->asSequence()->including(no(int)->asSet()->including(3))->including(no(int)->asSet()->including(3)->including(7)->including(5))->iterate(x;a=0| if x = no(int) then a+1 else a endif)",
      [0]
    ),
    eval_test(
      'count() 4 - Empty Set(Set(int))',
      "no(Set(int))->asSequence()->iterate(x;a=0| if x= 5 then a+1 else a endif)",
      [0]
    ),
    eval_test(
      'count() 3 - Empty sequence(int)',
      "no(int)->asSequence()->iterate(x;a=0|if x= 5 then a+1 else a endif)",
      [0]
    ),
    eval_test(
      'count() 2 - Full bag(int) - the element is not included',
      "0->asBag()->including(7)->including(7)->iterate(x;a = 0 |if x=5 then a+1 else a endif)",
      [0]
    ),
    eval_test(
      'count() 1 - Full bag(int) - the element is included',
      "0->asBag()->including(7)->including(7)->iterate(x;a = 0 |if x=7 then a+1 else a endif)",
      [2]
    ),
    eval_test(
      'excluding() 7 - Full Set(Set(int)) - the element is included',
      "no(Set(int))->asSequence()->including(no(int)->asSet()->including(3))->including(no(int)->asSet()->including(3)->including(7)->including(5))->iterate(x;a=no(Set(int))->asSet()|if (x = no(int)->asSet()->including(3)) then a else a->including(x) endif)",
      [[5,7,3]]
    ),
    eval_test(
      'excluding() 6 - Full Set(Set(int)) - the element is not included',
      "no(Set(int))->asSequence()->including(no(int)->asSet()->including(3))->including(no(int)->asSet()->including(3)->including(7)->including(5))->iterate(x;a=no(Set(int))->asSet() | if x = no(Set(int)) then a else a->including(x) endif)",
      [[5,7,3],[3]]
    ),
    eval_test(
      'excluding() 5 - Empty Set(Set(int))',
      "no(Set(int))->asSequence()->iterate(x;a=no(Set(int))->asSet()| if x= no(Set(int)) then a else a->including(x) endif)",
      []
    ),
    eval_test(
      'excluding() 4 - Empty sequence(int)',
      "no(int)->asSequence()->iterate(x;a=no(int)->asSet()|if x = 5 then a else a->including(x) endif)",
      []
    ),
    eval_test(
      'excluding() 3 - Full bag(int) - the element is not included',
      "0->asBag()->including(2)->including(7)->iterate(x;a=no(int)->asSet()|if x=5 then a else a->including(x) endif)",
      [0,2,7]
    ),
    eval_test(
      'excluding() 2 - Full bag(int) - the element is included',
      "2->asBag()->including(7)->including(7)->iterate(x;a=no(int)->asSet()|if x=7 then a else a->including(x) endif)",
      [2]
    ),
    eval_test(
      'excluding() 1 - Full bag(int) - the element is not included',
      "0->asBag()->including(7)->including(7)->iterate(x;a=no(int)->asBag()|if x=3 then a else a->including(x) endif)",
      [0,7,7]
    ),
    eval_test(
      'prepend() 3 - [[1,8]]->prepend [[1],[]]',
      "no(Sequence(int))->asSequence()->including(no(Sequence(int))->asSequence()->including(no(int)->asSequence()->including(1))->including(no(int)->asSequence()))->including(no(Sequence(int))->asSequence()->including(no(int)->asSequence()->including(1)->including(8)))",
      [[1],[],[1,8]]
    ),
    eval_test(
      'prepend() 2 - [1,8]->prepend Empty sequence(int)',
      "no(int)->asSequence()->including(no(int))->including(no(int)->asSequence()->including(1)->including(8))",
      [1,8]
    ),
    eval_test(
      'prepend() 1 - [1,8]->prepend([2,7])',
      "no(int)->asSequence()->including(no(int)->asSequence()->including(2)->including(7))->including(no(int)->asSequence()->including(1)->including(8))",
      [2,7,1,8]
    ),
    eval_test(
      'flatten() 3 - flatten [[1],[],[1,8]]',
      "no(Sequence(int))->asSequence()->including(no(Sequence(int))->asSequence()->including(no(int)->asSequence()->including(1))->including(no(int)->asSequence()))->including(no(Sequence(int))->asSequence()->including(no(int)->asSequence()->including(1)->including(8)))->iterate(x;a=no(int)->asSequence()|a->including(x))",
      [1,1,8]
    ),
    eval_test(
      'flatten() 2 - flatten [1,2,3]',
      "no(int)->asSequence()->including(1)->including(2)->including(3)->iterate(x;a=no(int)->asSequence()|a->including(x))",
      [1,2,3]
    ),
    eval_test(
      'flatten() 1 - Empty sequence',
      "no(int)->asSequence()->iterate(x; a = no(int)->asSequence()| a->including(x))",
      []
    ),
    eval_test(
      'includesAll() 6 - both full and included',
      "no(int)->asBag()->including(7)->including(3)->including(5)->asSet()->including(no(int)->asBag()->including(5)->including(7)) =no(int)->asBag()->including(7)->including(3)->including(5)->asSet()",
      [1]
    ),
    eval_test(
      'includesAll() 5 - both full but only some included',
      "no(int)->asBag()->including(7)->including(3)->asSet()->including(no(int)->asBag()->including(7)->including(1)) =no(int)->asBag()->asSet()",
      [0]
    ),
    eval_test(
      'includesAll() 4 - both full but not included',
      "no(int)->asBag()->including(7)->asSet()->including(no(int)->asBag()->including(1)) =no(int)->asBag()->asSet()",
      [0]
    ),
    eval_test(
      'includesAll() 3 - e2 is empty',
      "no(int)->asBag()->including(7)->asSet()->including(no(int)->asBag()) = no(int)->asBag()->asSet()->including(7)",
      [1]
    ),
    eval_test(
      'includesAll() 2 - Both e1 and e2 are empty',
      "no(int)->asBag()->asSet()->including(no(int)->asBag()) = no(int)->asBag()->asSet()",
      [1]
    ),
    eval_test(
      'includesAll() 1 - e1 is an Empty bag',
      "no(int)->asBag()->asSet()->including(no(int)->asBag()->including(1)) =no(int)->asBag()->asSet()",
      [0]
    ),
    eval_test(
      'any() 7 - e1 not empty and condition is satisfied',
      "no(Set(int))->asBag()->including(no(int)->asSet()->including(7)->including(5))->including(no(int)->asSet())->iterate(x;a=no(Set(int))|if x = no(int)->asSet() then x else a endif)",
      []
    ),
    eval_test(
      'any() 6 - e1 not empty and condition is satisfied',
      "no(Set(int))->asBag()->including(no(int)->asSet()->including(7)->including(5))->including(no(int)->asSet())->iterate(x;a=no(Set(int))|if x = no(int)->asSet()->including(7)->including(5) then x else a endif)",
      [5,7]
    ),
    eval_test(
      'any() 5 - e1 not empty but condition is not satisfied',
      "no(Set(int))->asBag()->including(no(int)->asSet()->including(5))->iterate(x;a=no(Set(int))|if x = no(int)->asSet()->including(7) then x else a endif)",
      []
    ),
    eval_test(
      'any() 4 - e1 is an Empty bag',
      "no(Set(int))->asBag()->iterate(x;a=no(Set(int))|if x = no(int)->asSet() then x else a endif)",
      []
    ),
    eval_test(
      'any() 3 - e1 not empty and condition is satisfied',
      "no(int)->asBag()->including(5)->including(7)->iterate(x;a=no(int)|if x = 7 then x else a endif)",
      [7]
    ),
    eval_test(
      'any() 2 - e1 not empty but condition is not satisfied',
      "no(int)->asBag()->including(5)->iterate(x;a=no(int)|if x = 7 then x else a endif)",
      []
    ),
    eval_test(
      'any() 1 - e1 is an Empty bag',
      "no(int)->asBag()->iterate(x;a=no(int)|if x = 7 then x else a endif)",
      []
    ),
    eval_test(
      'any() 7 - e1 not empty and condition is satisfied',
      "no(Set(int))->asBag()->including(no(int)->asSet()->including(7)->including(5))->including(no(int)->asSet())->iterate(x;a=no(Set(Set(int)))|if x = no(int)->asSet() then no(Set(int))->asSet()->including(x) else a endif)",
      [ [ ] ]
    ),
    eval_test(
      'any() 6 - e1 not empty and condition is satisfied',
      "no(Set(int))->asBag()->including(no(int)->asSet()->including(7)->including(5))->including(no(int)->asSet())->iterate(x;a=no(Set(Set(int)))|if x = no(int)->asSet()->including(7)->including(5) then no(Set(int))->asSet()->including(x) else a endif)",
      [ [5,7] ]
    ),
    eval_test(
      'any() 5 - e1 not empty but condition is not satisfied',
      "no(Set(int))->asBag()->including(no(int)->asSet()->including(5))->iterate(x;a=no(Set(Set(int)))|if x = no(int)->asSet()->including(7) then no(Set(int))->asSet()->including(x) else a endif)",
      []
    ),
    eval_test(
      'any() 4 - e1 is an Empty bag',
      "no(Set(int))->asBag()->iterate(x;a=no(Set(int))|if x = no(int)->asSet() then x else a endif)",
      []
    ),
    eval_test(
      'any() 3 - e1 not empty and condition is satisfied',
      "no(int)->asBag()->including(5)->including(7)->iterate(x;a=no(Set(int))|if x = 7 then no(int)->asSet()->including(x) else a endif)",
      [7]
    ),
    eval_test(
      'any() 2 - e1 not empty but condition is not satisfied',
      "no(int)->asBag()->including(5)->iterate(x;a=no(Set(int))|if x = 7 then no(int)->asSet()->including(x) else a endif)",
      []
    ),
    eval_test(
      'any() 1 - e1 is an Empty bag',
      "no(int)->asBag()->iterate(x;a=no(Set(int))|if x = 7 then no(int)->asSet()->including(x) else a endif)",
      []
    )
    %eval_test(
    %  'collect() 1 - e1 is empty',
    %  "no(int)->asBag()->iterate(x;a=no(int)|if x = 7 then x else a endif)",
    %  []
    %)


  ]).

% append - not necessary, it is equivalent to including
% collect

unit_test_all :-
  unit_test_parser(Total1, Passed1, Failed1),
  unit_test_type_check(Total2, Passed2, Failed2),
  unit_test_eval(Total3, Passed3, Failed3),
  Total  is Total1 + Total2 + Total3,
  Passed is Passed1 + Passed2 + Passed3,
  Failed is Failed1 + Failed2 + Failed3,
  format('~nAll tests~n Total : ~w~n Passed: ~w~n Failed: ~w~n~n', [Total, Passed, Failed]).
  

unit_test_parser(Total, Passed, Failed) :-
  format('Starting parser test~n--------------------~n', []),
  test_list_parser( TestList ),
  do_test_parser( TestList, Passed, Failed),
  Total is Passed + Failed,
  format('--------------------~n', []),
  format('Parser tests~n Total : ~w~n Passed: ~w~n Failed: ~w~n~n', [Total, Passed, Failed]).

do_test_parser( [], 0, 0).
do_test_parser( [Test | Rest], Passed, Failed) :-
  Test = parser_test(Name, Input, ExpectedOutput),
  do_test_parser( Rest, RestPassed, RestFailed ),
  ( parse_ocls( Input, Output ),
    Output = ExpectedOutput ->
    Passed is RestPassed + 1,
    Failed is RestFailed,
    format('- ~w: Passed~n', [Name])
  ;
    Passed is RestPassed,
    Failed is RestFailed + 1,
    format('- ~w: Failed~n', [Name]) 
  ).

unit_test_type_check(Total, Passed, Failed) :-
  format('Starting type check test~n--------------------------~n', []),
  test_list_type_check( TestList ),
  do_test_type_check( TestList, Passed, Failed),
  Total is Passed + Failed,
  format('--------------------------~n', []),
  format('Type check tests~n Total : ~w~n Passed: ~w~n Failed: ~w~n~n', [Total, Passed, Failed]).

do_test_type_check( [], 0, 0).
do_test_type_check( [Test | Rest], Passed, Failed) :-
  Test = type_check_test(Name, Input, ExpectedOutput),
  do_test_type_check( Rest, RestPassed, RestFailed ),
  ( type_check_ocls_expr(Input, Output),
    Output = ExpectedOutput ->
    Passed is RestPassed + 1,
    Failed is RestFailed,
    format('- ~w: Passed~n', [Name])   
  ;
    type_check_ocls_expr(Input, Output),
    Passed is RestPassed,
    Failed is RestFailed + 1,
    format('- ~w: Failed~n', [Name]),
    format('  - Expected: ~w~n', [ExpectedOutput]),
    format('  - Obtained: ~w~n', [Output])     
  ).

unit_test_eval(Total, Passed, Failed) :-
  format('Starting eval test~n--------------------~n', []),
  test_list_eval( TestList ),
  do_test_eval( TestList, Passed, Failed),
  Total is Passed + Failed,
  format('--------------------~n', []),
  format('Eval tests~n Total : ~w~n Passed: ~w~n Failed: ~w~n~n', [Total, Passed, Failed]).

do_test_eval( [], 0, 0).
do_test_eval( [Test | Rest], Passed, Failed) :-
  Test = eval_test(Name, Input, ExpectedOutput),
  do_test_eval( Rest, RestPassed, RestFailed ),
  ( eval_ocls_expr(Input, Output), 
    Output = ExpectedOutput ->
    Passed is RestPassed + 1,
    Failed is RestFailed,
    format('- ~w: Passed~n', [Name])
  ;
    eval_ocls_expr(Input, Output), 
    Passed is RestPassed,
    Failed is RestFailed + 1,
    format('- ~w: Failed~n', [Name]),
    format('  - Expected: ~w~n', [ExpectedOutput]),
    format('  - Obtained: ~w~n', [Output])     
  ).
  
  

    


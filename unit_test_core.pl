:- set_prolog_flag(double_quotes, chars).

test_list_parser(
  [
    parser_test(
      'Basic constraint',
      "true" , 
      bool_const(1)
    ),
    parser_test(
      'Expressions using merge',
      "12 merge true lower", 
      merge(int_const(12),lower(bool_const(1)))
    ),
    parser_test(
      'Arithmetic expression 1',
      "1 + 2 = 3",
      equals(plus(int_const(1), int_const(2)),int_const(3))
    ),
    parser_test(
      'Arithmetic expression 2',
      "0 * 2 + (4 / 2 - 4) + 220 + 12 = 1",
      equals(plus(mult(int_const(0), int_const(2)), plus(minus(divide(int_const(4), int_const(2)), int_const(4)), plus(int_const(220), int_const(12)))), int_const(1))
    ),
    parser_test(
      'Boolean expression 1',
      "true and false or false xor true",
      and(bool_const(1), or(bool_const(0), exclusive_or(bool_const(0), bool_const(1))))
    ),
    parser_test(
      'all',
      "all T",
      all(_)
    ),
    parser_test(
      'null',
      "no int",
      null(integer)
    ),
    parser_test(
      'merge',
      "1 merge 2 merge 3",
      merge(int_const(1), merge(int_const(2), int_const(3)))
    ),
    parser_test(
      'lower',
      "no int lift",
      lift(null(integer))
    ),
    parser_test(
      'merge',
      "no bool lower",
      lower(null(boolean))
    ),
    parser_test(
      'if-then-else',
      "(1 =2) ? 3 : 4",
      if_then_else( equals(int_const(1), int_const(2)), int_const(3), int_const(4) )
    ),
    parser_test(
      'iterator',
      "all T -> [x | a <- 0 | a * 2 ]",
      iterate( all(_), _, _, int_const(0), mult(ident(_), int_const(2)) )
    )
  ]
).

test_list_type_check( [
    type_check_test(
      'Boolean test 1',
      "(1 = 2) xor false",
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
      "1 + ( 2 * 3 )",
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
      "no Set(bool)",
      ctype(ctype(boolean, 0, [*], 1, 0), 0, 1 ,_, _)
    ),
    type_check_test(
      'no() test 3',
      "no any",
      ctype(any, 0, 1 ,_, _)
    ),
    type_check_test(
      'no test 2',
      "no int ",
      ctype(integer, 0, 1 ,_, _)
    ),
    type_check_test(
      'no() test 1',
      "no bool",
      ctype(boolean, 0, 1 ,_, _)
    ),
    type_check_test(
      'if-then-else test 3',
      "true ? no bool : false",
      ctype(boolean, 0, 1, _, _)
    ),
    type_check_test(
      'if-then-else test 2',
      "true ? false : true",
      %TODO: Nullable test
      %ctype(boolean, 0, 1, _, _)
      ctype(boolean, 1, 1 ,_, _)
    ),
    type_check_test(
      'if-then-else test 1',
      "(false = true) ? (7+2/2) : (2*0)",
      %TODO: Nullable test
      %ctype(integer, 0, 1, _, _)
      ctype(integer, 1, 1 ,_, _)
    ),
    type_check_test(
      'asCollection test 3',
      "no Seq(Set(int)) as Bag",
      ctype(ctype(ctype(integer, 0, [*], 1, 0), 0, [*], 0, 1), 0, [*], 0, 0)
    ),    
    type_check_test(
      'asCollection test 2',
      "(true as Seq) as OSet",
      ctype(boolean, 0, [*], 1, 1)
    ),    
    type_check_test(
      'asCollection test 1',
      "1 as Set",
      ctype(integer, 0, [*] ,_, _)
    ),
    type_check_test(
      'All instances test',
      "all C",
      ctype(class(_), 0, [*] ,_, _)
    ),
    type_check_test(
      'Class name test 1',
      "no T",
      ctype(class(_), 0, 1 ,_, _)
    ),
    type_check_test(
      'including test 2',
      "(1 as Set) merge (2 as Set)",
      ctype(integer, 0, [*] ,_, _)
    ),
    type_check_test(
      'including test 1',
      "(1 as Set) merge 2",
      ctype(integer, 0, [*] ,_, _)
    ),
    type_check_test(
      'iterate 3',
     "((no bool) as Bag) -> [ x | b <- true |b or x]",
     %TODO: Nullable test
     %ctype(boolean, 0, 1, _, _)
     ctype(boolean, 1, 1, _, _)
    ),
    type_check_test(
      'iterate 2',
      "((no bool) as Set) -> [x | b <- true | true or x]",
      %TODO: Nullable test
      %ctype(boolean, 0, 1, _, _)
      ctype(boolean, 1, 1, _, _)
    ),
    type_check_test(
      'iterate 1',
      "(1 as Seq) -> [a | b <- 0 | 1]",
      %TODO: Nullable test
      %ctype(integer, 0, 1, _, _)
      ctype(integer, 1, 1, _, _)
    )
  ]).

test_list_eval( [
    eval_test(
      'Boolean test 3',
      "(1 = 2) and false",
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
      "1 + ( 2 * (0-3) )",
      [-5]
    ),
    eval_test(
      'Integer test 1',
      "3",
      [3]
    ),
    eval_test(
      'no() test 4',
      "no bool as Set",
      []
    ),
    eval_test(
      'no() test 3',
      "no any",
      []
    ),
    eval_test(
      'no test 2',
      "no int",
      []
    ),
    eval_test(
      'no() test 1',
      "no bool",
      []
    ),
    eval_test(
      'if-then-else test 4',
      "(no bool) ?  75 : (2+33)",
      []
    ),
    eval_test(
      'if-then-else test 4',
      "true ? no bool : false",
      []
    ),
    eval_test(
      'if-then-else test 3',
      "false ? no bool : false",
      [0]
    ),
    eval_test(
      'if-then-else test 2',
      "true ? false : true",
      [0]
    ),
    eval_test(
      'if-then-else test 1',
      "(false = true) ? (7+1/2) :  (2*0)",
      [0]
    ),
    eval_test(
      'asCollection test 3',
      "(((no int) as Set) lift) as Bag",
      [[]]
    ),    
    eval_test(
      'asCollection test 2',
      "(true as Seq) as OSet",
      [1]
    ),    
    eval_test(
      'asCollection test 1',
      "(1 as Set) merge 2",
     [2,1]
    )
    %eval_test(
    %  'asSet 3',
    % "no(int)->asSet()->including(7)->including(8) = no(int)->asSet()->including(8)->including(7)",
    %  [1]
    %),
    %eval_test(
    %  'asSet 2',
    %  "no(int)->asSet()->including(7)->including(7)->asSequence()",
    %  [7]
    %),
    %eval_test(
    %  'asSet 1',
    %  "no(int)->asSequence()->including(7)->including(7)->asSet()",
    %  [7]
    %,
    %eval_test(
    %  'equals 2',
    %  "1->asBag()->including(2) = 2->asBag()->including(1)",
    %  [1]
    %),
    %val_test(
    %  'equals 1',
    %  "8->asSet()->including(2) = 2->asBag()->including(8)->including(8)->asSet()",
    %  [1]
    %),
    %eval_test(
    %  'Class name test 1',
    %  "no(T)",
    %  []
    %),
    %eval_test(
    %  'including test 2',
    %  "1->asSet()->including(2->asSet())",
    %  [2,1]
    %),
    %eval_test(
    %  'including test 1',
    %  "1->asSet()->including(2)",
    %  [2,1]
    %),
    %eval_test(
    %  'iterate 3',
    %  "2->asBag()->including(3)->including(5)->including(3)->iterate(x;b=no(int)->asSequence()|b->including(x*3))",
    %  [9,15,9,6]
    %),
    %eval_test(
    %  'iterate 2',
    %  "no(bool)->asSet()->iterate(x;b=true|false and x)",
    %  [1]
    %),
    %eval_test(
    %  'iterate 1',
    %  "1->asSequence()->iterate(a;b=0|1)",
    %  [1]
    %),
    %eval_test(
    %  'size() 5 - Full Set(Set(int))',
    %  "no(Set(int))->asSequence()->including(no(int)->asSet()->including(3))->including(no(int)->asSet()->including(3)->including(7)->including(5))->iterate(x;a=0|a+1)",
    %  [2]
    %),
    %eval_test(
    %  'size() 4 - Empty Set(Set(int))',
    %  "no(Set(int))->asSequence()->iterate(x;a=0|a+1)",
    %  [0]
    %),
    %eval_test(
    %  'size() 3 - Empty sequence(int)',
    %  "no(int)->asSequence()->iterate(x;a=0|a+1)",
    %  [0]
    %),
    %eval_test(
    %  'size() 2 - Full bag(int)',
    %  "0->asBag()->iterate(x;a=0|a+1)",
    %  [1]
    %),
    %eval_test(
    %  'size() 1 - Full OrderedSet(int)',
    %  "0->asOrderedSet()->including(3)->including(4)->iterate(x;a=0|a+1)",
    %  [3]
    %),
    %eval_test(
    %  'isEmpty() 5 - Full Set(Set(int))',
    %  "no(Set(int))->asSequence()->including(no(int)->asSet()->including(3))->including(no(int)->asSet()->including(3)->including(7)->including(5))->iterate(x;a=true|false)",
    %  [0]
    %),
    %eval_test(
    %  'isEmpty() 4 - Empty Set(Set(int))',
    %  "no(Set(int))->asSequence()->iterate(x;a=true|false)",
    %  [1]
    %),
    %eval_test(
    %  'isEmpty() 3 - Empty sequence(int)',
    %  "no(int)->asSequence()->iterate(x;a=true|false)",
    %  [1]
    %),
    %eval_test(
    %  'isEmpty() 2 - Full bag(int)',
    %  "0->asBag()->iterate(x;a=true|false)",
    %  [0]
    %),
    %eval_test(
    %  'isEmpty() 1 - Full OrderedSet(int)',
    %  "0->asOrderedSet()->including(3)->including(4)->iterate(x;a=true|false)",
    %  [0]
    %),
    %eval_test(
    %  'includes() 6 - Full Set(Set(int)) - the element is included',
    %  "no(Set(int))->asSequence()->including(no(int)->asSet()->including(3))->including(no(int)->asSet()->including(3)->including(7)->including(5))->iterate(x;a=false|a or (x = no(int)->asSet()->including(3)))",
    %  [1]
    %),
    %eval_test(
    %  'includes() 5 - Full Set(Set(int)) - the element is not included',
    %  "no(Set(int))->asSequence()->including(no(int)->asSet()->including(3))->including(no(int)->asSet()->including(3)->including(7)->including(5))->iterate(x;a=false| a or (x = no(int)))",
    %  [0]
    %),
    %eval_test(
    %  'isEmpty() 4 - Empty Set(Set(int))',
    %  "no(Set(int))->asSequence()->iterate(x;a=false| a or (x = no(int)))",
    %  [0]
    %),
    %eval_test(
    %  'includes() 3 - Empty sequence(int)',
    %  "no(int)->asSequence()->iterate(x;a=false|a or (x = 7))",
    %  [0]
    %),
    %eval_test(
    %  'includes() 2 - Full bag(int) - the element is not included',
    %  "0->asBag()->iterate(x;a=false|a or (x = 7))",
    %  [0]
    %),
    %eval_test(
    %  'includes() 1 - Full bag(int) - the element is included',
    %  "0->asBag()->including(2)->including(5)->iterate(x;a=false|a or (x = 2))",
    %  [1]
    %),
    %eval_test(
    %  'count() 6 - Full Set(Set(int)) - the element is included',
    %  "no(Set(int))->asSequence()->including(no(int)->asSet()->including(3))->including(no(int)->asSet()->including(3)->including(7)->including(5))->iterate(x;a=0|if (x = no(int)->asSet()->including(3)) then a+1 else a endif)",
    %  [1]
    %),
    %eval_test(
    %  'count() 5 - Full Set(Set(int)) - the element is not included',
    % "no(Set(int))->asSequence()->including(no(int)->asSet()->including(3))->including(no(int)->asSet()->including(3)->including(7)->including(5))->iterate(x;a=0| if x = no(int) then a+1 else a endif)",
    %  [0]
    %),
    %eval_test(
    %  'count() 4 - Empty Set(Set(int))',
    %  "no(Set(int))->asSequence()->iterate(x;a=0| if x= 5 then a+1 else a endif)",
    %  [0]
    %),
    %eval_test(
    %  'count() 3 - Empty sequence(int)',
    %  "no(int)->asSequence()->iterate(x;a=0|if x= 5 then a+1 else a endif)",
    %  [0]
    %),
    %eval_test(
    %  'count() 2 - Full bag(int) - the element is not included',
    %  "0->asBag()->including(7)->including(7)->iterate(x;a = 0 |if x=5 then a+1 else a endif)",
    %  [0]
    %),
    %eval_test(
    %  'count() 1 - Full bag(int) - the element is included',
    %  "0->asBag()->including(7)->including(7)->iterate(x;a = 0 |if x=7 then a+1 else a endif)",
    %  [2]
    %),
    %eval_test(
    %  'excluding() 7 - Full Set(Set(int)) - the element is included',
    %  "no(Set(int))->asSequence()->including(no(int)->asSet()->including(3))->including(no(int)->asSet()->including(3)->including(7)->including(5))->iterate(x;a=no(Set(int))->asSet()|if (x = no(int)->asSet()->including(3)) then a else a->including(x) endif)",
    %  [[5,7,3]]
    %),
    %eval_test(
    %  'excluding() 6 - Full Set(Set(int)) - the element is not included',
    %  "no(Set(int))->asSequence()->including(no(int)->asSet()->including(3))->including(no(int)->asSet()->including(3)->including(7)->including(5))->iterate(x;a=no(Set(int))->asSet() | if x = no(Set(int)) then a else a->including(x) endif)",
    %  [[5,7,3],[3]]
    %),
    %eval_test(
    %  'excluding() 5 - Empty Set(Set(int))',
    %  "no(Set(int))->asSequence()->iterate(x;a=no(Set(int))->asSet()| if x= no(Set(int)) then a else a->including(x) endif)",
    %  []
    %),
    %eval_test(
    %  'excluding() 4 - Empty sequence(int)',
    %  "no(int)->asSequence()->iterate(x;a=no(int)->asSet()|if x = 5 then a else a->including(x) endif)",
    %  []
    %),
    %eval_test(
    %  'excluding() 3 - Full bag(int) - the element is not included',
    %  "0->asBag()->including(2)->including(7)->iterate(x;a=no(int)->asSet()|if x=5 then a else a->including(x) endif)",
    %  [0,2,7]
    %),
    %eval_test(
    %  'excluding() 2 - Full bag(int) - the element is included',
    %  "2->asBag()->including(7)->including(7)->iterate(x;a=no(int)->asSet()|if x=7 then a else a->including(x) endif)",
    %  [2]
    %),
    %eval_test(
    %  'excluding() 1 - Full bag(int) - the element is not included',
    %  "0->asBag()->including(7)->including(7)->iterate(x;a=no(int)->asBag()|if x=3 then a else a->including(x) endif)",
    %  [0,7,7]
    %),
    %eval_test(
    %  'prepend() 3 - [[1,8]]->prepend [[1],[]]',
    %  "no(Sequence(int))->asSequence()->including(no(Sequence(int))->asSequence()->including(no(int)->asSequence()->including(1))->including(no(int)->asSequence()))->including(no(Sequence(int))->asSequence()->including(no(int)->asSequence()->including(1)->including(8)))",
    %  [[1],[],[1,8]]
    %),
    %eval_test(
    %  'prepend() 2 - [1,8]->prepend Empty sequence(int)',
    %  "no(int)->asSequence()->including(no(int))->including(no(int)->asSequence()->including(1)->including(8))",
    %  [1,8]
    %),
    %eval_test(
    %  'prepend() 1 - [1,8]->prepend([2,7])',
    %  "no(int)->asSequence()->including(no(int)->asSequence()->including(2)->including(7))->including(no(int)->asSequence()->including(1)->including(8))",
    %  [2,7,1,8]
    %),
    %eval_test(
    %  'flatten() 3 - flatten [[1],[],[1,8]]',
    %  "no(Sequence(int))->asSequence()->including(no(Sequence(int))->asSequence()->including(no(int)->asSequence()->including(1))->including(no(int)->asSequence()))->including(no(Sequence(int))->asSequence()->including(no(int)->asSequence()->including(1)->including(8)))->iterate(x;a=no(int)->asSequence()|a->including(x))",
    %  [1,1,8]
    %),
    %eval_test(
    %  'flatten() 2 - flatten [1,2,3]',
    %  "no(int)->asSequence()->including(1)->including(2)->including(3)->iterate(x;a=no(int)->asSequence()|a->including(x))",
    %  [1,2,3]
    %),
    %eval_test(
    %  'flatten() 1 - Empty sequence',
    %  "no(int)->asSequence()->iterate(x; a = no(int)->asSequence()| a->including(x))",
    %  []
    %),
    %eval_test(
    %  'includesAll() 6 - both full and included',
    %  "no(int)->asBag()->including(7)->including(3)->including(5)->asSet()->including(no(int)->asBag()->including(5)->including(7)) =no(int)->asBag()->including(7)->including(3)->including(5)->asSet()",
    %  [1]
    %),
    %eval_test(
    %  'includesAll() 5 - both full but only some included',
    %  "no(int)->asBag()->including(7)->including(3)->asSet()->including(no(int)->asBag()->including(7)->including(1)) =no(int)->asBag()->asSet()",
    %  [0]
    %),
   % eval_test(
    %  'includesAll() 4 - both full but not included',
     % "no(int)->asBag()->including(7)->asSet()->including(no(int)->asBag()->including(1)) =no(int)->asBag()->asSet()",
     % [0]
    %),
    %eval_test(
    % 'includesAll() 3 - e2 is empty',
    %  "no(int)->asBag()->including(7)->asSet()->including(no(int)->asBag()) = no(int)->asBag()->asSet()->including(7)",
    %  [1]
    %),
    %eval_test(
    %  'includesAll() 2 - Both e1 and e2 are empty',
    %  "no(int)->asBag()->asSet()->including(no(int)->asBag()) = no(int)->asBag()->asSet()",
    %  [1]
    %),
    %eval_test(
    %  'includesAll() 1 - e1 is an Empty bag',
    %  "no(int)->asBag()->asSet()->including(no(int)->asBag()->including(1)) =no(int)->asBag()->asSet()",
    %  [0]
    %),
    %eval_test(
    %  'any() 7 - e1 not empty and condition is satisfied',
    %  "no(Set(int))->asBag()->including(no(int)->asSet()->including(7)->including(5))->including(no(int)->asSet())->iterate(x;a=no(Set(int))|if x = no(int)->asSet() then x else a endif)",
    %  []
    %),
    %eval_test(
    %  'any() 6 - e1 not empty and condition is satisfied',
    %  "no(Set(int))->asBag()->including(no(int)->asSet()->including(7)->including(5))->including(no(int)->asSet())->iterate(x;a=no(Set(int))|if x = no(int)->asSet()->including(7)->including(5) then x else a endif)",
    %  [5,7]
    %),
    %eval_test(
    %  'any() 5 - e1 not empty but condition is not satisfied',
    %  "no(Set(int))->asBag()->including(no(int)->asSet()->including(5))->iterate(x;a=no(Set(int))|if x = no(int)->asSet()->including(7) then x else a endif)",
    %  []
    %),
    %eval_test(
    %  'any() 4 - e1 is an Empty bag',
    %  "no(Set(int))->asBag()->iterate(x;a=no(Set(int))|if x = no(int)->asSet() then x else a endif)",
    %  []
    %),
    %eval_test(
    %  'any() 3 - e1 not empty and condition is satisfied',
    %  "no(int)->asBag()->including(5)->including(7)->iterate(x;a=no(int)|if x = 7 then x else a endif)",
    %  [7]
    %),
    %eval_test(
    %  'any() 2 - e1 not empty but condition is not satisfied',
    %  "no(int)->asBag()->including(5)->iterate(x;a=no(int)|if x = 7 then x else a endif)",
    %  []
    %),
    %eval_test(
    %  'any() 1 - e1 is an Empty bag',
    %  "no(int)->asBag()->iterate(x;a=no(int)|if x = 7 then x else a endif)",
    %  []
    %),
    %eval_test(
    %  'any() 7 - e1 not empty and condition is satisfied',
    %  "no(Set(int))->asBag()->including(no(int)->asSet()->including(7)->including(5))->including(no(int)->asSet())->iterate(x;a=no(Set(Set(int)))|if x = no(int)->asSet() then no(Set(int))->asSet()->including(x) else a endif)",
    %  [ [ ] ]
    %),
    %eval_test(
    %  'any() 6 - e1 not empty and condition is satisfied',
    %  "no(Set(int))->asBag()->including(no(int)->asSet()->including(7)->including(5))->including(no(int)->asSet())->iterate(x;a=no(Set(Set(int)))|if x = no(int)->asSet()->including(7)->including(5) then no(Set(int))->asSet()->including(x) else a endif)",
    %  [ [5,7] ]
    %),
    %eval_test(
    %  'any() 5 - e1 not empty but condition is not satisfied',
    %  "no(Set(int))->asBag()->including(no(int)->asSet()->including(5))->iterate(x;a=no(Set(Set(int)))|if x = no(int)->asSet()->including(7) then no(Set(int))->asSet()->including(x) else a endif)",
    %  []
    %),
    %eval_test(
    %  'any() 4 - e1 is an Empty bag',
    %  "no(Set(int))->asBag()->iterate(x;a=no(Set(int))|if x = no(int)->asSet() then x else a endif)",
    %  []
    %),
    %eval_test(
    %  'any() 3 - e1 not empty and condition is satisfied',
    %  "no(int)->asBag()->including(5)->including(7)->iterate(x;a=no(Set(int))|if x = 7 then no(int)->asSet()->including(x) else a endif)",
    %  [7]
    %),
    %eval_test(
    %  'any() 2 - e1 not empty but condition is not satisfied',
    %  "no(int)->asBag()->including(5)->iterate(x;a=no(Set(int))|if x = 7 then no(int)->asSet()->including(x) else a endif)",
    %  []
    %),
    %eval_test(
    %  'any() 1 - e1 is an Empty bag',
    %  "no(int)->asBag()->iterate(x;a=no(Set(int))|if x = 7 then no(int)->asSet()->including(x) else a endif)",
    %  []
    %),
  ]).

test_list_rewrite( [
  rewrite_test(
      'Integer constant',
      int_const(1),
      int_const(1)
  ),
  rewrite_test(
      'Boolean constant',
      bool_const(1),
      bool_const(1)
  ),
  rewrite_test(
      'Identifier',
      ident("X"),
      ident("X")
  ), 
  rewrite_test(
     'Negation',
     not(bool_const(0)),
     exclusive_or(bool_const(0), bool_const(1))
  ),
  rewrite_test(
     'Change of sign',
     negative(int_const(1)),
     minus(int_const(0), int_const(1))
  )

  ]).


unit_test_all :-
  unit_test_parser(Total1, Passed1, Failed1),
  unit_test_type_check(Total2, Passed2, Failed2),
  unit_test_eval(Total3, Passed3, Failed3),
  unit_test_rewrite(Total4, Passed4, Failed4),
  Total  is Total1 + Total2 + Total3 + Total4,
  Passed is Passed1 + Passed2 + Passed3 + Passed4,
  Failed is Failed1 + Failed2 + Failed3 + Failed4,
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
  ( parse_oclc_expr( Input, Output ),
    format('Output:~w~n', [Output]),
    format('Expected:~w~n', [ExpectedOutput]),
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
  ( type_check_oclc_expr(Input, Output),
    Output = ExpectedOutput ->
    Passed is RestPassed + 1,
    Failed is RestFailed,
    format('- ~w: Passed~n', [Name])   
  ;
    type_check_oclc_expr(Input, Output),
    Passed is RestPassed,
    Failed is RestFailed + 1,
    format('- ~w: Failed~n', [Name]),
    format('  - Expected: ~w~n', [ExpectedOutput]),
    format('  - Obtained: ~w~n', [Output])     
  ).

unit_test_eval(Total, Passed, Failed) :-
  format('Starting eval test~n--------------------~n', []),
  test_list_eval( TestList ),
  write("Here"),
  do_test_eval( TestList, Passed, Failed),
  Total is Passed + Failed,
  format('--------------------~n', []),
  format('Eval tests~n Total : ~w~n Passed: ~w~n Failed: ~w~n~n', [Total, Passed, Failed]).

do_test_eval( [], 0, 0).
do_test_eval( [Test | Rest], Passed, Failed) :-
  Test = eval_test(Name, Input, ExpectedOutput),
  do_test_eval( Rest, RestPassed, RestFailed ),
  ( eval_oclc_expr(Input, Output), 
    Output = ExpectedOutput ->
    Passed is RestPassed + 1,
    Failed is RestFailed,
    format('- ~w: Passed~n', [Name])
  ;
    eval_oclc_expr(Input, Output), 
    Passed is RestPassed,
    Failed is RestFailed + 1,
    format('- ~w: Failed~n', [Name]),
    format('  - Expected: ~w~n', [ExpectedOutput]),
    format('  - Obtained: ~w~n', [Output])     
  ).
  
unit_test_rewrite(Total, Passed, Failed) :-
  format('Starting rewrite test~n--------------------~n', []),
  test_list_rewrite( TestList ),
  do_test_rewrite( TestList, Passed, Failed),
  Total is Passed + Failed,
  format('--------------------~n', []),
  format('Rewrite tests~n Total : ~w~n Passed: ~w~n Failed: ~w~n~n', [Total, Passed, Failed]).

do_test_rewrite( [], 0, 0).
do_test_rewrite( [Test | Rest], Passed, Failed) :-
  Test = rewrite_test(Name, Input, ExpectedOutput),
  do_test_rewrite( Rest, RestPassed, RestFailed ),
  ( translate_ocls_to_oclc(Input, Output), 
    Output = ExpectedOutput ->
    Passed is RestPassed + 1,
    Failed is RestFailed,
    format('- ~w: Passed~n', [Name])
  ;
    translate_ocls_to_oclc(Input, Output), 
    Passed is RestPassed,
    Failed is RestFailed + 1,
    format('- ~w: Failed~n', [Name]),
    format('  - Expected: ~w~n', [ExpectedOutput]),
    format('  - Obtained: ~w~n', [Output])     
  ).

    


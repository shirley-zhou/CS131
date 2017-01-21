type awksub_nonterminals =
  S | NP | VP | V | D | No

let accept_all derivation string = Some (derivation, string)
let accept_empty_suffix derivation = function
   | [] -> Some (derivation, [])
   | _ -> None

let awksub_rules =
   [S, [T"Hi"; N NP; N VP];
    NP, [N D; N No];
    VP, [N V ; N NP];
    No, [T"cat"];
    No, [T"dog"];
    V, [T"ate"];
    V, [T"bit"];
    D, [T"the"];
    D, [T"a"]]

let awksub_grammar = S, awksub_rules

let test_gram = convert_grammar awksub_grammar

let test_1 = ((parse_prefix test_gram accept_all ["Hi"; "the"; "cat"; "bit"; "a"; "dog"])
   = Some ([(S, [T"Hi"; N NP; N VP]); (NP, [N D; N No]); (D, [T"the"]); (No, [T"cat"]); (VP, [N V ; N NP]); (V, [T"bit"]); (NP, [N D; N No]); (D, [T"a"]); (No, [T"dog"])], []))

let test_11 = ((parse_prefix test_gram accept_empty_suffix ["Hi"; "the"; "cat"; "bit"; "a"; "dog"])
   = Some ([(S, [T"Hi"; N NP; N VP]); (NP, [N D; N No]); (D, [T"the"]); (No, [T"cat"]); (VP, [N V ; N NP]); (V, [T"bit"]); (NP, [N D; N No]); (D, [T"a"]); (No, [T"dog"])], []))

let test_12 = ((parse_prefix test_gram accept_all ["Hi"; "the"; "cat"; "bit"; "a"; "dog"; "!"])
   = Some ([(S, [T"Hi"; N NP; N VP]); (NP, [N D; N No]); (D, [T"the"]); (No, [T"cat"]); (VP, [N V ; N NP]); (V, [T"bit"]); (NP, [N D; N No]); (D, [T"a"]); (No, [T"dog"])], ["!"]))

let test_13 = ((parse_prefix test_gram accept_empty_suffix ["Hi"; "the"; "cat"; "bit"; "a"; "dog"; "!"])
   = None)


type awksub_nonterminals =
  | Expr | Term | Lvalue | Incrop | Binop | Num

let awkish_grammar =
  (Expr,
   function
     | Expr ->
         [[N Term; N Binop; N Expr];
          [N Term]]
     | Term ->
	 [[N Num];
	  [N Lvalue];
	  [N Incrop; N Lvalue];
	  [N Lvalue; N Incrop];
	  [T"("; N Expr; T")"]]
     | Lvalue ->
	 [[T"$"; N Expr]]
     | Incrop ->
	 [[T"++"];
	  [T"--"]]
     | Binop ->
	 [[T"+"];
	  [T"-"]]
     | Num ->
	 [[T"0"]; [T"1"]; [T"2"]; [T"3"]; [T"4"];
	  [T"5"]; [T"6"]; [T"7"]; [T"8"]; [T"9"]])

(*grammar underspecified for this case, ambiguous parse. But parses correctly based on required left-right parse*)
let test_2 =
  ((parse_prefix awkish_grammar accept_empty_suffix ["$"; "5"; "+"; "3"; "-"; "2"])
   = Some ([(Expr, [N Term; N Binop; N Expr]); (Term, [N Lvalue]);
   (Lvalue, [T "$"; N Expr]); (Expr, [N Term; N Binop; N Expr]);
   (Term, [N Num]); (Num, [T "5"]); (Binop, [T "+"]); (Expr, [N Term]);
   (Term, [N Num]); (Num, [T "3"]); (Binop, [T "-"]); (Expr, [N Term]);
   (Term, [N Num]); (Num, [T "2"])],
  []))

(*correct parse*)
let test_21 =
  ((parse_prefix awkish_grammar accept_empty_suffix ["$"; "5"; "+"; "3"])
   = Some ([(Expr, [N Term; N Binop; N Expr]); (Term, [N Lvalue]);
   (Lvalue, [T "$"; N Expr]); (Expr, [N Term]); (Term, [N Num]);
   (Num, [T "5"]); (Binop, [T "+"]); (Expr, [N Term]); (Term, [N Num]);
   (Num, [T "3"])],
  []))

let awkish_grammar_fail =
  (Expr,
   function
     | Expr ->
         [[N Expr; N Binop; N Term];
          [N Term]]
     | Term ->
	 [[N Num];
	  [N Lvalue];
	  [N Incrop; N Lvalue];
	  [N Lvalue; N Incrop];
	  [T"("; N Expr; T")"]]
     | Lvalue ->
	 [[T"$"; N Expr]]
     | Incrop ->
	 [[T"++"];
	  [T"--"]]
     | Binop ->
	 [[T"+"];
	  [T"-"]]
     | Num ->
	 [[T"0"]; [T"1"]; [T"2"]; [T"3"]; [T"4"];
	  [T"5"]; [T"6"]; [T"7"]; [T"8"]; [T"9"]])

  let test_3 =
  ((parse_prefix awkish_grammar_fail accept_empty_suffix ["$"; "5"; "+"; "3"])
   = Some ([(Expr, [N Term; N Binop; N Expr]); (Term, [N Lvalue]);
   (Lvalue, [T "$"; N Expr]); (Expr, [N Term]); (Term, [N Num]);
   (Num, [T "5"]); (Binop, [T "+"]); (Expr, [N Term]); (Term, [N Num]);
   (Num, [T "3"])],
  []))
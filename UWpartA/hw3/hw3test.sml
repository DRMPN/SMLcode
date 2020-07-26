(* Homework3 Simple Test*)
(* These are basic test cases_ Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname_sml"; *)
(* All the tests should evaluate to true_ For example, the REPL should say: val test1 = true : bool *)

(* assume all string have at least 1 char *)
val _ = print "\nTests for 1st funcion:\n"
val test1_1 = only_capitals ["A","B","C"] = ["A","B","C"];
val test1_2 = only_capitals ["aBc","ABc","Cde","fgh"] = ["ABc","Cde"];

val _ = print "\nTests for 2nd funcion:\n"
val test2_0 = longest_string1 [] = "";
val test2_1 = longest_string1 ["A","bc","C"] = "bc";
val test2_2 = longest_string1 ["A","bc","C","de"] = "bc";

val _ = print "\nTests for 3rd funcion:\n"
val test3_0 = longest_string2 [] = "";
val test3_1 = longest_string2 ["A","bc","C"] = "bc";
val test3_2 = longest_string2 ["A","bc","C","de"] = "de";

val _ = print "\nTests for 4th A funcion:\n"
val test4_0a = longest_string3 [] = "";
val test4_1a = longest_string3 ["A","bc","C"] = "bc";
val test4_2a = longest_string3 ["A","bc","C","de"] = "bc";

val _ = print "\nTests for 4th B funcion:\n"
val test4_0b = longest_string4 [] = "";
val test4_1b = longest_string4 ["A","bc","C"] = "bc";
val test4_2b = longest_string4 ["A","bc","C","de"] = "de";

val _ = print "\nTests for 5th funcion:\n"
val test5_0 = longest_capitalized [] = "";
val test5_1 = longest_capitalized ["A","bc","C"] = "A";
val test5_2 = longest_capitalized ["A","bc","C","CD"] = "CD";

val _ = print "\nTests for 6th funcion:\n"
val test6_0 = rev_string "" = "";
val test6_1 = rev_string "abc" = "cba";

val _ = print "\nTests for 7th funcion:\n"
val test7_0 = ((first_answer (fn x => if x > 3 then SOME x else NONE) []); false) handle NoAnswer => true;
val test7_1 = first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,4,5] = 4;

val _ = print "\nTests for 8th funcion:\n"
val test8_0 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [] = SOME [];
val test8_1 = all_answers (fn x => if x < 1 then SOME [x] else NONE) [0,1,2,3,4,5,6,7] = NONE;
val test8_2 = all_answers (fn x => if x > 1 then SOME [x] else NONE) [2,3,4] = SOME [2,3,4];

val _ = print "\nTests for 9th A funcion:\n"
val test9_0a = count_wildcards UnitP = 0;
val test9_1a = count_wildcards Wildcard = 1;
val test9_2a = count_wildcards (TupleP [Wildcard,(Variable "hi"),Wildcard]) = 2;

val _ = print "\nTests for 9th B funcion:\n"
val test9_0b = count_wild_and_variable_lengths UnitP = 0;
val test9_1b = count_wild_and_variable_lengths Wildcard = 1;
val test9_2b = count_wild_and_variable_lengths (TupleP [Wildcard,(Variable "hi"),(TupleP [Wildcard])]) = 4;

val _ = print "\nTests for 9th C funcion:\n"
val test9_0c = count_some_var ("x", Wildcard) = 0;
val test9_1c = count_some_var ("x", Variable("x")) = 1;
val test9_2c = count_some_var ("x", (TupleP [Wildcard,(Variable "x"),(Variable "c")])) = 1;

val _ = print "\nTests for 10th funcion:\n"
val test10_0 = check_pat UnitP = true;
val test10_1 = check_pat (Variable("x")) = true;
val test10_2 = check_pat (TupleP [Wildcard,(Variable "x"),(Variable "c"),(ConstructorP ("yo",Wildcard))]) = true;

val _ = print "\nTests for 11th funcion:\n"
val test11_0 = match (Tuple [], TupleP []) = SOME [];
val test11_1 = match (Const(1), UnitP) = NONE;
val test11_2 = match (Tuple [Const 1,Unit], TupleP [ConstP 1]) = NONE;
val test11_3 = match (Tuple [Const 1,Const 0], TupleP [ConstP 1,Variable "x"]) = SOME [("x",Const 0)];

val _ = print "\nTests for 12th funcion:\n"
val test12_0 = first_match Unit [] = NONE;
val test12_1 = first_match Unit [UnitP] = SOME [];
val test12_2 = first_match (Const 0) [UnitP,Variable "x"] = SOME [("x", Const 0)];

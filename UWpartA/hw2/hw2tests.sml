(* Homework2 Simple Test *)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

val test1 = all_except_option ("string", ["string"]) = SOME []

val test2 = get_substitutions1 ([["foo"],["there"]], "foo") = []

val test3 = get_substitutions2 ([["foo"],["there"]], "foo") = []

val test4 = similar_names ([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], {first="Fred", middle="W", last="Smith"}) =
	    [{first="Fred", last="Smith", middle="W"}, {first="Fredrick", last="Smith", middle="W"},
	     {first="Freddie", last="Smith", middle="W"}, {first="F", last="Smith", middle="W"}]

val test5 = card_color (Clubs, Num 2) = Black

val test6 = card_value (Clubs, Num 2) = 2

val test7 = remove_card ([(Hearts, Ace)], (Hearts, Ace), IllegalMove) = []

val test8 = all_same_color [(Hearts, Ace), (Hearts, Ace)] = true

val test9 = sum_cards [(Clubs, Num 2),(Clubs, Num 2)] = 4

val test10 = score ([(Hearts, Num 2),(Clubs, Num 4)],10) = 4

val test11 = officiate ([(Hearts, Num 2),(Clubs, Num 4)],[Draw], 15) = 6

val test12 = officiate ([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],
                        [Draw,Draw,Draw,Draw,Draw],
                        42)
             = 3

val test13 = ((officiate([(Clubs,Jack),(Spades,Num(8))],
                         [Draw,Discard(Hearts,Jack)],
                         42);
               false)
              handle IllegalMove => true)


(* >>> Test for Problem 1 <<< *)

(* Tests for (a) *)
val A0 = all_except_option ("kek",[]) = NONE;
val A1 = all_except_option ("string", ["string"]) = SOME [];
val A2 = all_except_option ("hey",["hello","hey","there"]) = SOME ["hello","there"];
val A_ = all_except_option ("wow",["why","are","you","reading","this"]) = NONE;

(* Tests for (b) *)
val B0 = get_substitutions1 ([],"") = [];
val B1 = get_substitutions1 ([["hey","you"],["you"]],"you") = ["hey"];
val B2 = get_substitutions1 ([["foo"],["there"]], "foo") = [];
val B_ = get_substitutions1 ([["hello"],["how","are","you"],["today"]],"stop") = [];

(* Tests for (c) *)
val C0 = get_substitutions2 ([],"") = [];
val C1 = get_substitutions2 ([["hey","you"],["you"]],"you") = ["hey"];
val C2 = get_substitutions2 ([["foo"],["there"]], "foo") = [];
val C_ = get_substitutions2 ([["hello"],["how","are","you"],["today"]],"stop") = [];

(* Tests for (d) *)
val D0 = similar_names ([],{first="",middle="",last=""}) = [{first="",middle="",last=""}];
val D1 = similar_names ([["Fred","Fredrick"]], {first="Fred", middle="W", last="Smith"}) = [{first="Fred", last="Smith", middle="W"}, {first="Fredrick", last="Smith", middle="W"}];
val D2 = similar_names ([["Fred","Fredrick"],["Ben","Fred"]], {first="Fred", middle="W", last="Smith"}) = [{first="Fred", last="Smith", middle="W"}, {first="Fredrick", last="Smith", middle="W"},{first="Ben", last="Smith", middle="W"}];
val D_ = similar_names ([["Fred","Fredrick"],["Ben","Fred"]], {first="Greg", middle="W", last="Smith"}) = [{first="Greg", middle="W", last="Smith"}];

(* Coursera Programming Languages, Homework 3, Provided Code *)
(* Homework assignment 3 | UWpartA | by SID 2020 *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let
	val r = g f1 f2
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end;

(**** you can put all your code here ****)

(* >>> Exercise 1 <<< *)
val only_capitals = List.filter (fn x => (Char.isUpper o String.sub) (x,0));

(* >>> Exercise 2 <<< *)
val longest_string1 = List.foldl (fn (x,acc) => if String.size x > String.size acc
                                                then x else acc) "";

(* >>> Exercise 3 <<< *)
val longest_string2 = List.foldl (fn (x,acc) => if String.size x >= String.size acc
                                                then x else acc) "";

(* >>> Exercise 4 <<< *)
fun longest_string_helper f = foldl (fn (x,acc) => if f (String.size x, String.size acc)
                                                   then x else acc) "";
val longest_string3 = longest_string_helper op >;
val longest_string4 = longest_string_helper op >=;

(* >>> Exercise 5 <<< *)
val longest_capitalized = longest_string3 o only_capitals;

(* >>> Exercise 6 <<< *)
val rev_string = String.implode o List.rev o String.explode;

(* >>> Exercise 7 <<< *)

fun first_answer f xs =
    case List.filter isSome (List.map f xs) of
        (SOME x)::xs => x
      | _ => raise NoAnswer;

(* >>> Exercise 8 <<< *)

fun all_answers f xs=
    let fun aux acc xs = case xs of
                             [] => SOME acc
                           | NONE::_ => NONE
                           | SOME x :: xs => aux (acc@x) xs
    in aux [] (List.map f xs)
    end;





(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

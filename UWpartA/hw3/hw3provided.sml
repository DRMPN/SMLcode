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

(* >>> Exercise 9 <<< *)
 (* >> Funciton A << *)
val count_wildcards = g (fn x => 1) (fn y => 0);

 (* >> Funciton B << *)
val count_wild_and_variable_lengths = g (fn x => 1) (fn x => String.size x);

 (* >> Funciton C << *)
fun count_some_var (s,p) = g (fn x => 0) (fn x => if s = x then 1 else 0) p;

(* >>> Exercise 10 <<< *)
(* same as g but produces list instead of int*)
fun f f1 f2 p =
    let val r = f f1 f2
    in case p of
	         Wildcard          => f1 ()
	       | Variable x        => f2 x
	       | TupleP ps         => List.foldl (fn (p,i) => i @ (r p)) [] ps
	       | ConstructorP(_,p) => r p
	       | _                 => []
    end;

(* no_duplicates -> string list -> bool *)
val no_duplicates =
    let fun aux acc xs =
            case xs of
                [] => acc
              | x::xs => aux (acc andalso (not (List.exists (fn a : string => a = x) xs))) xs
    in aux true end;

val check_pat = no_duplicates o f (fn x => []) (fn x => [x]);

(* >>> Exercise 11 <<< *)
(* match : valu * pattern -> (string * valu) list option *)
fun match v_p =
    case v_p of
        (_, Wildcard) => SOME []
      | (v, Variable s) => SOME [(s,v)]
      | (Unit, UnitP) => SOME []
      | (Const i1, ConstP i2) => if i1 = i2 then SOME [] else NONE
      | (Tuple vs, TupleP ps) => if List.length vs = List.length ps
                                 then (all_answers match o ListPair.zip) (vs,ps) else NONE
      | (Constructor (s1,v), ConstructorP (s2,p)) => if s1 = s2 then match (v,p) else NONE
      | _ => NONE;

(* Didn't understand correctly
      | (Constructor (s1,v), ConstructorP (s2,p)) => if s1 = s2 andalso (isSome o match) (v,p)
                                                     then SOME [(s1,v)] else NONE
*)

(* >>> Exercise 12 <<< *)
fun first_match v lop = (SOME (first_answer (fn x => match (v,x)) lop)) handle NoAnswer => NONE;

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

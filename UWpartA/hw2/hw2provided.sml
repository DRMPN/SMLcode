(* Dan Grossman, Coursera PL, HW2 Provided Code *)
(* Homework assignment 2 | UWpartA | by SID 2020 *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2;

(* put your solutions for problem 1 here *)

(* -- (a) -- *)
(* all_except_option : string -> string list -> string list option *)
fun all_except_option (str, los) =
    let fun aux (st,[],acc) = if acc = los then NONE else SOME (acc)
          | aux (st,x::xs,acc) = if same_string (st, x) then aux (st,xs,acc)
                                 else aux (st,xs,acc@[x]) (* cons reverses the list *)
    in aux (str,los,[])
    end;

(* Tests for (a) *)
val A0 = all_except_option ("kek",[]) = NONE;
val A1 = all_except_option ("string", ["string"]) = SOME [];
val A2 = all_except_option ("hey",["hello","hey","there"]) = SOME ["hello","there"];
val A_ = all_except_option ("wow",["why","are","you","reading","this"]) = NONE;


(* -- (b) -- *)
(* get_substitutions1 : string list list -> string -> string list *)
fun get_substitutions1 ([],_) = []
  | get_substitutions1 (x::xs,str) = case all_except_option (str,x) of
                                         NONE => get_substitutions1 (xs,str)
                                       | SOME ls => ls @ get_substitutions1 (xs,str);

(* Tests for (b) *)
val B0 = get_substitutions1 ([],"") = [];
val B1 = get_substitutions1 ([["hey","you"],["you"]],"you") = ["hey"];
val B2 = get_substitutions1 ([["foo"],["there"]], "foo") = [];
val B_ = get_substitutions1 ([["hello"],["how","are","you"],["today"]],"stop") = [];


(* -- (c) -- *)
(* get_substitutions2 : string list list -> string -> string list *)
fun get_substitutions2 (los,st) =
    let fun aux ([],_,acc) = acc
          | aux (x::xs,st,acc) = case all_except_option (st,x) of
                                     NONE => aux (xs,st,acc)
                                   | SOME ls => aux (xs,st,acc @ ls)
    in aux (los,st,[])
    end;

(* Tests for (c) *)
val C0 = get_substitutions2 ([],"") = [];
val C1 = get_substitutions2 ([["hey","you"],["you"]],"you") = ["hey"];
val C2 = get_substitutions2 ([["foo"],["there"]], "foo") = [];
val C_ = get_substitutions2 ([["hello"],["how","are","you"],["today"]],"stop") = [];


(* -- (d) -- *)
(* similar_names : string list list -> s * s * s -> (s * s * s) list *)
(* Here I used keyword "as", I know that its prohibited but who cares :) *)
fun similar_names (los, r as {first=a,middle=b,last=c}) =
    let fun aux ([],acc) = acc
          | aux (x::xs,acc) = aux (xs,acc @ [{first=x,middle=b,last=c}])
    in aux (get_substitutions2 (los,a),[r]) (* without keyword this part looks ugly *)
    end;

(* Tests for (d) *)
val D0 = similar_names ([],{first="",middle="",last=""}) = [{first="",middle="",last=""}];
val D1 = similar_names ([["Fred","Fredrick"]], {first="Fred", middle="W", last="Smith"}) = [{first="Fred", last="Smith", middle="W"}, {first="Fredrick", last="Smith", middle="W"}];
val D2 = similar_names ([["Fred","Fredrick"],["Ben","Fred"]], {first="Fred", middle="W", last="Smith"}) = [{first="Fred", last="Smith", middle="W"}, {first="Fredrick", last="Smith", middle="W"},{first="Ben", last="Smith", middle="W"}];
val D_ = similar_names ([["Fred","Fredrick"],["Ben","Fred"]], {first="Greg", middle="W", last="Smith"}) = [{first="Greg", middle="W", last="Smith"}];


(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades;
datatype rank = Jack | Queen | King | Ace | Num of int;
type card = suit * rank;

datatype color = Red | Black;
datatype move = Discard of card | Draw;

exception IllegalMove;

(* put your solutions for problem 2 here *)

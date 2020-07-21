(* Dan Grossman, Coursera PL, HW2 Provided Code *)
(* Homework assignment 2 | UWpartA | by SID 2020 *)

(* >>>------------------------- Problem 1 -------------------------<<< *)

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

(* -- (b) -- *)
(* get_substitutions1 : string list list -> string -> string list *)
fun get_substitutions1 ([],_) = []
  | get_substitutions1 (x::xs,str) =
    case all_except_option (str,x) of
        NONE => get_substitutions1 (xs,str)
      | SOME ls => ls @ get_substitutions1 (xs,str);

(* -- (c) -- *)
(* get_substitutions2 : string list list -> string -> string list *)
fun get_substitutions2 (los,st) =
    let fun aux ([],_,acc) = acc
          | aux (x::xs,st,acc) =
            case all_except_option (st,x) of
                NONE => aux (xs,st,acc)
              | SOME ls => aux (xs,st,acc @ ls)
    in aux (los,st,[])
    end;

(* -- (d) -- *)
(* similar_names : string list list -> s * s * s -> (s * s * s) list *)
(* Here I used keyword "as", I know that its prohibited but who cares :) *)
fun similar_names (los, r as {first=a,middle=b,last=c}) =
    let fun aux ([],acc) = acc
          | aux (x::xs,acc) = aux (xs,acc @ [{first=x,middle=b,last=c}])
    in aux (get_substitutions2 (los,a),[r]) (* without keyword this part looks ugly *)
    end;

(* >>>------------------------- Problem 2 -------------------------<<< *)

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades;
datatype rank = Jack | Queen | King | Ace | Num of int;
type card = suit * rank;

datatype color = Red | Black;
datatype move = Discard of card | Draw;

exception IllegalMove;

(* put your solutions for problem 2 here *)

(* -- (a) -- *)
fun card_color (suit,_) =
    case suit of
        Clubs => Black
      | Spades => Black
      | _ => Red;

(* -- (b) -- *)
fun card_value (_,rank) =
    case rank of
        Num x => x
      | Ace => 11
      | _ => 10;

(* -- (c) -- *)
fun remove_card (loc,c : card,e) =
    let
        fun aux ([],ys) = raise e
          | aux (x::xs,ys) = if x = c then xs@ys
                             else aux (xs,[x]@ys)
    in
        aux (loc,[])
    end;

(* -- (d) -- *)
fun all_same_color [] = true
  | all_same_color (x::xs) =
    let
        fun aux ([],_) = true
          | aux (x::xs,c) = card_color x = c andalso aux (xs,c)
    in aux (xs,card_color x)
    end;

(* -- (e) -- *)
fun sum_cards loc =
    let
        fun aux ([],acc) = acc
          | aux (x::xs,acc) = aux (xs,acc+card_value x)
    in aux (loc,0)
    end;

(* -- (f) -- *)
fun score (loc,goal) =
    let
        val sum = sum_cards loc
        val preliminary_score =
            if sum > goal then 3 * (sum - goal)
            else (goal - sum)
    in
        if all_same_color loc then preliminary_score div 2
        else preliminary_score
    end;

(* -- (g) -- *)
(* officiate takes list of cards, list of moves, goal *)
(* return the score at the end of the game *)
fun officiate (loc,lom,goal) =
    let fun aux (loc,[],lohc) = score (lohc,goal)                            (* game ends if there no more moves *)
          | aux (loc,m::ms,lohc) =
            case m of
                Discard c => aux (loc, ms, remove_card (lohc,c,IllegalMove)) (* play continues or raise an exception *)
              | _ => case loc of
                         [] => score (lohc,goal)                             (* game is over if the player draws and the card list is empty *)
                       | (x::xs) => if sum_cards (x::lohc) > goal            (* if drawing causes the sum of held exceed the goal, the game is over*)
                                    then score (x::lohc,goal)
                                    else aux (xs,ms,x::lohc)                 (* play continues *)
    in aux (loc,lom,[])                                                      (* >> start of the game << *)
    end;

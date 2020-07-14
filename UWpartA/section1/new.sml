(* Test file to get used to syntax and semantics *)

(* How to define variables *)
val x = 34

val y = 17

val z = x + y

val abs_of_z = if z < 0 then 0 - z else z;

(*
How to define funtions
Function over variables
*)

fun pow (x:int, y:int)=
    if y = 0
    then 1
    else x * pow (x, y-1);

(* Functions over tuples *)

fun swap (pr : int*bool) =
    (#2 pr, #1 pr);

fun sum_two_pairs (pr1 : int*int, pr2 : int*int)
    = ((#1 pr1 + #1 pr2), (#2 pr1 + #2 pr2));

fun div_mod (x:int, y:int) =
    (x div y, x mod y);

fun sort_pair (pr:int*int)=
    if (#1 pr) < (#2 pr)
    then pr
    else (#2 pr, #1 pr);

(* Functions over lists *)

fun sum_list (xs : int list) =
    if null xs
    then 0
    else hd xs + sum_list (tl xs);

fun prod_list (xs : int list) =
    if null xs
    then 1
    else hd xs * prod_list (tl xs);

fun countdown (x:int) =
    if x = 0
    then []
    else x::countdown(x-1);

fun append (xs : int list, ys : int list) =
    if null xs
    then ys
    else (hd xs) :: append ((tl xs), ys);

(* Functions over lists of tuples *)

fun sum_pair_list (xs : (int * int) list) =
    if null xs
    then 0
    else #1 (hd xs) + #2 (hd xs) + sum_pair_list (tl xs);

fun firsts (xs : (int * int) list) =
    if null xs
    then []
    else #1 (hd xs) :: firsts (tl xs);

(* Let experssions *)

fun le (x : int) =
    let
        val y = 2
    in
        x * y
    end;

fun countup_from1 (x : int) =
    let
        fun count (from) =
            if from = x
            then x :: []
            else from :: count (from+1)
    in
        count (1)
    end;

fun max_list (xs : int list) =
    if null xs
    then 0
    else if null (tl xs)
    then hd xs
    else
        let
            val tl_ans = max_list (tl xs)
        in
            if hd xs > tl_ans
            then hd xs
            else tl_ans
        end;

(* Monads *)

fun monad_max_list (xs : int list) =
    if null xs
    then NONE
    else
        let val tl_ans = monad_max_list (tl xs)
        in
            if isSome tl_ans andalso valOf tl_ans > hd xs
            then tl_ans
            else SOME (hd xs)
        end;

fun better_max_list (xs : int list) =
    if null xs
    then NONE
    else
        let
            fun none_empty (xs : int list) =
                if null (tl xs)
                then hd xs
                else
                    let
                        val tl_ans = none_empty (tl xs)
                    in
                        if hd xs > tl_ans
                        then hd xs
                        else tl_ans
                    end;
        in
            SOME (none_empty xs)
        end;

(* UWpartA | Homewoek assignment #1 | by SID *)

(* No 1. *)
(* is_older : date -> date -> bool *)
(* Too many conditionals, don't like it *)
fun is_older ( fst : int*int*int, snd : int*int*int ) =
        if #1 fst > #1 snd then false else
        if #1 fst < #1 snd then true else
        if #2 fst > #2 snd then false else
        if #2 fst < #2 snd then true else
        #3 fst < #3 snd

(* No 2. *)
(* better to do this with map or gurdians or case of *)
(* number_in_month : [dates] -> month -> int *)
fun number_in_month (xs : (int*int*int) list, n : int) =
    if null xs
    then 0
    else
        if #2 (hd xs) = n
        then 1 + number_in_month (tl xs, n)
        else number_in_month (tl xs, n);

(* No 3. *)
(* number_in_months : [dates] -> [months] -> int *)
fun number_in_months (xs : (int*int*int) list, ys : int list) =
    if null ys
    then 0
    else
        number_in_month (xs, hd ys) + number_in_months (xs, tl ys);

(* No 4. *)
(* Did it with mutual recursion
   also might do this with filter or with list comprehension *)
(* dates_in_month : [dates] -> month -> [dates] *)
fun dates_in_month (xs : (int*int*int) list, m : int) =
    if null xs
    then []
    else
        let fun isEqual (d : (int*int*int), m : int) =
                if (#2 d) = m
                then d :: dates_in_month (tl xs, m)
                else dates_in_month (tl xs, m)
        in isEqual (hd xs, m)
        end;

(* No 5. *)
(* dates_in_months : [dates] -> [months] -> [dates] *)
fun dates_in_months ( xs : (int*int*int) list, ys : int list) =
    if null ys
    then []
    else dates_in_month (xs, hd ys) @ dates_in_months (xs, tl ys);

(* No 6. *)
(* get_nth : [string] -> int -> string *)
fun get_nth (xs : string list, n : int) =
    if n = 1
    then hd xs
    else get_nth (tl xs, n-1);

(* No 7. *)
(* Fixed error *)
(* date_to_string : date -> string *)
fun date_to_string (date : (int*int*int)) =
    let
        val months = ["January ","February ","March ","April ","May ","June ","July ","August ","September ","October ","November ","December "]
        fun toStr (x:int) = Int.toString x
    in get_nth (months, (#2 date)) ^ toStr (#3 date) ^ ", " ^ toStr (#1 date)
    end;

(* No 8. *)
(* number_before_reaching_sum : int -> [int] -> int *)
(* Don't like what I've written at all, I can't understand what he wants *)
fun number_before_reaching_sum (sum : int, xs : int list) =
    let
        fun sum_elem (n : int, ns : int list) =
            if null ns orelse (sum < n orelse (n + hd ns) >= sum) (* BAD!? *)
            then 0
            else 1 + sum_elem (n + hd ns, tl ns)
    in sum_elem (0,xs)
    end;

(* No 9. *)
(* Fixed error *)
(* what_month : int -> int *)
fun what_month (x:int) =
    let
        val dates = [31,28,31,30,31,30,31,31,30,31,30,31]
    in
        1 + number_before_reaching_sum (x, dates)
    end;

(*
Might try this one if previous fails
fun what_month_poo (x : int) =
    ceil (Real.fromInt x / 31.0)
*)

(* No 10. *)
(* Fixed error *)
(* month_range : int -> int -> [int] *)
fun month_range (d1 : int, d2 : int) =
    if d1 > d2
    then []
    else what_month (d1) :: month_range (d1+1,d2);

(* No 11. *)
(* TODO [(5,5,2),(5,10,2),(5,2,2),(5,12,2)] *)
(* AGAIN mutual recursion :P *)
(* oldest : [(int,int,int)] -> (int,int,int) option *)
fun oldest (xs : (int*int*int) list) =
    if null xs
    then NONE
    else if null (tl xs)
    then SOME (hd xs)
    else
        let
            fun helper (date, lod, acc) =
                if null lod
                then oldest acc
                else if is_older (date, hd lod)
                then SOME (date)
                else helper (hd lod, tl lod, acc)
        in (helper (hd xs, tl xs, tl xs))
        end;

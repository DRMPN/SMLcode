(* Higer-order functions *)

fun n_times (f,n,x) =
    if n = 0 then x
    else f (n_times (f, n-1, x));

fun triple x = 3 * x;

fun triple_n_times (n,x) = n_times (triple, n, x);

fun map (f, xs) =
    case xs of
        [] => []
      | x::xs => f x :: map (f,xs);

fun filter (f, xs) =
    case xs of
        [] => []
      | (x::xs) => if f x then x::filter (f,xs)
                   else filter (f,xs);
(* left fold *)
fun fold (f,acc,xs) =
    case xs of
        [] => acc
      | x::xs => fold (f, f(x,acc), xs);

(* Anonymous expression synax *)

fun anon_triple_n_times (n,x) = n_times (fn x => x*3, n, x);

(* rev : 'a list -> 'a list *)
val rev = List.rev;

(* Closure and lexical scoping *)
val x = 1;
fun f y = x + y;
val x = 2;
val y = 3;
val z = f (x + y);

(* Recomputation *)
fun allShorterThan1 (xs,s) =
    filter (fn x => String.size x < (print "!"; String.size s),xs);

fun allShorterThan2 (xs,s) =
    let val i = (print "!"; String.size s)
    in filter (fn x => String.size x < i, xs)
    end;

val _ = print "\nwith allShorterThan1: ";
val x1 = allShorterThan1 (["1","333","22","4444"],"xxx");
val _ = print "\nwith allShorterThan2: ";
val x2 = allShorterThan2 (["1","333","22","4444"],"xxx");
val _ = "\n"

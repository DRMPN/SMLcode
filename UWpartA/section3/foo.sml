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

(*
Closure and lexical scoping
Closure : 1. Code (func body)
          2, Environment (variables)
*)
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
val _ = "\n";

(* Function composition *)
fun compose (f,g) = fn x => f (g x);

(* fun sqrt_of_abs i = (Math.sqrt o Real.fromInt o abs) i; *)
val sqrt_of_abs = Math.sqrt o Real.fromInt o abs;

infix !>;
fun x !> f = f x;

fun new_sqrt_of_abs i = i !> abs !> Real.fromInt !> Math.sqrt;

(* Currying *)

(* non-curry *)
fun sorted3_tuple (x,y,z) = x <= y andalso y <= z;
val cur1 = sorted3_tuple (3,5,7);

(* curry *)
val sorted3 = fn x => fn y => fn z => x <= y andalso y <= z;
val cur2 = (((sorted3 3) 5) 7);

(* and with some sugar *)
(*  sorted3_nicer(x,y,z)= x <= y andalso y <= z *)
fun sorted3_nicer x y z = x <= y andalso y <= z;
val cur3 = sorted3_nicer 3 5 7;

(* Partial application *)
(* Meh style *)
fun is_nonnegative_inferior x = sorted3 0 0 x;
(* fun sum_inferior xs = fold (fn (x,y) => x + y) 0 xs; *)

(* Nice style*)
val isnonnegative = sorted3 0 0;
(* val sum = fold (fn (x,y) => x + y) 0; *)

(* >value restrictions

Error:
'a list -> ('a,int) list
val pairWithOne = List.map (fn x => (x,1));

workaround :
val pairWithOne xs = List.map (fn x => (x,1)) xs
or
val pairWithOne : string list -> (string * int) list = ...
*)

(* References (mutable data) *)
val r0 = ref 42;
val r1 = ref 42;
val try1 = !r0 + !r1;
val r2 = r0 (* alias *)
val _ = r0 := 43;
val try2 = !r1 + !r2;

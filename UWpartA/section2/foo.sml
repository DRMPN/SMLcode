
(* Record *)
val x = { bar=(9-4,true), bow=2+2 , cow=("hello",false) };

val my_friend = { name = "Amelia", bday = (01,01,2001)};


(* Syntactic sugar *)
(* Tuples are SyntSug for Records *)
val unusual_pair = {2=5,1=6};


(* Data bindings *)
datatype myperson = Name of string
       | Age of int
       | Gender;

val a = Name "KeK";
val b = Name;
val c = Age 7;


(* Case Expressions *)
(* myfun : mytype -> string *)
fun myfun x =
    case x of
        Gender => "There's only one gender xD"
      | Name nm => nm
      | Age y => Int.toString y;


datatype suit = Club | Diamond | Heart | Spade;
datatype rank = Jack | Queen | King | Ace | Num of int;


(* Expression Trees *)
datatype exp = Constant of int
             | Negate of exp
             | Add of exp * exp
             | Multiply of exp * exp;

(* solve_exp (Add (Constant (10+9), Negate (Constant 4))); *)
fun solve_exp e =
    case e of
        Constant ex => ex
      | Negate ex => ~ (solve_exp ex)
      | Add (ex1,ex2) => (solve_exp ex1) + (solve_exp ex2)
      | Multiply (ex1, ex2) => (solve_exp ex1) * (solve_exp ex2);


(* Type Synonyms *)
type date = (int*int*int);

type card = suit * rank;


(* Recursive datatypes *)
datatype my_int_list = Empty | Cons of int * my_int_list;

fun append_my_list (xs,ys) =
    case xs of
        Empty => ys
      | Cons (x,xs') => Cons (x, append_my_list (xs',ys));

(* NONE and SOME are just constructors *)
fun test_option x =
    case x of
        NONE => 0
      | _ => 1;

(* List constructors and pattern matching *)
fun sum_list xs =
    case xs of
        [] => 0
      | (x::xn) => x + sum_list (xn);

fun append (xs,ys) =
    case xs of
        [] => []
      | (x::xn) => x :: append (xn,ys);

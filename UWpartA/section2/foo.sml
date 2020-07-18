
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
(* append : 'a list -> 'a list -> 'a list *)
fun append (xs,ys) =
    case xs of
        [] => []
      | (x::xn) => x :: append (xn,ys);


(* Polymorphic datatypes *)
datatype ('a,'b) tree =
         Leaf of 'b
         | Node of 'a * ('a,'b) tree * ('a,'b) tree;

(* sum_tree : int * int tree -> int *)
fun sum_tree tr=
    case tr of
        Leaf e => e
      | Node (e,l,r) => e + sum_tree l + sum_tree r;

fun num_leaves tr =
    case tr of
        Leaf e => e
      | Node (_,l,r) => num_leaves l + num_leaves r;

(* Extend of pattern matching *)
fun sum_triple_okay triple =
    let val (x,y,z) = triple
    in x + y + z
    end;

fun sum_triple (x,y,z) =
    x + y + z;

(* special_eq : ''a * ''a -> string *)
fun special_eq (x,y) =
    if x=y then "yes" else "no";

(* Nested patterns *)
exception ListLengthMismatch;

fun zip3 lot =
    case lot of
        ([],[],[]) => []
      | (x::xs, y::ys, z::zs) => (x,y,z) :: zip3 (xs,ys,zs)
      | _ => raise ListLengthMismatch;

fun unzip3 lot =
    case lot of
        [] => ([],[],[])
      | (x,y,z)::ls => let val (l1,l2,l3) = unzip3 ls
                       in
                           (x::l1,b::l2,c::l3)
                       end;

fun non_decr xs =
    case xs of
        [] => true
      | _::[] => true
      | head::(neck::tail) => head <= neck andalso non_decr (neck::tail);

  (* Wildcard example *)
fun length xs =
    case xs of
        [] => 0
      | _::xn => 1 + length xn;

(* My favorite??? *)
fun append_fav ([],ys) = ys
  | append_fav (x::xs,ys) = x :: append (xs,ys);

(* Exceptions *)
exception AnyNameIWant;

fun mydiv (x,y) =
    if y = 0
    then raise AnyNameIWant
    else x div y;
val div_zero = mydiv (2,0) handle AnyNameIWant => 1;

(* Tail recursion *)
fun fac n =
let fun aux (n,acc) =
        if n = 0
        then acc
        else aux (n-1,acc*n)
in
    aux (n,1)
end;

(* Metodology
 1) Create a helper function that takes an acc
 2) Old base case becomes initial acc
 3) New base case becomes final acc*)

fun not_rec_sum xs =
    case xs of
        [] => 0
      | x::xs => x + not_rec_sum xs;

fun rec_sum xs =
    let fun aux (xs,acc)=
            case xs of
                [] => acc
              | x::xs => aux (xs,acc+x)
    in aux (xs,0)
    end;

fun reverse xs =
    let fun aux ([],acc) = acc
          | aux (x::xs,acc) = aux (xs,x::acc)
    in aux (xs,[])
    end;

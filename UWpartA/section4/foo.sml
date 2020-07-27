signature MR =
sig
    val match : int list -> bool
(* everything else is hidden and can not be used outside of the module *)
end;

structure MutualRecursion :> MR =
(* Call anything with ModuleName.bindingName *)
struct

(* Mutual recursion *)
  (* Silly example *)
val match =
    let
        fun s_need_one xs =
            case xs of
                [] => true
              | 1::xs => s_need_two xs
              | _ => false
        and s_need_two xs =
            case xs of
                [] => false
              | 2::xs => s_need_one xs
              | _ => false;
    in s_need_one
    end;

  (* Recursive definition *)
datatype t1 = Foo of int | Bar of t2
     and t2 = Baz of string | Quux of t1;

fun no_zeros_or_empty_strings_t1 x =
    case x of
        Foo i => i <> 0
      | Bar y => no_zeros_or_empty_strings_t2 y
and no_zeros_or_empty_strings_t2 x =
    case x of
        Baz s => s <> ""
      | Quux y => no_zeros_or_empty_strings_t1 y;

  (* alternate way (slow) is to pass a function as an argument *)
fun no_zero_empty_t1 (f,x) =
    case x of
        Foo i => i <> 0
      | Bar y => f y;

fun no_zero_empty_t2 x =
    case x of
        Baz s => size s > 0
      | Quux y => no_zero_empty_t1 (no_zero_empty_t2, y);

end;

(* to open whole module use: open ModuleName *)
val right_sequence = MutualRecursion.match [1,2,1,2];
val wrong_sequence = MutualRecursion.match [1,2,3];
(* val error_module = MutualRecursion.no_zeros_or_empty_strings_t1 (Foo 1); *)

(* Extend is an auxiliary function. It takes a list of elements (e0,e1,...,en)
   and a value x and creates a list of pairs ((x,e0),(x,e1),...(x,en))
 *)
fun extend x nil = nil
  | extend x (h::t) = (x,h)::(extend x t)
      
(* Induction on the first list. 
   base case: result is the empty list
   induction: extend b with x and concatenate the result with the cross
              product of the tail (xs) by b. 
*)
fun cross nil     b = nil
  | cross (x::xs) b = (extend x b)@(cross xs b)




fun printPair (x, y) = print("("^Int.toString(x)^","^Int.toString(y)^") ")

fun printList x = if null x then print("\n")
        else (printPair(hd(x)); printList (tl x));

printList (cross [1, ~1, 2, ~2, 3, ~3] [ 1, 1, 2, 2, 3, 3]);
printList (cross [5, 4, 3, 2, 1] [1, 2, 3, 4, 5]);
printList (cross [~5, ~4, ~3, ~2, ~1] [1, 2, 3, 4, 5]);
printList (cross [5, 5, 4, 4, 3, 3, 2, 2, 1, 1] [1, 1, 1, 1, 1, 1, 1, 1, 1, 1]);

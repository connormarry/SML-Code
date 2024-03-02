(* Part 4. Compute the cross product with a higher-order *)

fun crossHigh a b = let fun extend x l = map (fn y => (x,y)) l
                    in foldr (op @) nil (map (fn x => extend x b) a)
                    end

fun printPair (x, y) = print("("^Int.toString(x)^","^Int.toString(y)^") ")

fun printList x = if null x then print("\n")
        else (printPair(hd(x)); printList (tl x));

printList (crossHigh [1, ~1, 2, ~2, 3, ~3] [ 1, 1, 2, 2, 3, 3]);
printList (crossHigh [5, 4, 3, 2, 1] [1, 2, 3, 4, 5]);
printList (crossHigh [~5, ~4, ~3, ~2, ~1] [1, 2, 3, 4, 5]);
printList (crossHigh [5, 5, 4, 4, 3, 3, 2, 2, 1, 1] [1, 1, 1, 1, 1, 1, 1, 1, 1, 1]);

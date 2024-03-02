fun qsHigh nil    = nil
  | qsHigh (a::b) = 
  		let val (l,r) = 
			(foldr (fn (e,(l,r)) => 
				if (e < a) then (e::l,r) 
				else (l,e::r))
			(nil,nil) b)
	in 
		(qsHigh l)@(a::(qsHigh r))
	end

fun printList x = if null x then print("\n")
        else (print(Int.toString((hd x))^" "); printList (tl x));

printList (qsHigh [1, ~1, 2, ~2, 3, ~3]);
printList (qsHigh [5, 4, 3, 2, 1]);
printList (qsHigh [~5, ~4, ~3, ~2, ~1]);
printList (qsHigh [5, 5, 4, 4, 3, 3, 2, 2, 1, 1]);

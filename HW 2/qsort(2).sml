fun qSort nil    = nil
  | qSort (a::b) = 
  	let fun part e nil    = (nil,nil)
		| part e (h::t) = 
			let val (nh,nt) = part e t
				in if (h<e)
					then (h::nh,nt)
					else (nh,h::nt)
				end
		    	val (l,r) = part a b
	in 
		(qSort l)@(a::(qSort r))
	end

fun printList x = if null x then print("\n")
        else (print(Int.toString((hd x))^" "); printList (tl x));

printList (qSort [1, ~1, 2, ~2, 3, ~3]);
printList (qSort [5, 4, 3, 2, 1]);
printList (qSort [~5, ~4, ~3, ~2, ~1]);
printList (qSort [5, 5, 4, 4, 3, 3, 2, 2, 1, 1]);

local
	fun fibFast1 1 = (1, 0)
		| fibFast1 n = 
		let val (a, b) = fibFast1(n-1)
		in
			(a + b, a)
		end
in
	fun fibFast n =
		let val (a, _) = fibFast1(n)
		in 
			a
		end
end;

print(Int.toString(fibFast 1)^"\n");
print(Int.toString(fibFast 10)^"\n");
print(Int.toString(fibFast 20)^"\n");
print(Int.toString(fibFast 30)^"\n");

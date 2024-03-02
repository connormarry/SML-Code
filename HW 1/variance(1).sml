local 
	fun len nil = 0
		| len (a :: s) = 1 + len(s)

	fun sum_x nil = 0.0
		| sum_x (a :: s) = a +  sum_x s

	fun sum_x2 nil = 0.0
		| sum_x2 (a :: s) = a*a + sum_x2 s
in 
	fun variance nil = 0.0
		| variance x = 
		let val u = sum_x x / real(len(x))
		in 
			sum_x2 x /real(len x) - u*u
		end
end;

print(Real.toString(variance [1.0, 2.0, 3.0, 4.0, 5.0])^"\n");
print(Real.toString(variance [~1.0, ~2.0, ~3.0, ~4.0, ~5.0])^"\n");
print(Real.toString(variance [~1.0, 1.0, ~1.0, 1.0, ~1.0, 1.0])^"\n");
print(Real.toString(variance [~2.0, 2.0, ~2.0, 2.0, ~2.0, 2.0])^"\n");

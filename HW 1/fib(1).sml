fun fib 0 = 0
	| fib 1 = 1
	| fib n = fib (n-1) + fib (n-2);

print(Int.toString(fib 1)^"\n");
print(Int.toString(fib 10)^"\n");


fun sumsq 0 = 0
	| sumsq n = n*n + sumsq (n-1);

print(Int.toString(sumsq 1)^"\n");
print(Int.toString(sumsq 10)^"\n");
print(Int.toString(sumsq 100)^"\n");
print(Int.toString(sumsq 1000)^"\n");

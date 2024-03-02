fun sumOdd 0 = 0
	| sumOdd n = 2*n - 1 + sumOdd (n-1);

print(Int.toString(sumOdd 1)^"\n");
print(Int.toString(sumOdd 10)^"\n");
print(Int.toString(sumOdd 100)^"\n");
print(Int.toString(sumOdd 1000)^"\n");
fun sum 0 = 0
	| sum n = n + sum (n-1);

print(Int.toString(sum 1)^"\n");
print(Int.toString(sum 10)^"\n");
print(Int.toString(sum 100)^"\n");
print(Int.toString(sum 1000)^"\n");






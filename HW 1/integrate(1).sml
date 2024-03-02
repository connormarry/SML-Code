fun integrate f a b n = if n = 0 then 0.0 
	else 
		let val delta = (b - a)/real(n)
		in
			(f (b - delta) + f b)*delta/2.0 + integrate f a (b-delta) (n-1)
		end;

print(Real.toString(integrate Math.sin (~ Math.pi/2.0) 0.0 10000)^"\n");
print(Real.toString(integrate Math.sin (~ Math.pi) 0.0 10000)^"\n");
print(Real.toString(integrate Math.cos (~ Math.pi/2.0) 0.0 10000)^"\n");


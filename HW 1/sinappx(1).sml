local 
	fun sinappx2 n x = if n = 0 then (x, x)
		else 
			let val (a, b) = sinappx2 (n-1) x
				val c = 2.0*real(n)
				val d = b*x*x/c/(c+1.0)
			in
				(a - d,  ~ d)
			end
in
	fun sinappx n x = 
		let val (a, _) = sinappx2 n x
		in
			a
		end
end;

print(Real.toString(sinappx 1000 0.0)^"\n");
print(Real.toString(sinappx 1000 (Math.pi/2.0))^"\n");
print(Real.toString(sinappx 1000 (~ Math.pi/2.0))^"\n");


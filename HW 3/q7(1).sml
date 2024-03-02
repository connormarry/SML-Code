datatype 'a Stream = Nil 
              | Cons of 'a * (unit -> 'a Stream);
exception Bad of string;
fun from seed next = Cons(seed, fn () => from (next seed) next);   

fun head (Nil)       = raise Bad("got nil in head")
   |head (Cons(a,b)) = a;

fun tail (Nil)       = raise Bad("got nil in tail")
   |tail (Cons(a,b)) = b();

fun take 0 stream = nil
	| take n Nil = raise Bad("got nil in take")
	| take n (Cons(h,t)) = h::(take (n-1) (t()));

val nat = from 1.0 (fn x => x + 1.0);

val one = from 1.0 (fn x => 1.0);

val zeros = from 0.0 (fn x=> 0.0);

val alt = from 1.0 (fn x=> ~1.0*x);


fun mul Nil b = Nil
	| mul a Nil = Nil
	| mul (Cons(a:real, fa)) (Cons(b:real, fb)) = Cons(a*b, fn()=> mul (fa()) (fb())) 

fun fac v Nil = Nil
	| fac (v:real) (Cons(b:real, fb)) = Cons(v, fn() => fac (v*b) (fb()))


fun px x = from 1.0 (fn z => z*x);

fun frac Nil = Nil
	| frac (Cons(a:real, fa)) = Cons(1.0/a, fn() => frac (fa()))

(* Q7 Talyor expansion for exponential *)
fun foldl f e nil = e
	| foldl f e (a::b) = foldl f (f a e) b

fun curry f = fn x => fn y=> f(x, y)

fun eval Nil x order = 0.0
	| eval s x order = let val ppx = px x;
							val l = take order (mul s ppx);
						in foldl (curry op +) 0.0 l
						end; 

val coefs = frac (fac 1.0 nat);

val v = eval coefs 1.0 10;

fun printRealList x = if null x then print("\n")
        else (print(Real.toString((hd x))^" "); printRealList (tl x));

printRealList (take 10 coefs);
print(Real.toString(v)^"\n");


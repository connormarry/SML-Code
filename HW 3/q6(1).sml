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

fun frac Nil = Nil
	| frac (Cons(a:real, fa)) = Cons(1.0/a, fn() => frac (fa()))

fun printRealList x = if null x then print("\n")
        else (print(Real.toString((hd x))^" "); printRealList (tl x));

val odd = from 1.0 (fn x => x + 2.0);

printRealList (take 10 (frac odd));

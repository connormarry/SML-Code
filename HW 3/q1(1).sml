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

fun printRealList x = if null x then print("\n")
        else (print(Real.toString((hd x))^" "); printRealList (tl x));

printRealList (take 10 nat);
printRealList (take 10 one);
printRealList (take 10 alt);


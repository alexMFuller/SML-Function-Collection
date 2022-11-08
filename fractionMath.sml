
fun gcd (a :int , 0 :int) : int = a
  | gcd (a:int,b:int) : int = gcd(b, a mod b);

val base = (7,21);
val test = gcd(base);

fun sim(a: int, b :int): (int * int) = ( a div gcd(a,b), b div gcd(a,b));

val base2 = sim(base);


fun add((a: int, b :int) , (c: int, d :int)): (int * int) = sim( a*d + c*b , b*d);

val base3 = add(base,base2);


fun times((a: int, b :int) , (c: int, d :int)): (int * int) = sim( a*c , b*d);

val base4 = times(base2,base3);


fun addAll [] = (0,1)
  | addAll [x] = sim(x)
  | addAll ((a, b)::(c, d)::xs) = addAll (( a*d + c*b , b*d) :: xs);

val base5 = addAll[base,base2,base3,base4];



fun timesAll [] = (1,1)
  | timesAll [x] = sim(x)
  | timesAll ((a, b)::(c, d)::xs) = timesAll (( a*c , b*d) :: xs);

val base6 = timesAll[base,base2,base3,base4];




fun lessThan((a: int, b :int), (c: int, d :int)) =
  let 
    val x = (a*d) 
    val y = (c*b)
  in
    if x < y then true
             else false
  end;

val test2 = lessThan(base6,base5);
val test3 = lessThan(base5,base6);
val testx = lessThan((5,7),(1,2));


fun insert (n,nil) = [n]
  | insert (n, L as head::tail) =
    if(lessThan(n,head)) then n::L
                      else head::(insert(n,tail));

val test4 = insert((5,7), [(1,2),(2,3),(3,4),(4,5)])


fun sort [] = []
  | sort [x] = [x]
  | sort (L as head::tail) = 
    insert(head, sort tail);

val test5 = sort([(2,3),(5,6),(7,8),(5,8),(3,4)]);






  

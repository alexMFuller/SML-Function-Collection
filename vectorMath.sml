fun zip (nil,nil) = nil
  | zip (nil, L) = nil
  | zip (L, nil) = nil
  | zip (L as h1::t1,L2 as h2::t2) = 
    (h1,h2)::zip(t1,t2);

fun zipWithIndex nil = nil
  | zipWithIndex (L as h::t) = 
  let
    val x = List.tabulate (List.length L, fn i => i);
  in 
    zip(x,L)
  end;

fun reduce f (a::b) = foldl f a b;

fun vectorAdd x y = 
  let
    val L = zip(x,y)
    fun add x = map (fn (a,b) => a+b) x
  in
    add L
  end;

val test = vectorAdd [4,5,6] [4,5,6];

fun svProduct x y =  map (fn c => c*x) y;

val test2 = svProduct 2 [1,2,3];

fun mapZip x y = map(fn z => zip(x,z)) y;

val test = mapZip [1,2,3] [[1,1], [2,1], [3,1]];

fun doubleMapProduct x y = map(fn one: int => map( fn two : int list =>  svProduct one two )x )y;

val test = doubleMapProduct  [[1,1], [2,1], [3,1]] [1,2,3];

val test3 = zipWithIndex test;

fun simplify L = map( fn (z,L2) =>  List.drop( List.take(L2 , z+1),z) ) L;

val test4 = simplify test3;

fun a L = map(fn hh::tt => vectorAdd [0,0,0] hh ) L;

fun x L = reduce (fn (z,y) => vectorAdd y z) L;

val x = a test4;


fun vmProduct y x = 
  let
    fun mapZip x y = map(fn z => zip(x,z)) y;
    fun doubleMapProduct x y = map(fn one: int => map( fn two : int list =>  svProduct one two )x )y;
    fun simplify L = map( fn (z,L2) =>  List.drop( List.take( L2 , z+1),z) ) L;
    fun a L = map(fn hh::tt => vectorAdd [0,0,0] hh ) L;
    fun add L = reduce (fn (z,y) => vectorAdd y z) L
  in
    add(a(simplify( zipWithIndex( doubleMapProduct x y))))
  end;

val test4 = vmProduct [1,2,3] [[1,1], [2,1], [3,1]] ;


fun listTab (L::t) = List.tabulate (List.length L, fn i => i);

val x = [[1, 1], [2, 1], [3, 1] ];
val y = [ [1, 2, 3], [1, 1, 1] ];


fun matrixProduct L L2  = map (fn c => vmProduct c L2) L;

val test5 = matrixProduct y x;




    
  






  

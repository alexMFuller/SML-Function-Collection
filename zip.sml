fun zip (nil,nil) = nil
  | zip (nil, L) = nil
  | zip (L, nil) = nil
  | zip (L as h1::t1,L2 as h2::t2) = 
    (h1,h2)::zip(t1,t2);

val base = zip([1, 2, 3], [4, 5]);

fun fst nil = nil
      | fst ((a,_) :: b) = a :: (fst b);


fun unzip nil = ([],[])
  | unzip (L) =
  let
    fun fst nil = nil
      | fst ((a,_) :: b) = a :: (fst b)
    fun lst nil = nil
      | lst ((_,a) :: b) = a::(lst b)
  in
    (fst(L),lst(L))
  end;

val base2 = unzip [(1,2), (3,4), (5,6)];

fun zip3 (nil,nil,nil) = nil
  | zip3 (nil, L, nil) = nil
  | zip3 (L, nil, nil) = nil
  | zip3 (nil, nil, L) = nil
  | zip3 (L, nil, L3) = nil
  | zip3 (nil, L2, L3) = nil
  | zip3 (L, L2, nil) = nil
  | zip3 (L as h1::t1,L2 as h2::t2, L3 as h3::t3) = 
    (h1,h2,h3)::zip3(t1,t2,t3);

val base3 = zip3 ([1, 2, 3], [4, 5], [6,7,8]);

fun unzip3 nil = ([],[],[])
  | unzip3 (L) =
  let
    fun fst nil = nil
      | fst ((a,_,_) :: b) = a :: (fst b)
    fun mid nil = nil
      | mid ((_,a,_) :: b) = a :: (mid b)
    fun lst nil = nil
      | lst ((_,_,a) :: b) = a::(lst b)
  in
    (fst(L),mid(L),lst(L))
  end;

val base4 = unzip3 [(1,2,3), (4,5,6), (7,8,9)];

fun zipWithIndex nil = nil
  | zipWithIndex (L as h::t) = 
  let
    val x = List.tabulate (List.length L, fn i => i);
  in 
    zip(x,L)
  end;

val base5 = zipWithIndex["a", "b", "c"];

fun flatten nil = nil
  | flatten (L as h::t) = 
    h @ flatten(t);

val base6 = flatten [[1,2], [3], [4,5,6]];


fun flatten2 nil = nil
  | flatten2 (L as (a: int,b: int)::t) = 
    a :: b :: flatten2(t);

val base7 = flatten2[(1,2), (3,4), (5,6)];
    
    





    
    

  
    
    
  






  

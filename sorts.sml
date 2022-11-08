fun is_sorted f =
  fn nil => true
    | [x] => true
    | L as h::t::rest => f(h,t) andalso is_sorted f (t::rest);

val test = is_sorted (op >) [4,3,2,1];
val test = is_sorted (op >) [4,3,1,2];


fun selection_sort f =
  fn nil => nil
   | [x] => [x]
   | L as h::t =>
    let
      fun select [] = nil
        | select [x] = [x]
        | select (x::xs) =
      let 
        val L as (y::z) = select xs
        
      in
        if f(x,y) then x :: y :: z  else y :: x :: z 
      end
      val k as (hh::tt) = select L
    in
      [hh] @ selection_sort f tt
    end;

val test2 = selection_sort (op >) [4,3,56,2,58];
val test2 = selection_sort (op <) [4,3,56,2,58];

fun insertion_sort f =
  fn nil => nil
   | L as h::t =>
    let   
      fun insert (n,nil) = [n]
        | insert (n, L as h::t) =
          if(f(n,h)) then n::L
                        else h::(insert(n,t))
    in
      insert(h, insertion_sort f t)
    end;

val test3 = insertion_sort (op >) [0, 5, 1, ~4, 9, 11];
val test3 = insertion_sort (op <) [0, 5, 1, ~4, 9, 11];

      







  

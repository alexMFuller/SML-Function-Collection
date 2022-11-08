
datatype grade = A | B | C | D | F;

datatype 'a tree = 
   Leaf |
   Node of 'a tree * 'a * 'a tree;

fun percent2grade x = 
  if x>= 90.0 then A:grade
  else if (x>= 80.0 andalso x<90.0) then B
  else if (x>= 70.0 andalso x<80.0) then C
  else if (x>= 60.0 andalso x<70.0) then D
  else F;

val test = percent2grade 85.0;

fun grade2point x =
  if x = A then 4.0
  else if x = B then 3.0
  else if x = C  then 2.0
  else if x = D then 1.0
  else 0.0;

val test2 = grade2point A;

fun gpa x = (foldr (op+) 0.0 (map(fn z => grade2point z) x) ) / Real.fromInt(length x);

val test3 = gpa [A, B, C, D, A, B, A];

fun gpaFromPercent x = gpa (map(fn z => percent2grade z) x);

val test4 = gpaFromPercent [67.0, 77.0, 84.0, 99.0];

fun max (x:int) y = if x > y then x else y;


fun height Leaf = 0
  |  height(Node(l,_,r)) = 1 + max (height l) (height r);


val int_tree= Node (Node (Leaf, 1, Leaf), 2, Node (Leaf, 3, Leaf));


val test5 = height int_tree;

fun isBalanced Leaf = true
  | isBalanced(Node(l,_,r)) = if isBalanced(l) andalso isBalanced(r) 
  then true
  else false;


val test6 = isBalanced int_tree;

fun size Leaf = 1
  | size(Node(l,_,r)) = 1 + size(l) + size(r);

val test7 = size int_tree;

fun makeBST [] f = Leaf
  | makeBST (x::xs) f = 
    let
      val tree = Leaf
      fun insert Leaf x = Node (Leaf, x, Leaf)
        | insert (Node(l, root, r)) x = 
          if f(x, root) then Node(insert l x, root, r)
            else Node(l, root, insert r x)
    in
      insert (makeBST xs f) x
    end;


(* 
* Test code for binary search tree 
*
* 'printBST' prints the digraph of a tree.
* It depends on a working 'size' function 
* that returns the size of a tree.
*)

fun printBST tree = 
let
  fun f Leaf n = Int.toString n ^ " [label=\"L\" width=0.5 shape=box style=filled]\n"
    | f (Node(l, x, r)) n = 
    let
      val label = Int.toString n ^ " [label=\""^Int.toString x^"\"]\n"
      val n1 = n + 1
      val n2 = n + 1 + size l
      val edge1 = Int.toString n ^ " -> " ^ Int.toString n1 ^ "\n"
      val edge2 = Int.toString n ^ " -> " ^ Int.toString n2 ^ "\n"
    in
      label ^ f l n1 ^ f r n2 ^ edge1 ^ edge2
    end
in
  print("digraph{\n" ^ f tree 0 ^"}\n\n")
end;


(* 
* This code prints 3 trees as 3 separate digraphs, 
* which can be visualized by copy/pasting each digraph to
*    https://dreampuf.github.io/GraphvizOnline
*)

  val t5 = [35,221,372,223,69];
  val t7 = [48,309,521,312,96,398,466];
  val t9 = [62,397,670,402,123,512,600,173,687];


  val _ = printBST (makeBST t5 (op <=));
  val _ = printBST (makeBST t7 (op <=));
  val _ = printBST (makeBST t9 (op <=));

 




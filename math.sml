
datatype exp = 
    Int of int
  | Plus of exp * exp
  | Minus of exp * exp
  | Times of exp * exp
  | Div of  exp * exp
  | Var of string
  | Let of string * exp * exp;

datatype value = 
    CVal of int
  | Error of string;

(* 
 * 1. implement 
 *      toString: exp -> string 
 *)

fun toString (Int x) = Int.toString x
 | toString (Plus(x, y)) = "(" ^toString(x)^ " + " ^toString(y)^ ")"
 | toString (Minus(x,y)) = "(" ^toString(x)^ " - " ^toString(y)^ ")"
 | toString (Times(x,y)) = "(" ^toString(x)^ " * " ^toString(y)^ ")"
 | toString (Div(x,y)) = "(" ^ toString(x)^ " / " ^toString(y)^ ")"
 | toString (Var x) = x
 | toString (Let(x,e1,e2)) = "let val " ^ x ^ "=" ^ toString(e1)
 ^ " in " ^ toString(e2) ^ " end";


(* 2. implement 
 *       toStringValue: value -> string 
 *)

fun toStringValue (CVal x) = Int.toString x
  | toStringValue (Error x) = x;

(* 
 * you may want to implement the helper function 
 *       lookup: (string * value) list -> string -> value 
 *)

(*fun lookup ((x:string ,CVal v):: ctx) y = if x=y then CVal v else lookup ctx y;*)


fun lookup ((x:string ,CVal v):: ctx) y = if x=y then CVal v else if size x
 = 0 then Error "Could not find a Var" else lookup ctx y;

(*
 * 3. implement the function 
 *       eval: exp -> (string * value) list -> value
 *
 * you may find 'case expression' useful in your implementation.
 * If you use case expressions in pattern matching, remember to wrap the case
 * expressions in paranthesis to avoid parsing errors.
 *)

fun eval (Int x) _ = CVal (x)
  | eval (Var x) ctx = lookup ctx x
  | eval (Let(x,e1,e2)) ctx =eval e2 ((x, eval e1 ctx)::ctx)
  | eval (Times(e1,e2)) ctx =
      let
        val (CVal x) = eval e1 ctx
        val (CVal y) = eval e2 ctx
      in
        CVal (x * y)
      end
  | eval (Plus(e1,e2)) ctx =
      let
        val (CVal x) = eval e1 ctx
        val (CVal y) = eval e2 ctx
      in
        CVal (x + y)
      end
  | eval (Minus(e1,e2)) ctx =
      let
        val (CVal x) = eval e1 ctx
        val (CVal y) = eval e2 ctx
      in
        
        CVal (x - y)
      end
  | eval (Div(e1,e2)) ctx =
      let
        val (CVal x) = eval e1 ctx
        val (CVal y) = eval e2 ctx
      in
        if y = 0 then Error "Div by Zero Error" else CVal (x div y)
      end;


          

(* Test Code *)


val t1 = Let("y", Int 10, 
                Let("y", Int 20,
                    Times(Var "y", Int 5)));


toStringValue(eval t1 []);

val t2 = Let("y", Int 10, 
                  Let("z", Plus(Var "x", Var "y"), 
                             Times(Var "y", Int 5)));
toStringValue(eval t2 []);
                                   

val t3 = Div(Int 10, Int 0);
toStringValue(eval t3 []);

val t4 = Plus(Int 10, Minus (Int 20, Var("x")));
toStringValue(eval t4 []);

val t5 = Let("x", Plus(Int 10, t3), Plus(Int 0, Int 20));
toStringValue(eval t5 []);



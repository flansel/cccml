open List;;
open Printf;;

exception Type_error;;

type typ = 
    | CCCTBool
    | CCCTInt
    | CCCTArrow of typ * typ;;

type type_environment = (string*typ)list;; 

type exp =
    | CCCTrue
    | CCCFalse
    | CCCIf of exp * exp * exp
    | CCCNum of int
    | CCCIsZero of exp
    | CCCPlus of exp * exp
    | CCCMult of exp * exp
    | CCCVar of string
    | CCCLambda of string * typ * exp
    | CCCApply of exp * exp
    | CCCLambdaRec of string * typ * typ * string * exp
    | CCCDiv of exp * exp
    | CCCTry of exp * exp
    | CCCRaiseDivByZero of typ * exp;;

exception Eval_error;;

exception Substitution_error;;


(*
 * My fliter for fvs()
 *)

let rec my_filter n lst = match lst with
    | hd :: tl -> if hd = n then (my_filter n tl) else hd :: (my_filter n tl)
    | [] -> [];;

(*
 * returns the free variables in expression e
 *)

let rec fvs (e : exp) = match e with
    | CCCTrue -> []
    | CCCFalse -> []
    | CCCNum n -> []
    | CCCVar n -> [n]
    | CCCIsZero e -> fvs e
    | CCCPlus(e1,e2) -> (fvs e1)@(fvs e2) 
    | CCCMult(e1,e2) -> (fvs e1)@(fvs e2)
    | CCCIf(e1,e2,e3) -> (fvs e1)@(fvs e2)@(fvs e3)
    | CCCLambda(n,t,e1) -> my_filter n (fvs e1)
    | CCCApply(e1, e2) -> (fvs e1)@(fvs e2)
    | CCCLambdaRec(f,t1,t2,x,e1) -> my_filter f (my_filter x (fvs e1))
    | CCCDiv(e1,e2) -> (fvs e1)@(fvs e2)
    | CCCTry(e1,e2) -> (fvs e1)@(fvs e2)
    | CCCRaiseDivByZero(t1,e1) -> fvs e1;;
(*
 * Substitute e2 into e1 for x
 *)

let rec substitution (e1 : exp) (x : string) (e2 : exp) = match e1 with
    | CCCTrue -> CCCTrue
    | CCCFalse -> CCCFalse
    | CCCNum n -> CCCNum n
    | CCCVar name -> if name = x then e2 else CCCVar name
    | CCCIsZero ex1 -> CCCIsZero (substitution ex1 x e2)
    | CCCPlus(ex1,ex2) -> CCCPlus(substitution ex1 x e2, substitution ex2 x e2)
    | CCCMult(ex1,ex2) -> CCCMult(substitution ex1 x e2, substitution ex2 x e2)
    | CCCIf(ex1,ex2,ex3) -> CCCIf(substitution ex1 x e2, substitution ex2 x e2, substitution ex3 x e2)
    | CCCLambda(n,t,ex) -> if x = n then e1 else if List.mem n (fvs e2) then raise Substitution_error else CCCLambda(n,t,substitution ex x e2)
    | CCCApply(ex1,ex2) -> CCCApply(substitution ex1 x e2, substitution ex2 x e2)
    | CCCLambdaRec(f,t1,t2,v,ex) -> if v = x || f = x then ex else if List.mem v (fvs e2) || List.mem f (fvs e2) then raise Substitution_error else (substitution ex x e2)
    | CCCDiv(ex1,ex2) -> CCCDiv(substitution ex1 x e2, substitution ex2 x e2)
    | CCCTry(ex1,ex2) -> CCCTry(substitution ex1 x e2, substitution ex2 x e2)
    | CCCRaiseDivByZero(t1,ex) -> CCCRaiseDivByZero(t1, substitution ex x e2);; 

(*
 * Print out expressions as strings for debugging
 *)
let rec string_of_exp (e:exp) = match e with
    | CCCVar x -> "CCCVar" ^ x
    | CCCNum n -> string_of_int n
    | CCCTrue -> "true"
    | CCCFalse -> "false"
    | CCCPlus(e1,e2) -> "(" ^ string_of_exp e1 ^ " + " ^ string_of_exp e2 ^ ")"
    | CCCIf(e1, e2, e3) -> "if " ^ string_of_exp e1 ^ " then " ^ string_of_exp e2 ^ " else " ^ string_of_exp e3
    | CCCMult(e1, e2) ->  "(" ^ string_of_exp e1 ^ " * " ^ string_of_exp e2 ^ ")"
    | CCCIsZero e1 -> "(isZero " ^ string_of_exp e1 ^ ")"
    | CCCLambda(n,t,ex) -> "CCCLambda("^n^","^string_of_exp ex^")"
    | CCCApply(e1,e2) -> "CCCApply("^string_of_exp e1^","^string_of_exp e2^")"
    | CCCLambdaRec(f,t1,t2,x,ex) -> "CCCLambdaRec()"
    | CCCDiv(e1,e2) -> "CCCDiv("^string_of_exp e1^","^string_of_exp e2^")"
    | CCCTry(e1,e2) -> "CCCTry("^string_of_exp e1^","^string_of_exp e2^")"
    | CCCRaiseDivByZero(t1,e1) -> "CCCRaiseDivByZero(CCCTInt,"^string_of_exp e1^")";;

(*
 * Small step reduction for our grammar
 *)
let rec step (e : exp) = (*print_endline ("step: "^(string_of_exp e));*) match e with
    | CCCTrue -> raise Eval_error
    | CCCFalse -> raise Eval_error
    | CCCNum n -> raise Eval_error
    | CCCVar x -> raise Eval_error
    | CCCIsZero CCCTrue -> raise Eval_error
    | CCCIsZero CCCFalse -> raise Eval_error
    | CCCIsZero (CCCNum n) -> if n = 0 then CCCTrue else CCCFalse
    | CCCIsZero (CCCRaiseDivByZero(t1,e1)) -> CCCRaiseDivByZero(t1,e1)
    | CCCIsZero e1 -> CCCIsZero (step e1)
    | CCCPlus(CCCNum n1, CCCNum n2) -> CCCNum ((n1+n2))
    | CCCPlus(CCCRaiseDivByZero(t1,e1), e2) -> CCCRaiseDivByZero(t1,e1)
    | CCCPlus(CCCNum n1, CCCRaiseDivByZero(t1,e1)) -> CCCRaiseDivByZero(t1,e1)
    | CCCPlus(CCCNum n1, e1) -> CCCPlus(CCCNum n1, step e1) 
    | CCCPlus(e1, e2) -> CCCPlus(step e1, e2)
    | CCCMult(CCCNum n1, CCCNum n2) -> CCCNum ((n1*n2))
    | CCCMult(CCCRaiseDivByZero(t1,e1), e2) -> CCCRaiseDivByZero(t1,e1)
    | CCCMult(CCCNum n1, CCCRaiseDivByZero(t1,e1)) -> CCCRaiseDivByZero(t1,e1)
    | CCCMult(CCCNum n1, e1) -> CCCMult(CCCNum n1, step e1)
    | CCCMult(e1, e2) -> CCCMult(step e1, e2)
    | CCCIf(CCCTrue,e1,e2) -> e1
    | CCCIf(CCCFalse,e1,e2) -> e2
    | CCCIf(CCCRaiseDivByZero(t1,e1),e2,e3) -> CCCRaiseDivByZero(t1,e1)
    | CCCIf(e1,e2,e3) -> CCCIf(step e1, e2, e3)
    | CCCApply(CCCRaiseDivByZero(t1,e1),e2) -> CCCRaiseDivByZero(t1,e1)
    | CCCApply(e1,CCCRaiseDivByZero(t1,e2)) -> CCCRaiseDivByZero(t1,e2)
    | CCCApply(CCCLambda(n,t,ex), e1) -> (substitution ex n e1)
    | CCCApply(CCCLambdaRec(f,t1,t2,x,ex), e1) -> substitution (substitution ex x e1) f (CCCLambdaRec(f,t1,t2,x,ex))
    | CCCApply(e1,e2) -> CCCApply(step e1, e2)
    | CCCDiv(CCCNum n1, CCCNum n2) -> if n2 = 0 then CCCRaiseDivByZero(CCCTInt, CCCNum n1) else CCCNum ((n1/n2))
    | CCCDiv(CCCRaiseDivByZero(t1,e1), e2) -> CCCRaiseDivByZero(t1,e1)
    | CCCDiv(CCCNum n1, CCCRaiseDivByZero(t1,e1)) -> CCCRaiseDivByZero(t1,e1)
    | CCCDiv(CCCNum n1, e1) -> CCCDiv(CCCNum n1, step e1)
    | CCCDiv(e1, e2) -> CCCDiv(step e1, e2)
    | CCCTry(CCCRaiseDivByZero(t1,e1),e2) -> CCCApply(e2,e1)
    | CCCTry(CCCNum n, e1) -> CCCNum n
    | CCCTry(e1,e2) -> CCCTry(step e1,e2)
    | CCCRaiseDivByZero(t1, CCCRaiseDivByZero(t2,e1)) -> CCCRaiseDivByZero(t2,e1)
    | CCCRaiseDivByZero(t1,e1) -> CCCRaiseDivByZero(t1, step e1)
    | _ -> failwith "wtf";;

(*
 * Driver for the small step evaluation
 *)

let rec multi_step (e : exp) = (*print_endline ("multi "^(string_of_exp e));*) match e with
    | CCCTrue -> CCCTrue
    | CCCFalse -> CCCFalse
    | CCCNum n -> CCCNum n
    | CCCLambda(n,t,ex) -> CCCLambda(n,t,ex)
    | CCCLambdaRec(f,t1,t2,x,ex) -> CCCLambdaRec(f,t1,t2,x,ex)
    | CCCRaiseDivByZero(t1,CCCNum n) -> CCCRaiseDivByZero(t1,CCCNum n)
    | e1 -> multi_step (step e1);;

(*
 * Validate the type of expressions is correct
 *)

let rec type_check (te : type_environment)(e : exp) = match e with
    | CCCTrue -> CCCTBool
    | CCCFalse -> CCCTBool
    | CCCNum n -> CCCTInt
    | CCCVar n -> let z = try List.assoc n te with Not_found -> raise Type_error in z
    | CCCIsZero e1 -> if type_check te e1 = CCCTInt then CCCTBool else raise Type_error
    | CCCPlus(e1, e2) -> if (type_check te e1 = CCCTInt && type_check te e2 = CCCTInt) then CCCTInt else raise Type_error
    | CCCMult(e1, e2) -> if (type_check te e1 = CCCTInt && type_check te e2 = CCCTInt) then CCCTInt else raise Type_error
    | CCCIf(e1,e2,e3) -> if (type_check te e1 = CCCTBool && (type_check te e2 = type_check te e3)) then type_check te e2 else raise Type_error
    | CCCLambda(n,t,e1) -> CCCTArrow(t, type_check ([(n,t)]@te) e1)
    | CCCApply(e1,e2) -> let z = type_check te e2 in (match type_check te e1 with
                                                  | CCCTArrow(t1, t2) -> if t1 = z then t2 else raise Type_error
                                                  | _ -> raise Type_error)
    | CCCLambdaRec(f,t1,t2,n,ex) -> if (type_check ([(f,CCCTArrow(t1,t2))]@[(n,t1)]@te) ex) = t2 then CCCTArrow(t1,t2) else raise Type_error
    | CCCDiv(e1,e2) -> if (type_check te e1 = CCCTInt && type_check te e2 = CCCTInt) then CCCTInt else raise Type_error
    | CCCTry(e1,e2) -> let z = type_check te e1 in if type_check te e2 = CCCTArrow(CCCTInt, z) then z else raise Type_error
    | CCCRaiseDivByZero(t1,e1) -> t1;; 
(*
let lst =  fvs ((CCCLambda ("x", CCCTInt, CCCPlus (CCCVar "x", CCCVar "y"))));;
let () = List.iter (printf "%s ") lst; print_endline ": free vars";;
        (*print_endline (string_of_exp ());;*)
*)

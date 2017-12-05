(*  Starting point for CS 442/642 W16 Assignment 3

   These datatype declarations form an abstract syntax for Milner
   expressions.  All inputs to the type-inferencer are assumed to be
   syntactically valid Milner programs.  *)

datatype prim = Add | Neg | Mult | Div | And | Or | Not | Eq | Lt | Gt 

datatype milner = Var of string
                | Abs of string * milner
                | App of milner * milner
                | If of milner * milner * milner
                | Let of string * milner * milner
                | Fix of string * milner
                | Int of int
                | Bool of bool
                | Prim of prim
                | Letex of string * milner
                | Raise of milner
                | Handle of milner * milner * milner



datatype mtype = TInt | TBool | TVar of string 
               | TException
               | Arrow of mtype * mtype
               | Quantified of string * mtype


(* Generating Type Variables:
   We reserve varables of the form Zn, n an integer, n>=0. *)
val counter = ref 0

fun newtype () = 
   "Z" ^ Int.toString(!counter before counter := !counter + 1)


(* Environment: mapping from names to types *)

(* type env =  ... *)


(* W:  Accepts the arguments E (expression) and A (environment).
    Returns the type of E in A.  *)
(* fun W E A = ... *)

exception illegalArgumentException

(* Part B *)

fun pptype(TInt) = "int"
|	pptype(TBool) = "bool"
|   pptype(TVar(x)) = "'" ^ x
|	pptype(Arrow(a,b)) = "(" ^ pptype(a) ^  " -> " ^ pptype(b) ^ ")"
|	pptype(TException) = "exception"
|	pptype(_) =  raise illegalArgumentException


(* Part C *)
 
fun primtype(Neg) =  Arrow(TInt, TInt)
|	primtype(And) = Arrow(TBool, Arrow(TBool, TBool))	
|	primtype(Or) = Arrow(TBool, Arrow(TBool, TBool))
|	primtype(Not) =  Arrow(TBool, TBool)
|	primtype(Eq) = Arrow(TInt, Arrow(TInt, TBool))
| 	primtype(Lt) = Arrow(TInt, Arrow(TInt, TBool))
| 	primtype(Gt) = Arrow(TInt, Arrow(TInt, TBool))
|	primtype(_) =  Arrow(TInt, Arrow(TInt, TInt))



fun	substvar(TVar(a),b,TVar(c)) = if a = c then b else TVar(a)
|	substvar(Arrow(x,y),b,TVar(c)) = Arrow(substvar(x,b,TVar(c)), substvar(y,b,TVar(c)))
|	substvar(Quantified(x,y),b, TVar(c)) = if x = c 
										   then substvar(y,b, TVar(c)) 
										    else Quantified(x,substvar(y,b,TVar(c)))
|	substvar(a,_,_) = a

fun subst(a, nil)  = a
|	subst(a, (x,TVar(y))::t) = subst(substvar(a, x, TVar(y)), t)
|	subst(a, _) = a

fun contains(TVar(a), TVar(b)) = a = b
|	contains(Arrow(a,b),c) = contains(a,c) orelse contains(b,c)
|	contains(_,_) = false


exception NoUnifierException
exception OccursCheckException

fun U(Arrow(a,b), Arrow(c,d)) = 
		let
			val s1 = U(a,c)
			val s2 = U(subst(b,s1), subst(d,s1))
		in
			s1 @ s2
		end

|	U(TVar(a), TVar(b)) = if (a = b) then [] else [(TVar(a), TVar(b))]
|	U(Arrow(a,b), TVar(c)) = if contains(Arrow(a,b), TVar(c)) then raise OccursCheckException else [(Arrow(a,b), TVar(c))]
|	U(TVar(c), Arrow(a,b)) = U(Arrow(a,b), TVar(c))
|	U(t, TVar(a)) = [(t, TVar(a))]
|	U(TVar(a), t) = U(t, TVar(a))
|	U(Arrow(a,b), t) = raise NoUnifierException
|	U(TException, t) = []
|	U(t, TException) = U(TException, t)
|	U(t, Arrow(a,b)) = U(Arrow(a,b), t)
|   U(a,b) = if a = b then [] else raise NoUnifierException


fun replace(TVar(a)) = TVar(newtype())
|	replace(Arrow(a,b)) = Arrow(replace(a),replace(b))
|	replace(_) = raise illegalArgumentException

fun replaceQuantified(Quantified(x,y)) = substvar(replaceQuantified(y), TVar(newtype()), TVar(x))
|	replaceQuantified(Arrow(a,b)) = Arrow(replaceQuantified(a), replaceQuantified(b))
|	replaceQuantified(x) = x



exception illTypedException
exception IllegalRaiseException

fun lookup(_, nil) = raise illTypedException
|	lookup(Raise(Var(x)), (Var(y), TException)::t) = if x = y then TException else lookup(Raise(Var(x)), t)
|	lookup(Var(x), (Var(y), h)::t) = if x = y then 
										(if h = TException then 
											raise IllegalRaiseException 
											else replaceQuantified(h))
									 	else lookup(Var(x), t)
|	lookup(_,_) = raise illegalArgumentException

fun updateEnvironment([], _) = []
|	updateEnvironment((x,y)::t1, S) = (x, subst(y, S))::updateEnvironment(t1,S)



fun Quantify(A,a) = 
	let
		fun findUnavailableVariables([], TVar(x)) = [x]
		|	findUnavailableVariables((x,y)::t, TVar(z)) = if contains(y,TVar(z)) then [] else findUnavailableVariables(t, TVar(z))
		|	findUnavailableVariables(B, Arrow(x,y)) = findUnavailableVariables(B,x) @ findUnavailableVariables(B,y)
		|	findUnavailableVariables(_,_) = []
		fun uniqueList([]) = []
  		| uniqueList(h::t) = TVar(h)::uniqueList(List.filter (fn y => y <> h) t)
		val toBeQuantified = uniqueList(findUnavailableVariables(A,a))
		fun quantify([], x) = x
		|	quantify(TVar(h)::t, y) = if contains(y,TVar(h)) then Quantified(h,quantify(t,y)) else quantify(t,y)
		|	quantify(_,_) = raise illegalArgumentException
	in
		quantify(toBeQuantified, a)
	end


(* Algorithm W *)

val initenv = []
val emptyenv = []

exception IllegalHandleException
fun Whelper(Var(x), A) = (lookup(Var(x),A), []) 
|	Whelper(Bool(_), A) = (TBool, [])
|	Whelper(Int(_),A) = (TInt, [])
|	Whelper(Prim(a), A) = (primtype(a), [])
|	Whelper(Abs(x, E), A) =
	let
		val newTVar = TVar(newtype())
		val (tau, S) = Whelper(E, (Var(x), newTVar)::A)
	in
		(Arrow(subst(newTVar,S),tau), S)
	end
|	Whelper(App(E1, E2), A) =
	let
		val newTVar = TVar(newtype())
		val (tau1, S1) = Whelper(E1, A)
		val (tau2, S2) = Whelper(E2, updateEnvironment(A,S1))
		val S3 = U(subst(tau1, S2), Arrow(tau2, newTVar))
	in
		(subst(newTVar, S3), S3 @ S2 @ S1)
	end
|	Whelper(If(E1, E2, E3), A) = 
	let
		val (tau1, S1) = Whelper(E1, A)
		val S2 = U(TBool, tau1)
		val (tau3, S3) = Whelper(E2, updateEnvironment(A, S2 @ S1))
		val (tau4, S4) = Whelper(E3, updateEnvironment(A, S3 @ S2 @ S1))
		val S5 = U(subst(tau3, S4), tau4)
	in
		(subst(tau4, S5), S5 @ S4 @ S3 @ S2 @ S1)
	end
|	Whelper(Fix(x, E), A) =
	let
		val newTVar = TVar(newtype())
		val (tau1, S1) = Whelper(E, [(Var(x), newTVar)] @ A)
		val S2 = U(subst(newTVar, S1), tau1)
	in
		(subst(newTVar, S2 @ S1), S2 @ S1)
	end
|	Whelper(Let(x, E1, E2), A) = 
	let
		val (tau1, S1) = Whelper(E1, A)
		val A2 = updateEnvironment(A, S1)
		val (tau2, S2) = Whelper(E2, (Var(x), Quantify(A2, tau1))::A2)
	in
		(tau2, S2 @ S1)
	end
|	Whelper(Letex(x, E), A) = Whelper(E, (Var(x), TException)::A)
|   Whelper(Raise(Var(x)), A) = (lookup(Raise(Var(x)),A), [])
|	Whelper(Raise(_), A) = raise IllegalRaiseException
|   Whelper(Handle(Var(E1), E2, E3), A) =Whelper(If(App (App (Prim (Eq), E2), Var(E1)), E3, E2), A)
|	Whelper(Handle(_,_,_), _) = raise IllegalHandleException

val W = fn x => fn y =>  Whelper(x,y)
;

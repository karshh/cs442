
signature DFASIG = sig
	eqtype Q
	eqtype Sigma
	val init : Q
	val delta : Q * Sigma -> Q
	val accepting : Q list
end

functor DFA (Spec:DFASIG) : sig
	val run : Spec.Sigma list -> bool
end
= struct
	fun run l = 
		let 
			fun run1 s [] = s
			|	run1 s (x::xs) = run1 (Spec.delta (s,x)) xs

			val final = run1 Spec.init l

			fun member _ [] = false
			|	member x (y::ys) = x = y orelse (member x ys)
		in
			member final Spec.accepting
		end
end

signature NFASIG = sig
	eqtype Q
	eqtype Sigma
	val init : Q list
	val delta : Q list * Sigma -> Q list
	val accepting : Q list
end


functor NFA (Spec:NFASIG) : sig
	val run : Spec.Sigma list -> bool
end
= struct
	fun run l = 
		let 
			fun run1 s [] = s
			|	run1 s (x::xs) = run1 (Spec.delta (s,x)) xs

			val final = run1 Spec.init l

			fun memberhelper _ [] = false
			|	memberhelper x (y::ys) = x = y orelse (memberhelper x ys)

			fun	member [] _ = false
			|	member (x::xs) Y = (memberhelper x Y) orelse (member xs Y)
		in
			member final Spec.accepting
		end
end

functor DFAtoNFA (D:DFASIG):NFASIG = struct
	type Q = D.Q
	type Sigma = D.Sigma
	val init = [D.init]
	fun delta (ql, sigm) = map (fn x => D.delta(x, sigm)) ql
	val accepting = D.accepting 
end

structure E0O1n = struct
	exception Bad
	type Sigma = int
	datatype QAux = ODD | EVEN
	type Q = QAux * QAux
	val init = [(EVEN, EVEN)]
	val accepting = [(EVEN, ODD)]

	fun toggle ODD = EVEN
	|	toggle _ = ODD

	fun delta([(x,y)], 0) = [(toggle x, y)]
	|	delta([(x,y)], 1) = [(x, toggle y)]
	|	delta _ = raise Bad
end

structure E0O1d = struct
	exception Bad
	type Sigma = int
	datatype QAux = ODD | EVEN
	type Q = QAux * QAux
	val init = (EVEN, EVEN)
	val accepting = [(EVEN, ODD)]

	fun toggle ODD = EVEN
	|	toggle _ = ODD

	fun delta((x,y), 0) = (toggle x, y)
	|	delta((x,y), 1) = (x, toggle y)
	|	delta _ = raise Bad
end

functor Intersection(structure Spec1:DFASIG and Spec2:DFASIG
                     sharing type Spec1.Sigma = Spec2.Sigma):DFASIG = 
struct
	type Q = Spec1.Q * Spec2.Q
	type Sigma = Spec1.Sigma
    val init = (Spec1.init, Spec2.init)
    fun delta((x,y), s) = (Spec1.delta(x, s), Spec2.delta(y, s))
    val accepting = 
    	let
    		fun acceptingHelper1 (_ ,[]) = []
    		| 	acceptingHelper1 (x ,y::ys) = (x,y)::acceptingHelper1(x,ys)
    		fun acceptingHelper2 ([], _) = []
    		|	acceptingHelper2 (x::xs, A) = acceptingHelper1(x,A) @ acceptingHelper2(xs, A)
    	in
    		acceptingHelper2 (Spec1.accepting, Spec2.accepting)
    	end 
end



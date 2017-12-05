
(* val scfoldr = fn : ('a * 'b * 'b cont -> 'b) * 'b * 'a list -> 'b *) 

type 'a cont = 'a SMLofNJ.Cont.cont
val throw = SMLofNJ.Cont.throw
val callcc = SMLofNJ.Cont.callcc

fun scfoldr (f, i, l) = 
	let
		fun helpr _ i [] _ = i
		|	helpr f i (x::xs) k = f(x,(helpr f i xs k),k)
	in
		callcc (helpr f i l)
	end

fun exists p L = scfoldr(fn(a,b,k) => if p = a then throw k true else false, false, L)

;


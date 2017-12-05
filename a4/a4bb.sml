datatype 'a streaming = Stream of 'a * (unit -> 'a streaming) | Null
fun lazymap f x = 
	let
		fun lazymaphelper (_, Null) = Null
		|	lazymaphelper (a, Stream(b,c)) = Stream(a b, fn () => lazymaphelper(a,(c ())))
	in
		lazymaphelper(f,x)
	end

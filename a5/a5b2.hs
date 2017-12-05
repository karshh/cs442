data State s a = ST(s -> (a, s))

instance Monad (State s) where
   return x = ST(\s -> (x, s))
   (ST x) >>= f = ST (\s0 -> let (a, s1) = x s0
                                 (ST g) = f a
                                 (b, s2) = g s1
                             in
                                 (b, s2)
                     )

type MyState = (Int, Int)

fib :: Int -> [Int]
fib n = ans
	where
	 inc :: State(Int, Int) Int
	 inc = ST(\(cur,next)->(cur, (next,cur+next)))
	 fibhelper :: Int -> State(Int,Int) [Int]
	 fibhelper 0 = return []
	 fibhelper n = do 
			x <- inc 
			y <- fibhelper(n-1) 
			return(x:y) 
	 ST s = fibhelper n 
	 (ans,_) = s (0,1) 

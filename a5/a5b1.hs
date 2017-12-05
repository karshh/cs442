guess :: IO ()
guess = 
	do 
	 putStr "Enter guessing range: "
	 range <- getLine
	 st <- return (read (head (words range)) :: Int)
	 en <- return (read (head (tail (words range))) :: Int)
	 helpguess ((st+en) `div` 2) st en
	where
	 helpguess :: Int -> Int -> Int -> IO ()
	 helpguess a st en = 	
	  do
	   putStr ("Is it "++(show a)++"?: ")
	   getLine >>= (\x -> if x == "higher" then 
				if a >= en  then putStrLn "Cheating!" 
				else helpguess ((a+en) `div` 2) (a+1) en
			     else 
				if x == "lower" then
				  if a <= st then putStrLn "Cheating!" 
				  else helpguess ( (a+st) `div` 2) st (a-1)
			        else 
				  if x == "yes" then putStrLn "Got it!"
			          else 
				   do 
				    putStrLn "Incorrect Command."
				    helpguess a st en)
						 	 	

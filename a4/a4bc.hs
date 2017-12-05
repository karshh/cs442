lazymap :: (a -> b) -> [a] -> [b]
lazymap f [] = []
lazymap f (x:xs) = f x : lazymap f xs

sortBy_stan :: (a -> a -> Ordering) -> [a] -> [a]
sortBy_stan _ [] = []
sortBy_stan ord_fn (a:rest) = 
	(sortBy_stan ord_fn [x | x<-rest , (ord_fn x a) == LT]) ++ 
	[a] ++ 
	(sortBy_stan ord_fn [y | y<-rest , (ord_fn y a) == GT || (ord_fn y a) == EQ])
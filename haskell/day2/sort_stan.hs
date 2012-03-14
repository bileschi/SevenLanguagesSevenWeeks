sort_stan :: Ord a => [a] -> [a]
sort_stan [] = []
sort_stan (a:rest) = (sort_stan [x | x<-rest , x <= a]) ++ [a] ++ (sort_stan [y | y<-rest , a < y])
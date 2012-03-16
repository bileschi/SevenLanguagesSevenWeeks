-- Write a function taht takes an argument x and returns a lazy sequence that has every third number, starting with x.  Then, write a function that includes every fifth number, beginning with y.  Combine these functions through composition to return every 8th number, beginning with x+y

every3 x = [x, x+3 ..]
every5 x = [x, x+5 ..]
every8 x y = zipWith (+) (every3 x) (every5 y) 

-- take 5 (every8 0 0)
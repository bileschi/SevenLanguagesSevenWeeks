-- greatest common denominator
-- the greatest common denominator, d, of inputs a and b, is the largest integer such that
-- mod(a, d) == 0 and mod(b, d) == 0

gcd_euclid :: Int -> Int -> Int
gcd_euclid a b
 | r == 0 = b
 | b >= r = gcd_euclid b r
 | otherwise = gcd_euclid r b
 where r = mod a b

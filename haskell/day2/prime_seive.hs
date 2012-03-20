-- * Create a lazy sequency of prime numbers.
-- from "The Genuine Seive of Eratosthenes"

import Data.Map

sqr :: (Num t ) => t -> t
sqr x = x * x

sieve xs = sieve' xs Map.empty
  where
    sieve' [] table = []
    sieve' (x:xs) table =
      case Map.lookup x table of
        Nothing −> (x:(sieve' xs (Map.insert (sqr x) [x] table)))
        Just facts −> sieve' xs (foldl reinsert (Map.delete x table) facts)
      where
        reinsert table prime = Map.insertWith ( ++ ) (x+prime) [prime] table
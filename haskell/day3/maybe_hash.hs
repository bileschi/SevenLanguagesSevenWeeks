
maybe_hash_1 :: (Eq k) => [(k, v)] -> k -> Maybe v 
maybe_hash_1 [] _ = Nothing
maybe_hash_1 ((keyH, val):rest) keyQ
  | keyH == keyQ = Just val
  | otherwise = maybe_hash_1 rest keyQ

-- Usage at GHCI terminal
-- maybe_hash_1 [] "hi"
-- Nothing
-- *Main> maybe_hash_1 [("hi", 4)] "hi"
-- Just 4
-- *Main> maybe_hash_1 [("hi", 4), ("bye", 3)] "hi"
-- Just 4
-- *Main> maybe_hash_1 [("hi", 4), ("bye", 3)] "bye"
-- Just 3
-- *Main> maybe_hash_1 [("hi", 4), ("bye", 3)] "sup"
-- Nothing


maybe_hash_2 :: (Eq k, Eq k2) => [(k, [(k2, v)])] -> k -> k2 -> Maybe v 
maybe_hash_2 [] _ _ = Nothing
maybe_hash_2 ((key1h, val):rest) key1q key2q 
  | key1h == key1q = maybe_hash_1 val key2q
  | otherwise = maybe_hash_2 rest key1q key2q


-- *Main> maybe_hash_2 [] 1 "hi"
-- Nothing
-- *Main> maybe_hash_2 [(1,[])] 1 "hi"
-- Nothing
-- *Main> maybe_hash_2 [(1,[("hi", "bye")])] 1 "hi"
-- Just "bye"
-- *Main> maybe_hash_2 [(1,[("hi", ())])] 1 "hi"
-- Just ()

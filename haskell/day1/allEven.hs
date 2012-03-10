-- using list comprehension and mod
allEven l = [x | x <- l , mod x 2 == 0]

-- using pattern matching and tail recursion
allEven' [] = []
allEven' (h:t) = if ((mod h 2) == 0) then h:allEven' t else allEven' t

-- using list comprehension and set membership
allEven'' l = [x | x <- l , (elem x [0, 2 .. maximum l])]
-- * Write a Haskell function to convert a string to a anumber.  
-- * The string should be in the form $2,345,678.99 and can possibly have leading zeroes

import Data.Char
import Data.List

string_to_num :: (Fractional b) => String -> b
string_to_num s = realToFrac (string_to_num_clean (just_digits s)) / realToFrac (10 ^ (digits_after_decimal s))

-- only works with strings of digits.  no funny business.
string_to_num_clean :: String -> Int
string_to_num_clean [] = 0
string_to_num_clean (c:[]) = digitToInt c
string_to_num_clean s = 10 * (string_to_num_clean (init s)) + (digitToInt (last s))


digits_after_decimal :: String -> Int
digits_after_decimal s = 
	case (findIndex (== '.') (reverse s)) of
	Just value -> value
	Nothing -> 0

just_digits :: String -> String
just_digits s = [c | c <- s, (elem c "0123456789") ]

-- *Main Data.Char Data.List> string_to_num "$2,345,678.99"
-- 2345678.99
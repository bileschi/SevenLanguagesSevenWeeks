-- * Use a partially applied function to define a function that will return half of a number and another that will append \n to the end of any string.

half :: Fractional a => a -> a
half = (/ 2)

endline :: String -> String
endline = (++ "\n")
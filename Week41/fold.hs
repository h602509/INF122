

sumFold :: (Num b, Enum b) => b -> b
sumFold n = foldr (+) 0 [1..n]

factorialFold :: (Num b, Enum b) => b -> b
factorialFold n = foldr (*) 1 [1..n]

headFold :: [a] -> Maybe a
headFold = foldr (\a b -> Just a) Nothing




{-
2.7.5
The library function init removes the last element from a non-empty list; 
for example, init [1,2,3,4,5] = [1,2,3,4]. Show how init could similarly 
be defined in two different ways.
-}

rmLast :: [a] -> [a]
rmLast [x] = []
rmLast (x:xs) = x : rmLast xs
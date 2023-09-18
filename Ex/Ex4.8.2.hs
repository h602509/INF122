{-
4.8.2
Define a function third :: this many elements using:

a. head and tail;
b. list indexing !!;
c. pattern matching.

[a] -> a that returns the third element in a list that contains at least
-}

thirdA1 :: [a] -> a
thirdA1 [x] = x
thirdA1 (x:y:ys) = head ys
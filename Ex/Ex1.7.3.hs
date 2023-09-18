{-
1.7.3
Define a function product that produces the product of a list of numbers, 
and show using your definitionthatproduct [2,3,4]=24.
-}

produkt :: [Integer] -> Integer
produkt [] = 1
produkt [x] = x
produkt (x:y:ys) = x * y * produkt ys 

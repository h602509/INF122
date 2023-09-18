-- example of using guards instead of if else

signum :: Integer -> Integer
signum x = if x < 0 then (-1) else 
            if x == 0 then 0 else 1

signGuard :: Integer -> Integer
signGuard x | x < 0 = -1
            | x == 0 = 0
            | otherwise = 1

{-
function x | condition1 = output1
           | condition2 = output2
           | otherwisre = (all other outputs)
-}
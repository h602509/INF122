module Week37Exercise1 where

f :: Integer -> Integer -> Integer -> Integer -> Integer
f x y z n = x^n + y^n - z^(n-1)

semiFermat :: Integer -> Integer -> [(Integer, Integer, Integer)]
semiFermat n m = [(a,b,c) | a <- [1..n], b <- [a..n], c <- [b..n], f a b c m == 0]
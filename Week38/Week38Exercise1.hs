module Week38Exercise1 where

combinations :: Integer -> [Char] -> [String]
combinations 0 _ = [""]
combinations n s = [x:xs | x <- s, xs <- combinations (n-1) s]

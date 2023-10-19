

f :: [Integer] -> [Integer]
f = map (+3) . reverse

{-
map1 f = foldr (\a b -> f a : b) []
map1 f = foldr (\a -> f a :) []
map1 f = foldr (\a b -> ()(f a)) []
map1 f = foldr ((:) . f) []
map1 f = flip foldr [] . ((:) . f)
map1 = flip foldr [] . ((:) . )
-}

map1 :: (a -> b) -> [a] -> [b]
map1 = flip foldr [] . ((:) . )


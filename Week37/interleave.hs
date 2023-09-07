
interleave :: [a] -> [a] -> [a]
interleave l1 l2= concat $ zipWith (\x y -> [x,y]) l1 l2
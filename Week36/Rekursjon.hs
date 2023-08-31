
duplicate :: [a] -> [a]
duplicate [] = []
duplicate (a : as) = a : a: duplicate as


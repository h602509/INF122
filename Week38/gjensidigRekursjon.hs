
odds :: [a] -> [a]
odds [] = []
odds (x:xs) = evens xs

evens :: [a] -> [a]
evens [] = []
evens (x:xs) = x : odds xs
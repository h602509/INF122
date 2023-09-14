-- Skriv en funksjon for å skjekke om en liste er sortert ved bruk av rekursjon

erSortert :: Ord a => [a] -> Bool
erSortert [] = True
erSortert (x:[]) = True
erSortert (x:(y:ys)) = x <= y && erSortert (y:ys)

-- Matche basert på type

filterEmpty :: [[a]] -> [[a]]
filterEmpty [] = []
filterEmpty ([]:xs) = filterEmpty xs 
filterEmpty (x:xs) = x : filterEmpty xs

addPairs :: (Num a) => [(a,a)] -> [a] 
addPairs [] = []
addPairs ((x,y):as) = (x + y) : addPairs as
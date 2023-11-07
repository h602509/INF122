module Week42Exercise2 where


isFiveMultiples :: [Integer] ->  Bool
isFiveMultiples = notElem False . map (\x -> (mod x 5) == 0)


factorial :: Integer -> Integer
factorial = foldr (*) 1 . enumFromTo 1 
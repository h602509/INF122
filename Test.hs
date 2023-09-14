digits :: Integer -> [Integer]
digits n = map read $ reverse $ digHelp $ show n

digHelp :: String -> [String]
digHelp [] = []
digHelp (x:xs) = [x] : digHelp xs 

removeZero :: [[Integer]] -> [Maybe [Integer]]
removeZero = map zHelp

zHelp :: [Integer] -> Maybe [Integer]
zHelp x = if sum x == 0 
    then Nothing
    else Just x
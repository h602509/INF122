module Week36Exercise0 where

f :: String -> Char -> Bool
f s c = not (c `elem` s)
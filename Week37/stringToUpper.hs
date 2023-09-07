import Data.Char
stringToUpper' :: String -> String
stringToUpper' str = [toUpper a | a <- str]

stringToUpper'' :: String -> String
stringToUpper'' = map toUpper
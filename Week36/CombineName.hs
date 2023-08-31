

combineNames :: String -> String -> Maybe Int -> String
combineNames f l Nothing = f ++ " " ++ l
combineNames f l y = f ++ " " ++ l ++ " where born the year " ++ show y


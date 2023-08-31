module Week36Exercise2 where

-- a)

firstHalf :: String -> String
firstHalf s = take (length s `div` 2) s

lastHalf :: String -> String
lastHalf s = drop ((length s + 1) `div` 2) s

semiRepetitive :: String -> Maybe String
semiRepetitive s =
    let headStr = firstHalf s
        taleStr = lastHalf s

    in if headStr == taleStr then
        Just headStr
        else
            Nothing

--b)

getCenterChar :: String -> Char
getCenterChar s = s !! ((length s - 1) `div` 2)

decomposeSemiRepetitive :: String -> Maybe (String, Maybe Char)
decomposeSemiRepetitive s = case semiRepetitive s of 
    Nothing -> Nothing
    Just str -> 
        if even (length s) then
            Just (str, Nothing)
            else
                Just (str, Just (getCenterChar s))

--c)

createSemiRepetitive :: String -> Maybe Char -> String
createSemiRepetitive s (Just c) = s ++ [c] ++ s
createSemiRepetitive s Nothing = s ++ s

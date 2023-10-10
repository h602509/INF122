module Week40Exercise0 where

type SemiRepetitive = (String, Maybe Char)


semiRepetitive :: String -> Maybe SemiRepetitive
semiRepetitive s = 
    let s' = (take (length s `div` 2) s, drop ((length s + 1) `div` 2) s)
    in if even (length s)
        then if fst s' == snd s'
            then Just (fst s', Nothing)
            else Nothing
        else 
            let c = s !! (length s `div` 2)
            in if fst s' == snd s'
                then Just (fst s', Just c)
                else Nothing

toString :: SemiRepetitive -> String
toString (s,Nothing) = s ++ s 
toString (s,Just c) = s ++ [c] ++ s 

    
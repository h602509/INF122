module Week37Exercise0 where

informasjon :: [String] -> [String] -> [Integer] -> [String]
informasjon name course year =
    let list = zip3 name course (map show year)
    in map f list

f :: (String, String, String) -> String
f (s1, s2, s3) = s1 ++ " is studying at " ++ s2 ++ " department and started in " ++ s3

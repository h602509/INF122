module Week37Exercise0 where

information :: [String] -> [String] -> [Integer] -> [String]
information name course year = map f $ filter (\ (_, _, year) -> year >= 2022) $ zip3 name course year

f :: (String, String, Integer) -> String
f (s1, s2, s3) = s1 ++ " is studying at " ++ s2 ++ " department and started in " ++ show s3

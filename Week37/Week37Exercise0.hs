module Week37Exercise0 where

n = ["Farhad", "Tina", "Alex"] 
c = ["Informatics", "Biological Sciences", "Mathematics"] 
y = [2022, 2023, 2019]

informasjon :: [String] -> [String] -> [Integer] -> [String]
informasjon name course year =
    let list = zip3 name course (map show year)
    in map f list

f :: (String, String, String) -> String
f (s1, s2, s3) = s1 ++ " is studying at " ++ s2 ++ " department and started in " ++ s3
import qualified Data.Map as Map
import Control.Monad (guard)
main = do
    putStrLn "What is your name? "
    name <- getLine
    putStrLn $ "Hello, " ++ name ++ "!"


ages :: Map.Map String Integer
ages = Map.fromList [("Ole",14),("Per",68),("Anna",94)]

names1 = ["Ole","Per"]
names2 = ["Anna","Eva"]

str name = do
    age <- Map.lookup name ages
    return $ name ++ " er " ++ show age ++ " years old"

record names = sequence (map str names)


triples = do
    a <- [1..10]
    b <- [1..10]
    c <- [1..50]
    guard (a*a + b*b == c*c)
    return (a,b,c)


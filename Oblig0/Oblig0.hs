module Oblig0 where

import Data.List (group, sort)
import Data.Char (ord)
import GHC.Char (chr)
import GHC.OldList (sortBy)
import Data.Function (on)
import Data.Set (Set)
import qualified Data.Map as Map


type Key = [(Char,Char)]
type FrequencyTable = [(Char,Double)]
type Alphabet = String
type Dictionary = Set String

encode :: Key -> String -> String
encode k = map (lookUp k)

lookUp :: Key -> Char -> Char
lookUp [] c = c
lookUp [k] c | fst k == c = snd k
             | otherwise = c
lookUp (k:ks) c | fst k == c = snd k
                | otherwise = lookUp ks c

decode :: Key -> String -> String
decode k = map (lookUp $ invert k)

invert :: Key -> Key
invert [] = []
invert [(x,y)] = [(y,x)]
invert ((x,y):xs) = (y,x) : invert xs

count :: String -> FrequencyTable
count s =
  let n = toInteger $ length s
  in map (countGroup n) (group $ sort s)

countGroup :: Integer -> String -> (Char, Double)
countGroup n s = (head s, fromIntegral (length s) / fromIntegral n)

caesar :: Alphabet -> Integer -> Key
caesar alphabet shift = map (\x -> (x, int2let $ (let2int x + fromInteger shift) `mod` length alphabet)) alphabet

let2int :: Char -> Int
let2int c = ord c - ord 'a'

int2let :: Int -> Char
int2let n = chr (ord 'a' + n)

loadFrequencyTable :: FilePath -> IO FrequencyTable
loadFrequencyTable filePath = do
  content <- readFile filePath
  return (count content)

initialGuess :: FrequencyTable -> FrequencyTable -> Key
initialGuess model observation = do
  let
    sortedModel = sortBy (compare `on` snd) model
    sortedObs = sortBy (compare `on` snd) observation
    in zipWith matchChars sortedModel sortedObs

matchChars :: (Char, Double) -> (Char, Double) -> (Char, Char)
matchChars x y = (fst x, fst y)

chiSquared :: FrequencyTable -> FrequencyTable -> Double
chiSquared mod obs = let m = Map.fromList mod
                         o = Map.fromList obs
                         mo = Map.keys $ Map.union o m
                     in sum $ map (chiHelp m o) mo

chiHelp :: Map.Map Char Double -> Map.Map Char Double -> Char -> Double 
chiHelp mod obs c = chisqr (Map.findWithDefault (1/10000) c mod) (Map.findWithDefault 0 c obs)

chisqr :: Double -> Double -> Double
chisqr mod obs = ((obs-mod)^2)/mod

neighbourKeys :: Key -> [Key]
neighbourKeys key = undefined

swapEntries :: (Char,Char) -> (Char, Char) -> Key -> Key
swapEntries (c1, e1) (c2, e2) key = undefined


greedy :: FrequencyTable -> String -> Key -> Key
greedy model cipherText initKey = undefined

loadDictionary :: FilePath -> IO Dictionary
loadDictionary fil = undefined

countValidWords :: Dictionary -> String -> Integer
countValidWords dict = undefined

greedyDict :: Dictionary -> String -> Key -> Key
greedyDict dict cipherText initKey = undefined

-- fault in this test
-- {
-- ghci> decode guessedKey2 (encode key "home") == "home"
-- False
-- }
-- 
-- output of this should be "home"
-- ghci> decode guessedKey2 (encode key "home")
-- "git "
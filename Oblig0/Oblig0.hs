module Oblig0 where

import Data.List (group, sort, minimumBy, maximumBy, sortBy)
import Data.Char (ord)
import Data.Function (on)
import Data.Set (Set)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.IntMap (keys)
import Data.Ord (comparing)

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
caesar alphabet shift = let shift' = mod (fromIntegral shift ) (length alphabet)
                            encrypted = drop shift' alphabet ++ take shift' alphabet
  in zip alphabet encrypted

loadFrequencyTable :: FilePath -> IO FrequencyTable
loadFrequencyTable filePath = do
  content <- readFile filePath
  return (count content)

initialGuess :: FrequencyTable -> FrequencyTable -> Key
initialGuess model observation = do
  let
    sortedModel = sortBy (flip compare `on` snd) model
    sortedObs = sortBy (flip compare `on` snd) observation
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

--Sammarbeid med Stein Olav Løset Frey
--neighbourKeys xs = [swapEntries x y xs | x <- xs ,y <- xs, x > y]
neighbourKeys :: Key -> [Key]
neighbourKeys xs = [swapEntries x y xs | (x,n) <- zip xs [1..], y <- drop n xs]

swapEntries :: (Eq a) => (a, b) -> (a, b) -> [(a, b)] -> [(a, b)]
swapEntries (c1, e1) (c2, e2) key = let
  a1 = (c1,e1)
  a2 = (c2,e2)
  b1 = (c2,e1)
  b2 = (c1,e2)
  in map (\x -> if fst x == fst a1 then b2 else if fst x == fst a2 then b1 else x) key

-- I sammarbeid med Espen Grepstad, Får timeout på test 3.1
greedy :: FrequencyTable -> String -> Key -> Key
greedy model cipherText initKey =
    let
        currentChiSquared = chiSquared model (count (decode initKey cipherText))
        bestNeighbor = minimumBy (comparing (\key -> chiSquared model (count (decode key cipherText)))) (neighbourKeys initKey)
    in
        if chiSquared model (count (decode bestNeighbor cipherText)) < currentChiSquared
        then greedy model cipherText bestNeighbor
        else initKey

-- Sammarbeid med Stein Olav Løset Frey, denne feiler på test 3.2
{-}
greedy :: FrequencyTable -> String -> Key -> Key
greedy model cipherText initKey = minimumBy (comparing (\key -> chiSquared model (count (decode key cipherText)))) (neighbourKeys initKey)
-}

{-
-- Laget selv, feiler også på 3.2, man har og for høy verdi på lokal test
greedy :: FrequencyTable -> String -> Key -> Key
greedy model cipherText initKey = let
  keys = neighbourKeys initKey
  frqTbl = count cipherText
  keyChiList = greedy' keys model frqTbl
  in snd $ minimumBy compareDoubles keyChiList 

greedy' :: [Key] -> FrequencyTable -> FrequencyTable -> [(Double,Key)]
greedy' [] _ _ = []
greedy' [k] mod obs = [(chiSquared mod obs, k)]
greedy' (k:ks) mod obs = (chiSquared mod obs, k) : greedy' ks mod obs

compareDoubles :: (Double,Key) -> (Double,Key) -> Ordering
compareDoubles (x,_) (y,_) = compare x y
-}

loadDictionary :: FilePath -> IO Dictionary
loadDictionary filePath = do
  dict <- readFile filePath
  let wordList = words dict
  return (Set.fromList wordList)

countValidWords :: Dictionary -> String -> Integer
countValidWords d s = let wordList = words s
                          isMember ss s = if Set.member s ss then 1 else 0
  in sum $ map (isMember d) wordList

-- I sammarbeid med Stein Olav Løset Frey
greedyDict :: Dictionary -> String -> Key -> Key
greedyDict dict cipherText initKey =
    let
        decodeInitKey = decode initKey cipherText
        validWords = countValidWords dict decodeInitKey

        findBestKey :: Key -> Key -> Key
        findBestKey currentBestKey nextKey
            | countValidWords dict (decode nextKey cipherText) > countValidWords dict (decode currentBestKey cipherText) = nextKey
            | otherwise = currentBestKey

        bestKey = foldl findBestKey initKey (neighbourKeys initKey)
        bestWords = countValidWords dict (decode bestKey cipherText)
    in
        if bestWords > validWords
        then greedyDict dict cipherText bestKey
        else initKey

{-
-- Sammarbeid med Stein Olav Løset Frey
greedyDict :: Dictionary -> String -> Key -> Key
greedyDict dict cipherText initKey = do
  let keys = neighbourKeys initKey
    in maximumBy (comparing (\key -> countValidWords dict (decode key cipherText))) keys
-}

{-
-- I sammarbeid med Espen Grepstad, Får timeout på test 3.1
greedyDict :: Dictionary -> String -> Key -> Key
greedyDict dict cipherText initKey =
    let
        validWords = countValidWords dict (decode initKey cipherText)
        bestKey = maximumBy (comparing (\key -> countValidWords dict (decode key cipherText))) (neighbourKeys initKey)
        bestWords = countValidWords dict (decode bestKey cipherText)
    in
        if  bestWords > validWords
        then greedyDict dict cipherText bestKey
        else initKey
-}
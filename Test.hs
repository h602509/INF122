import qualified Data.Set as Set
import Data.Char (ord, chr)

type Key = [(Char,Char)]
type FrequencyTable = [(Char,Double)]
type Alphabet = String
type Dictionary = Set.Set String


caesar :: Alphabet -> Integer -> Key
caesar alphabet shift = map (\x -> (x, int2let $ (let2int x + fromInteger shift) `mod` length alphabet)) alphabet


let2int :: Char -> Int
let2int c = ord c - ord 'a'

int2let :: Int -> Char
int2let n = chr (ord 'a' + n)

encode :: Key -> String -> String
encode k = map (lookUp k)

lookUp :: Key -> Char -> Char
lookUp [] c = c
lookUp [k] c | fst k == c = snd k
             | otherwise = c
lookUp (k:ks) c | fst k == c = snd k
                | otherwise = lookUp ks c

decode :: Key -> String -> String
decode k = map (lookUpDecode k)

lookUpDecode :: Key -> Char -> Char
lookUpDecode [] c = c
lookUpDecode [k] c | snd k == c = fst k
                   | otherwise = c
lookUpDecode (k:ks) c | snd k == c = fst k
                      | otherwise = lookUpDecode ks c

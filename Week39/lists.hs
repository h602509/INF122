import Data.Sequence (Seq)
import GHC.Event (Event)

data List a = Empty
            | Cons a (List a)


leng :: List a -> Integer
leng Empty = 0
leng (Cons a as) = 1 + leng as


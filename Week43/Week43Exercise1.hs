
module Week43Exercise1 where
import Prelude hiding (fmap, Functor)


-- Tre fra oppgave lagt inn fo testing
rt = Branch [-1,1,-1] [Branch [1,-1] [Branch [] []]]

data RoseTree a = Branch a [RoseTree a]
  deriving (Eq, Show)

class Functor f where
  fmap :: (a -> b) -> f a -> f b

instance Functor RoseTree where
  fmap f (Branch x subTrees) = Branch (f x) (map (fmap f) subTrees)


productNodes :: (Num a) => RoseTree [a] -> RoseTree a
-- productNodes = fmap (foldr (*) 1) roseT
productNodes = fmap product
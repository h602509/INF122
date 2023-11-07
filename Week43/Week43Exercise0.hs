module Week43Exercise0 where
import Prelude hiding (foldr)

-- hiding foldr må legges inn for å forhindre at programmet 
-- har tilgang til den innebygde foldr (lager kompileringsfeil)

-- tre fra oppgavetekst lagt inn for å teste
t = LeftRightChildBranch (LeftRightChildBranch  (Leaf 2) 3 (Leaf 4)) 8 (RightChildBranch 9 (Leaf 10))

data BinSearchTree a
  = Leaf a
  | LeftRightChildBranch (BinSearchTree a) a (BinSearchTree a)
  | LeftChildBranch (BinSearchTree a) a
  | RightChildBranch a (BinSearchTree a)
  deriving (Eq, Show)

instance Foldable BinSearchTree where 
  foldMap = undefined
foldr f start (Leaf x) = f x start
foldr f start (LeftRightChildBranch lTree x rTree) = foldr f (f x (foldr f start rTree)) lTree 
foldr f start (LeftChildBranch lTree x) = foldr f (f x start) lTree 
foldr f start (RightChildBranch x rTree ) = f x (foldr f start rTree) 


toList :: BinSearchTree a -> [a]
toList = foldr (:) []
module Week41Exercise0 where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

type Graph n = Map n (Set n)

bridge :: (Ord n) => n -> n -> Graph n -> Graph n
bridge x y g = undefined

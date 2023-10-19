module Week41Exercise1 where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

type Graph n = Map n (Set n)

disjoint :: (Ord a) => Set a -> Set a -> Bool
disjoint s1 s2 = Set.null $ Set.intersection s1 s2

hasCycle :: (Ord n) => Graph n -> n -> Bool
hasCycle graph start =
    case Map.lookup start graph of
    Nothing -> False
    Just children -> hasCycle' start Set.empty
      where
        hasCycle' current visited
          | not (Map.member current graph) = False
          | Set.member current visited = True
          | otherwise = let
            children = Map.findWithDefault Set.empty current graph
            updatedVisited = Set.insert current visited
            in any (`hasCycle'` updatedVisited) (Set.toList children)

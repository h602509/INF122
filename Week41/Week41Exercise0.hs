module Week41Exercise0 where

import Data.Map (Map, member, insertWith)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe ( listToMaybe, mapMaybe, isJust )

type Graph n = Map n (Set n)

bridge :: (Ord n) => n -> n -> Graph n -> Graph n
bridge start end graph | isJust $ path graph start end = graph
                       | otherwise = let
    graph' = Map.insertWith Set.union end Set.empty graph
    in Map.insertWith Set.union start (Set.fromList [end]) graph'

path :: (Ord node) => Graph node -> node -> node -> Maybe [node]
path g start end = path' g start end Set.empty

path' :: (Ord node) => Graph node -> node -> node -> Set node -> Maybe [node]
path' g start end visited
  | Set.member start visited = Nothing
        -- We have reached a cycle
  | start == end = Just []
  | otherwise
    = do
       let visited' = Set.insert start visited
       nexts <- Map.lookup start g
       listToMaybe
         $ mapMaybe
            (\next -> do
               pathCont <- path' g next end visited'
               Just (next:pathCont))
            (Set.toList nexts)

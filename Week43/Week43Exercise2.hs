{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Week43Exercise2 where
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Map
import Data.Set

class IntegerGraph g where
  emptyGraph :: g
  insertNode :: Integer -> g -> g
  insertEdge :: Integer -> Integer -> g -> g
  nodeInGraph :: Integer -> g -> Bool
  edgeInGraph :: Integer -> Integer -> g -> Bool

type MyGraph node = Map node (Set node)

instance IntegerGraph (MyGraph Integer) where
  emptyGraph = Map.empty
  insertNode node = (Map.insert node) Set.empty 
  insertEdge startNo= undefined
  nodeInGraph = undefined
  edgeInGraph = undefined
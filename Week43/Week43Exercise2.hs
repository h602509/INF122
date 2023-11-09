{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Week43Exercise2 where
import qualified Data.Map as Map
import qualified Data.Set as Set

class IntegerGraph g where
  emptyGraph :: g
  insertNode :: Integer -> g -> g
  insertEdge :: Integer -> Integer -> g -> g
  nodeInGraph :: Integer -> g -> Bool
  edgeInGraph :: Integer -> Integer -> g -> Bool

type MyGraph = Map.Map Integer (Set.Set Integer)

instance IntegerGraph MyGraph where
  
  emptyGraph = Map.empty
  insertNode node = (Map.insert node) Set.empty 


  insertEdge :: Integer -> Integer -> MyGraph -> MyGraph
  insertEdge startNo endNo graph = let
    -- legger inn nodene i grafen hvis de ikke eksisterer
    graph' = insertNode' startNo (Map.lookup startNo graph) graph
    graph'' = insertNode' endNo (Map.lookup endNo graph') graph'
    -- legger inn endNo i StartNo Value
    in Map.adjust (Set.insert endNo) startNo graph''

    where
      insertNode' _ (Just _) graph = graph
      insertNode' node Nothing graph = insertNode node graph

  nodeInGraph = Map.member
  edgeInGraph startNo endNo graph = case Map.lookup startNo graph of
    Nothing -> False
    Just dest -> Set.member endNo dest 


graph :: (IntegerGraph g) => g
graph = insertNode 3 . insertEdge 1 6 . insertEdge 1 8 . insertEdge 5 1 . insertEdge 5 8 . insertEdge 8 5 $ emptyGraph



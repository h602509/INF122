module Week40Exercise2 where

data BinSearchTree a
  = Empty
  | Branch (BinSearchTree a) a (BinSearchTree a)
  deriving (Eq, Show)

toBinarySearchTree :: [a] -> BinSearchTree a
toBinarySearchTree list = 
  let root = Empty
  in toBinarySearchTree' list 

toBinarySearchTree' :: [a] -> BinSearchTree a
toBinarySearchTree' list =
    let midIndex =  div (length list) 2
        lList = take midIndex list
        middle = list !! midIndex
        rList = drop (midIndex + 1) list
    
    in if null list then 
          Empty
       else 
          Branch (toBinarySearchTree' lList) middle (toBinarySearchTree' rList)
            
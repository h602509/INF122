data BinTree a = Empty 
               | Branch (BinTree a) a (BinTree a)
    deriving (Eq,Show)

leaf :: a -> BinTree a
leaf a = Branch Empty a Empty

t1 :: BinTree Integer
t1 = Branch 
    (Branch
        (Branch Empty 1 Empty)
        2 
        (Branch Empty 3 Empty)
    ) 
    4 
    (Branch Empty 5 Empty) 

t2 = Branch (Branch (leaf 1) 2 (leaf 3)) 4 (leaf 5)

heigth :: BinTree a -> Integer
heigth Empty = 0
heigth (Branch lefts _ rights) = 1 + max (heigth lefts) (heigth rights)

sumTree :: BinTree Integer -> Integer
sumTree Empty = 0
sumTree (Branch lefts x rigths) = x + sumTree lefts + sumTree rigths

toList :: BinTree a -> [a]
toList Empty = []
toList (Branch lefts x rigths) = toList lefts ++ [x] ++ toList rigths

-- week32

instance Foldable BinTree where
    
    foldr m z Empty = z
    foldr m z (Branch l x r) = 
        let fr = foldr m z l
            fx = m x fr
            in foldr m fx l


{-

            x
           / \
         l     r

m :: a -> b -> b
x :: a
z :: b

fr :: b
m x fr :: b

-}
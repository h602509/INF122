
data BalanceFactor = LeftHeavy
                   | Balanced
                   | RightHeavy
    deriving(Eq,Show)

data AVLTree a = Empty
               | Branch BalanceFactor
                    (AVLTree a)
                    a
                    (AVLTree a)
    deriving(Eq,Show)

leaf :: a -> AVLTree a
leaf a = Branch Balanced Empty a Empty

insert :: (Ord) a => a -> AVLTree a -> AVLTree a
insert a = undefined

insert' :: (Ord) a => a -> AVLTree a -> (Bool, AVLTree a)
insert' x Empty = (True, leaf x)
insert' x t@(Branch LeftHeavy lesser a greater) 
    | (x < a) = let (grew, newLesser) = insert' x greater
                in if not grew
                    then (False, Branch LeftHeavy newLesser a greater)
                    else case newLesser of 
                        (Branch LeftHeavy lesserb b greaterb) 
                            -> (False, Branch Balanced
                                        lesserb
                                        b
                                        (Branch Balanced
                                                greaterb
                                                a
                                                greater)) 
                        (Branch RightHeavy lesserb b (Branch cbf lesserc c greaterc)) 
                            -> (False, Branch Balanced 
                                                (Branch (if cbf == RightHeavy 
                                                        then LeftHeavy 
                                                        else Balanced) lesserb b lesserc)
                                                c
                                                (Branch (if cbf == LeftHeavy 
                                                        then RightHeavy 
                                                        else Balanced) greaterc a greater))
    | (x > a) = let (grew, newGreater) = insert' x greater
                    newBalance = if grew then Balanced else LeftHeavy
                in (False, Branch newBalance lesser a greater)
    | otherwise = (False, t) 

insert' x (Branch Balanced lesser a greater) 
    | (x < a) = let (grew, newLesser) = insert' x lesser
                    newBalance = if grew then LeftHeavy else Balanced
                in (grew, Branch newBalance
                                newLesser
                                a
                                greater)
    | (x > a) = let (grew, newGreater) = insert' x greater
                    newBalance = if grew then RightHeavy else Balanced
                in (grew, Branch newBalance
                                lesser
                                a
                                newGreater)

insert' x (Branch RightHeavy lesser a greater) = undefined
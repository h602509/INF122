{-
This datastructure should satisfy the invariant
that its elemetn are sorted
-}

data SortedList a = Empty | Cons a (SortedList a)

insert :: (Ord a) => a -> SortedList a -> SortedList a
insert a Empty = Cons a Empty
insert a as@(Cons b bs) = if a <= b
                          then Cons a as
                          else Cons b (insert a bs)

fromList :: (Ord a) => [a] -> SortedList a
fromList [] = Empty
fromList (a:as) = insert a (fromList as)

toList :: SortedList a -> [a]
toList Empty = []
toList (Cons a as) = a : toList as


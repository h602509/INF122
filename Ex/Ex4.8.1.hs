{-
4.8.1
Using library functions, define a function halve :: [a] -> ([a],[a]) that splits 
an even- lengthed list into two halves. For example:
> halve [1,2,3,4,5,6] ([1,2,3],[4,5,6])
-}

halve :: [a] -> ([a],[a])
halve x = splitAt (length x `div` 2) x
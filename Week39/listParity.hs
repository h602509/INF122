data Parity = Odd | Even
    deriving (Eq,Show)

--this function is O(n)
listParity :: [a] -> Parity
listParity [] = Even
listParity (a:as) 
    = case listParity as of
        Even -> Odd
        Odd -> Even

data ParityList a = Empty
                  | Cons Parity a (ParityList a)

cons :: a -> ParityList a -> ParityList a
cons a Empty = Cons Odd a Empty 
cons a as@(Cons Even b bs) = Cons Odd a as
cons a as@(Cons Odd b bs) = Cons Even a as

parity :: ParityList a -> Parity
parity Empty = Even
parity (Cons p _ _) = p

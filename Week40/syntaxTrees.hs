import Language.Haskell.TH (valD)

data Expression a = Constant Integer
                  | Add (Expression a) (Expression a)
                  | Mul (Expression a) (Expression a)
                  | Sub (Expression a) (Expression a)
                  | Var a
    deriving (Eq)

instance (Show a) => Show (Expression a) where
    show (Constant v) = show v
    show (Add e1 e2) = "(" ++ show e1 ++ " + " ++ show e2 ++ ")"
    show (Mul e1 e2) = show e1 ++ " * " ++ show e2
    show (Sub e1 e2) = "(" ++ show e1 ++ " - " ++ show e2 ++ ")"
    show (Var a) = show a

-- ex0 = 3(1-1) + 4

ex0 :: Expression a
ex0 = Add (Mul (Constant 3)
            (Sub (Constant 1)
            (Constant 1)))
        (Constant 4)

-- ex1 = 3x - 1
-- eval (\x -> 3) ex2
ex1 = Sub (Mul (Constant 3)
            (Var "x"))
          (Constant 1)

-- ex2 3x - y

ex2 = Sub (Mul (Constant 3)
            (Var "x"))
          (Var "y")

{-
eval :: (a -> Integer) -> Expression a -> Integer
eval f (Var x) = f x
eval f (Constant v) = v
eval f (Add e1 e2) = (eval f e1) + (eval f e2)
eval f (Mul e1 e2) = (eval f e1) * (eval f e2)
eval f (Sub e1 e2) = (eval f e1) - (eval f e2)
-}

eval :: Expression Integer -> Integer
eval (Var x) = x
eval (Constant v) = v
eval (Add e1 e2) = eval e1 + eval e2
eval (Mul e1 e2) = eval e1 * eval e2
eval (Sub e1 e2) = eval e1 - eval e2

mapE :: (a -> b) -> Expression a -> Expression b
mapE f (Var a) = Var (f a)
mapE f (Constant v) = Constant v
mapE f (Add e1 e2) = Add (mapE f e1) (mapE f e2)
mapE f (Mul e1 e2) = Mul (mapE f e1) (mapE f e2)
mapE f (Sub e1 e2) = Sub (mapE f e1) (mapE f e2)
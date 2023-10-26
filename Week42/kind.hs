
class Eq a where -- a har kind *
    (==) :: a -> a -> Bool

map :: (a -> b) -> [a] -> [b]
map = undefined

mapMaybe :: (a -> b) -> Maybe a -> Maybe b
mapMaybe f Nothing = Nothing
mapMaybe f (Just a) = Just (f a)

mapIO :: (a -> b) -> IO a -> IO b
mapIO f a = do
    x <- a
    return (f x)

class Functor f where -- f har kind * -> *
    fmap :: (a -> b) -> f a -> f b

instance Functor Maybe where
    fmap g Nothing = Nothing
    fmap g (Just a) = Just (g a)

class Applicative f where
    pure :: a -> f a
    (<*>) :: f -> (a -> f b) -> f a -> f b

class Monad f where -- f har kind * -> *
    return :: a -> f as
    (>>=) :: f a -> (a -> f b) -> f b

--- IO String -> (String -> IO ()) -> IO ()
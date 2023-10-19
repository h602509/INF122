module Week42Exercise1 where

fromLeftAndRight :: (Either a b -> c) -> (a -> c, b -> c)
fromLeftAndRight = undefined


either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' = undefined


toFstAndSnd :: (a -> (b, c)) -> (a -> b, a -> c)
toFstAndSnd = undefined


pair :: (a -> b) -> (a -> c) -> a -> (b, c)
pair = undefined
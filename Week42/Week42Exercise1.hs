module Week42Exercise1 where

fromLeftAndRight :: (Either a b -> c) -> (a -> c, b -> c)
fromLeftAndRight f = (leftFunc, rightFunc)
  where
    leftFunc a = f (Left a)
    rightFunc b = f (Right b)

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' fa _ (Left a) = fa a
either' _ fb (Right b) = fb b


toFstAndSnd :: (a -> (b, c)) -> (a -> b, a -> c)
toFstAndSnd f = (fst . f, snd . f)


pair :: (a -> b) -> (a -> c) -> a -> (b, c)
pair fa fb x = (fa x, fb x)
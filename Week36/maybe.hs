
maybe :: b -> (a -> b) -> Maybe a -> b
maybe _ f (Just value) = f value
maybe def f Nothing = def

fromMaybe :: a-> Maybe a -> a
fromMaybe _ (Just value) = value
fromMaybe def Nothing = def

fmap :: (a -> b) -> Maybe a -> Maybe b
fmap f Nothing = Nothing
fmap f (Just value) = Just (f value)


module Week36Exercise1 where

f :: [Integer] -> [t] -> [(Integer, t)]
f n t = reverse $ zip n t
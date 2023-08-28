module Week36.Week36Exercise1 where

f :: [Integer] -> [t] -> [(Integer, t)]
f n t = zip n `reverse` t
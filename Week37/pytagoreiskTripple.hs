
pyt :: [(Integer, Integer, Integer)]
pyt = [(a, b, c) | a <- [1..200], b <- [a..200], c <- [b..600], a^2 + b^2 == c^2]
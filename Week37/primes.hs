
isPrime :: Integer -> Bool
isPrime x = all (\y -> mod x y /= 0)
                (takeWhile (\y -> y*y <= x) primes)

primes :: [Integer]
primes = 2 : filter isPrime [3..]
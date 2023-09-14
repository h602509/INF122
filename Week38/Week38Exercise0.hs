module Week38Exercise0 where
      
runningSum :: [Integer] -> [Integer]
runningSum = runningSumHelper 0
  where
    runningSumHelper _ [] = []
    runningSumHelper sum (x:xs) = (sum + x) : runningSumHelper (sum + x) xs
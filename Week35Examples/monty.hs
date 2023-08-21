

import Probability


data Door = A | B | C
   deriving (Eq, Show, Ord)

data GameResult = Win | Loose
   deriving (Eq, Show, Ord)

door :: (Fractional d) => Distribution d Door
door = uniform [A,B,C]

-- Vi antar at vi alltid velger dør A først

type Strategy = Door -> Door

stay, switch  :: Strategy

stay reveal = A
switch B = C
switch C = B


computeResult :: Door -> Door -> GameResult
computeResult winDoor guess
  = if winDoor == guess
       then Win
       else Loose

game :: (Fractional d) => Strategy -> Distribution d GameResult
game strategy = bind door (\winDoor ->
    (!(computeResult winDoor . strategy))
       $ case winDoor of
           A -> uniform [B,C]
           B -> force C
           C -> force B)




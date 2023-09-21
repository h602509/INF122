data Month = January
           | February
           | March
           | April
           | May
           | June
           | July
           | August
           | September
           | October
           | November
           | December
    deriving (Eq, Show)

newtype Year = Year Integer
    deriving (Eq, Show)

data Date = Date Year Month Integer
    deriving (Eq, Show)

divisible :: Integer -> Integer -> Bool
divisible x y = mod x y == 0

isLeapYear :: Year -> Bool
isLeapYear (Year n) = divisible n 4
                    && (not (divisible n 100)
                    || (divisible n 400))


daysOf :: Year -> Month -> Integer
daysOf _ January = 31
daysOf y February = if isLeapYear y then 29 else 28
daysOf _ March = 31
daysOf _ April = 30
daysOf _ May = 31
daysOf _ June = 30
daysOf _ July = 31
daysOf _ August = 31
daysOf _ September = 30
daysOf _ October = 31
daysOf _ November = 30
daysOf _ December = 31

isValidDate :: Year -> Month -> Integer -> Bool
isValidDate y m d = 1 <= d && d <= daysOf y m

date :: Year -> Month -> Integer -> Maybe Date
date y m d = if isValidDate y m d
             then Just Date y m d
             else Nothing
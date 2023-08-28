module FinnTallet where


guess start end =
    if start == end
        then putStrLn $ "your number is " ++ show start
        else ask start end

ask start end = do
    let middle = div (start + end) 2 
    putStrLn $ "Is your number bigger than " ++ show middle ++ "?"
    answer <- getLine
    case answer of
        "y" -> guess (middle + 1) end 
        "n" -> guess start middle
        _ -> ask start end


main = do
    putStrLn "Think of a number from 0 to 100"
    guess 1 100
     

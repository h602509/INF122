module Main where

    main = do
        putStrLn "Hei, hva heter du?"
        name <- getLine
        putStrLn ("Hyggelig å møte deg, " ++ name ++ "!")
module Rush where

    notNull = filter (/=0) [1,2,0,7,0,5]
    doubleList = map (*2) [1,2,3]




    hasNotChar :: String -> Char -> Bool
    hasNotChar s c = notElem c s

    removeWordsWithChar :: String -> Char -> String
    removeWordsWithChar s c =
        let wordList = words s
            wordListRemovedWords = filter  (hasNotChar wordList c) wordList
        in unwords wordListRemovedWords 

    rushAlbumYears :: [Integer]
    rushAlbumYears = filter(/=1979) [1976..1982]

    rushAlbumTitles =   ["2112"
                        , "A Farewell to Kings" 
                        , "Hemispheres"
                        , "Permanent Waves"
                        , "Moving Pictures"
                        , "Signals"]
    
    rushAlbums :: [(Integer, String )]
    rushAlbums = zip rushAlbumYears rushAlbumTitles 

    displayAlbums :: String -> Integer -> String -> String
    displayAlbums band year title = "In " ++ show year ++ " " ++ band ++ " released " ++ title

    main = putStrLn
        $ unlines
        $ map (uncurry (displayAlbums "Rush"))
        $ rushAlbums
data CelestialObject = Star String Integer
                     | Planet String String
                     | Moon String String


solarSystem :: [CelestialObject]
solarSystem = [Star "The Sun" 4600000000000,
               Planet "Mercury" "The Sun",
               Planet "Venus" "The Sun",
               Planet "Earth" "The Sun", 
               Moon "The Moon" "Earth",
               Planet "Mars" "The Sun", 
               Moon "Phobos" "Mars",
               Moon "Deimos" "Mars"]



displayInfo :: CelestialObject -> String
displayInfo (Star name age)
    = "The star " ++ name
    ++ " is " ++ show age ++ " years old." 
displayInfo (Planet name star)
    ="The planet " ++ name
    ++ " orbits " ++ star ++ "." 
displayInfo (Moon name planet)
    ="The moon " ++ name
    ++ " orbits " ++ planet ++ "." 

-- map displayInfo solarSystem
-- dette skriver ut alle objektene i listen
import Control.Exception (handle)
import Text.Read (readMaybe)

normalise :: (Double, Double, Double) -> (Double, Double, Double)
normalise (x, y, z) = (x/n, y/n, z/n) where
    n = norm (x, y, z)

-- Curried form
norm' :: Double -> Double -> Double -> Double
norm' x y z = sqrt (x^2 + y^2 + z^2)

-- Uncurried form
norm :: (Double, Double, Double) -> Double
norm (x, y, z) = sqrt (x^2 + y^2 + z^2)

handleInput :: Maybe (Double, Double, Double) -> IO ()
handleInput Nothing 
    = do
        putStrLn "Please enter a valid vector"
handleInput (Just vector)
    = do
        let result = normalise vector
        putStrLn $ "Normalised vector: " ++ show result

main = do
    putStrLn "Enter a three dimensional vector:"
    input <- getLine
    handleInput (readMaybe input)
import Control.Monad ((>=>))

-- Monad function composition examples

sayHi :: String -> IO String
sayHi greeting = do
  putStrLn greeting
  getLine

readM :: Read a => String -> IO a
readM = return . read

getAge :: String -> IO Int
getAge = sayHi >=> readM -- >=> is the kleisli composition operator

askForAge :: IO Int
askForAge = getAge "Hello! How old are you? "

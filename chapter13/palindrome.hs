import Control.Monad (forever)
import Data.Char
import System.Exit (exitSuccess)

isPalindrome :: String -> Bool
isPalindrome x = w == reverse w
  where f = filter (\x -> x /= '\'' && x /= ',')
        w = f $ concat $ words $ map toLower x

palindrome :: IO ()
palindrome = forever $ do
  line1 <- getLine
  case (line1 == reverse line1) of
    True  -> do
      putStrLn "It's a palindrome!"
      return ()
    False -> do
      putStrLn "Nope!"
      exitSuccess

main :: IO ()
main = palindrome

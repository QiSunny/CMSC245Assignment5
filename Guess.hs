module Main where
import System.Random

main :: IO ()
main = do
 putStrLn "We generate a random integer between 1 and 10."
 num <- randomRIO (1, 10)
 input <- judge num
 putStrLn $ input

judge :: Integer -> IO String
judge num = do
  putStrLn "Guess the number we generated:"
  input <- getLine
  case readMaybe input of
    Nothing ->
      putStrLn "Please enter a number."
    Just n
      | n == num = putStrLn "You guessed correctly!"
      | otherwise = judge num

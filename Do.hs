{- Author: Yudi Qi
 File: Do.hs
 Practice exercises with do-notation
-}

module Main where
main :: IO ()
main = do
 putStrLn "Hello, world!"
-- putStrLn "What is your name?"
 name <- prompt "What is your name?"
 putStrLn $ "Hello, " ++ name
 str <- prompt2 "Enter two lines of texts: "
 putStrLn $ str

prompt :: String -> IO String
prompt query = do
 putStrLn query
 answer <- getLine
 pure answer

prompt2 :: String -> IO String
prompt2 query = do
  putStrLn query
  str1 <- getLine
  str2 <- getLine
  pure $ str1 ++ str2

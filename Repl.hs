-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Java.Repl
-- Copyright   :  (C) 2018 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (rae@cs.brynmawr.edu)
-- Stability   :  experimental
-- Portability :  portable
--
-- Implements a REPL for Java.
--
----------------------------------------------------------------------------

module Language.Java.Repl ( repl ) where

import Language.Java.Monad
import Language.Java.Mutate
import Language.Java.Lexer
import Language.Java.TypeCheck
import Language.Java.Execute
import Language.Java.Pretty
import Language.Java.PrettyM
import Language.Java.Parser
import Language.Java.State

import System.Console.Haskeline

import Control.Monad.Except
import Data.Char
import Data.List
import Text.Read ( readMaybe )

-- | The Stitch interpreter
repl :: IO ()
repl = runInputT defaultSettings $
       runJava $ do
         helloWorld
         loop

loop :: Java ()
loop = do
  m_line <- prompt "Java> "
  case stripWhitespace <$> m_line of
    Nothing          -> quit
    Just (':' : cmd) -> runCommand cmd
    Just str         -> runStatement str
    `catchError`
    printLine
  loop

-- | Prints welcome message
helloWorld :: Java ()
helloWorld = do
  printLine $ "Welcome to the Java interpreter."
  printLine $ "Type `:?` at the prompt for the list of commands."

-------------------------------------------
-- running statements

runStatement :: String -> Java ()
runStatement str = do
    toks <- lexJava str
    if null toks
      then pure () -- blank line (or comment); skip
      else do
        stmt <- parseStatement toks
        typeCheckStatement stmt
        result <- execStatement stmt
        case result of
          NothingR  -> pure ()
          ValueR v  -> printValue v

-------------------------------------------
-- commands

-- | Interpret a command (missing the initial ':').
runCommand :: String -> Java ()
runCommand line
  = case filter ((cmd `isPrefixOf`) . fst) cmdTable of
      []            -> do printLine $ "Unknown command: " ++ cmd
      [(_, action)] -> action arg
      many          -> do printLine $ "Ambiguous command: " ++ cmd
                          printLine $ "Possibilities: " ++ intercalate ", " (map fst many)
  where (cmd, arg) = break isSpace line

cmdTable :: [(String, String -> Java ())]
cmdTable = [ ("quit", quitCmd)
 -- TODO   , ("load", loadCmd)
           , ("type", typeCmd)
           , ("heap", heapCmd)
           , ("?",    helpCmd)
           , ("{",    bracesCmd)
           ]

-- | Accepts multiple lines of input, then processes the input as a statement
bracesCmd :: String -> Java ()
-- NB: prompt strips the newline, but we want it back, both for comment termination
-- and whitespace between tokens
bracesCmd first_line = do
  rest_input <- braces_loop
  runStatement (first_line ++ "\n" ++ rest_input)

  where
    -- collect a String until a :} on its own line
    braces_loop :: Java String
    braces_loop = do
      m_line <- prompt "|> "
      case m_line of
        Nothing -> braces_loop
        Just line
          | stripWhitespace line == ":}" -> pure ""
          | otherwise                    -> do rest_input <- braces_loop
                                               pure (line ++ "\n" ++ rest_input)

--echocmd :: String -> Java()
--echocmd str = printLine str

-- | Reports the type of the given expression, without evaluating it
typeCmd :: String -> Java ()
typeCmd expr_str = do
  toks <- lexJava expr_str
  expr <- parseExpression toks
  ty <- typeCheckExpression expr
  printLine (ppr ty)

{- TODO
loadCmd file = do
  let file_name = stripWhitespace file
  file_exists <- liftIO $ doesFileExist file_name
  if not file_exists
    then file_not_found
    else do
      contents <- liftIO $ readFile file
      runFile contents
  where
    file_not_found = do
      printLine (text "File not found:" <+> squotes (text file))
      cwd <- liftIO getCurrentDirectory
      printLine (parens (text "Current directory:" <+> text cwd))
-}

heapCmd :: String -> Java ()
heapCmd num
  | null stripped_num
    -- print entire heap
  = do JavaState { jst_next_ptr = heap_limit } <- getState
       mapM_ printHeapAt [initialHeapPtr .. heap_limit-1]

  | otherwise
    -- try to convert the stripped_num into a number
  = case readMaybe stripped_num of
      Just heap_num -> printHeapAt heap_num
      Nothing       -> issueError $ "Unknown heap element: " ++ num

    -- this "where" clause scopes over all the guards
  where
    stripped_num = stripWhitespace num

-- | Prints help text
helpCmd :: String -> Java ()
helpCmd _ = do
  printLine "Available commands:"
  printLine " :quit             Quits the interpreter"
--  printLine " :load <filename>  Loads a file with ;-separated Java statements"
  printLine " :type <expr>      Type-check an expression and report the type"
  printLine " :heap <num>       Show the element in the heap @<num>"
  printLine " :heap             Show the entire heap"
  printLine " :{                Starts multi-line input; end with :} on a line by itself"
  printLine " :?                Shows this help text"
  printLine "You may also type a statement to execute it and display the result."

-- | Exits the interpreter
quitCmd :: String -> Java ()
quitCmd _ = quit

----------------------------------------------
-- utility

printValue :: Value -> Java ()
printValue val = do
  val_str <- pprM defaultHeapUnrollCount val
  printLine val_str

printHeapAt :: Int -> Java ()
printHeapAt ptr = do
    -- use the "Checked" version of dereference, because the user might
    -- have provided a negative number or too high a number
  heap_elt <- dereferenceRawChecked ptr
  elt_str <- pprM defaultHeapUnrollCount heap_elt
  printLine $ "@" ++ show ptr ++ " => " ++ elt_str

-- | (Inefficiently) strips whitespace from a string
stripWhitespace :: String -> String
stripWhitespace = dropWhile isSpace . dropWhileEnd isSpace

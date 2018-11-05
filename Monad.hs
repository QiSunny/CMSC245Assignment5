{-# LANGUAGE GeneralizedNewtypeDeriving, ScopedTypeVariables #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Java.Monad
-- Copyright   :  (C) 2018 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (rae@cs.brynmawr.edu)
-- Stability   :  experimental
--
-- The Java monad, allowing for failing
-- with an error message, and tracking global state.
--
----------------------------------------------------------------------------

module Language.Java.Monad (
  -- * The 'Java' monad
  Java, runJava,
  getState, setState, ioAction,
  prompt, quit, printLine, printString, eitherToJava,
  issueError, unimplementedM
  ) where

import Language.Java.State

import System.Console.Haskeline

import Control.Monad.Trans.Maybe
import Control.Monad.Except
import Control.Monad.State

-- | A monad giving Haskeline-like interaction, access to 'JavaState',
-- the ability to abort (successfully) with 'mzero', and error handling
-- via Strings.
newtype Java a = Java { unJava :: ExceptT String (MaybeT (StateT JavaState (InputT IO))) a }
  deriving (Monad, Functor, Applicative, MonadState JavaState, MonadError String, MonadIO)

-- | Print a 'String' without a newline at the end
printString :: String -> Java ()
printString = liftIO . putStr

-- | Print a 'String' with a newline at the end
printLine :: String -> Java ()
printLine = liftIO . putStrLn

-- | Retrieve the current state of the interpreter
getState :: Java JavaState
getState = get   -- get is from Control.Monad.State

-- | Set the current state of the interpreter
setState :: JavaState -> Java ()
setState = put   -- put is from Control.Monad.State

-- | Perform an action in the IO monad
ioAction :: IO a -> Java a
ioAction = liftIO

-- | Prompt the user for input, returning a string if one is entered.
-- Like 'getInputLine'. (Strips the newline off the input.)
prompt :: String -> Java (Maybe String)
prompt = Java . lift . lift . lift . getInputLine  -- oy.

-- | Abort the 'Java' monad
quit :: Java a
quit = do
  printLine "Good-bye."
  Java (lift mzero)  -- mzero typechecks, but it does the wrong thing: it will
                     -- be as if issueError "". So we use (lift mzero) to access
                     -- the failure in the MaybeT.

-- | Abort the computation with an error
issueError :: String -> Java a
issueError = Java . throwError

-- | Abort the computation with an error about an unimplemented feature.
-- See also 'Panic.unimplemented' if you're not in the Java monad.
unimplementedM :: String -> Java a
unimplementedM msg = issueError $ "Oops! I don't yet know how to " ++ msg

-- | Hoist an 'Either' into 'Java'
eitherToJava :: Either String a -> Java a
eitherToJava = liftEither

-- | Run a 'Java' computation
runJava :: Java () -> InputT IO ()
runJava thing_inside = do
  result :: Maybe (Either String ())
         <- flip evalStateT initialState $ runMaybeT $ runExceptT $ unJava thing_inside
  case result of
    Just (Left err) -> liftIO (putStrLn $ "Uncaught exception: " ++ err)
    _               -> pure ()

-- | Run a 'Java' computation and print the result to the console.
-- Useful for debugging in GHCi.
printJava :: Show a => Java a -> IO ()
printJava thing_inside = runInputT defaultSettings $ runJava $ do
result <- thing_inside
liftIO (print result)

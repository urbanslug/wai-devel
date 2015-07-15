{-|
Module      : Devel.Compile
Description : For building and running your WAI application.
Copyright   : (c)
License     : GPL-3
Maintainer  : njagi@urbanslug.com
Stability   : experimental
Portability : POSIX
-}
module Devel.Build (build) where

import Devel.Watch
-- import Control.Monad.STM
import Control.Concurrent.STM.TVar
--  import Devel.Build (build)
import Devel.Compile (compile)
import Devel.ReverseProxy (runServer, createSocket)

-- import System.FSNotify
-- import Control.Monad      (forever)
import Control.Concurrent (forkIO, killThread, ThreadId)

-- For use within `loop`
import IdeSession
import qualified Data.ByteString.Char8 as S8

-- import Control.Concurrent
-- import System.Exit
-- import Control.Exception.Base

import Network.Socket

-- | Compiles and runs your WAI application.
build :: IO ()
build = do

  -- Either an ideBackend session or a list of errors from `build`.
  eitherSession <- compile

  -- Create a new socket each time.
  sock <- createSocket

  case eitherSession of
    Left  errorList -> do
      -- Start the warp server if the TVar is True.
      _ <- forkIO $ runServer errorList sock

      -- Listen for changes in the current working directory.
      isDirty <- newTVarIO False
      _ <- forkIO $ watch isDirty
      checkForChange isDirty

      -- close the socket so that we may create another in the new build.
      close sock

      -- Restart the whole process.
      restart

    Right session -> do
      --  Run the WAI application in a separate thread.
      _ <- forkIO $ run session sock

      putStrLn "Starting devel server http://localhost:3000"
      _ <- forkIO $ runServer [] sock
      listenForEnter

-- | Invoked when we are ready to run the compiled code.
run :: IdeSession -> Socket -> IO ()
run session sock = do

  -- Run the given ide-backend session.
  runActionsRunResult <- runStmt session "Application" "main"

  threadId  <- forkIO $ loop runActionsRunResult

  -- For watching for file changes in current working directory.
  isDirty <- newTVarIO False

  -- Watch for changes in the current working directory.
  _ <- forkIO $ watch isDirty
  -- Block until change is made then carry on with program execution.
  checkForChange isDirty
  stopApp runActionsRunResult threadId sock

  -- Restart the whole process.
  restart

-- | Restart the whole process.
-- Like calling main in Main but first notifies that
-- it's about to restart.
restart :: IO ()
restart = do
  putStrLn "\n\nRestarting...\n\n"
  build

-- | Listen for ENTER on terminal.
listenForEnter :: IO ()
listenForEnter = do
  putStrLn "Press ENTER to exit."
  input <- getLine
  case input of
    "" -> return ()
    _  -> listenForEnter

-- | Stop the currently running WAI application.
stopApp :: RunActions RunResult -> ThreadId -> Socket -> IO ()
stopApp runResult threadId sock = do
  interrupt runResult
  killThread threadId
  close sock

-- | Run for as long as we need to.
loop :: RunActions RunResult -> IO ()
loop res = do
  runAction <- runWait res
  case runAction of
    Left bs -> S8.putStr bs >> loop res
    Right result -> putStrLn $ "Run result: " ++ show result

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
import Devel.Compile (compile)
import Devel.ReverseProxy (runServer, createSocket)

import Control.Concurrent (forkIO, killThread, ThreadId)
import Control.Concurrent.STM.TVar
import Network.Socket

-- For use within `loop`
import IdeSession
import qualified Data.ByteString.Char8 as S8

import Control.Concurrent (threadDelay)

-- | Compiles and runs your WAI application.
build :: Bool -> SessionConfig -> IO ()
build reverseProxy' config = do

  -- Either an ideBackend session or a list of errors from `build`.
  eitherSession <- compile config 

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
      restart reverseProxy' config

    Right session -> do
      --  Run the WAI application in a separate thread.
      (runActionsRunResult, threadId) <- run session sock reverseProxy'

      case reverseProxy' of
        False -> putStrLn "Project built. Preparing to start devel server without reverse proxy"

        -- Start the reverse proxy server
        True -> do putStrLn "Project built. Preparing to start devel server"

                   -- For watching for file changes in current working directory.
                   isDirty <- newTVarIO False

                   -- Watch for changes in the current working directory.
                   _ <- forkIO $ watch isDirty
                   
                   -- Block until change is made then carry on with program execution.
                   checkForChange isDirty

                   stopApp runActionsRunResult threadId sock

                   restart reverseProxy' config


-- | Invoked when we are ready to run the compiled code.
run :: IdeSession -> Socket -> Bool -> IO (RunActions RunResult, ThreadId)
run session sock reverseProxy' = do

  -- Run the given ide-backend session.
  runActionsRunResult <- runStmt session "Application" "develMain"

  threadId  <- forkIO $ loop runActionsRunResult
  
  _ <- threadDelay 1000

  _ <- forkIO $ runServer [] sock
  
  return (runActionsRunResult, threadId)

-- | Restart the whole process.
-- Like calling main in Main but first notifies that
-- it's about to restart.
restart :: Bool -> SessionConfig -> IO ()
restart reverseProxy' config = do
  putStrLn "\n\nRestarting...\n\n"
  build reverseProxy' config

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

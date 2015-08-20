{-|
Module      : Devel.Compile
Description : For building and running your WAI application.
Copyright   : (c)
License     : GPL-3
Maintainer  : njagi@urbanslug.com
Stability   : experimental
Portability : POSIX
-}

{-# LANGUAGE RecordWildCards #-}

module Devel.Build (build) where

import IdeSession hiding (getEnv)
-- import IdeSession.Query hiding (getEnv)
-- import IdeSession.State
import qualified Data.ByteString.Char8 as S8

import Control.Concurrent (forkIO, killThread, ThreadId)
import Control.Concurrent.STM.TVar
import Network.Socket

-- import System.Environment (lookupEnv)
import Control.Concurrent (threadDelay)
import Data.Text (unpack)
-- import System.Process (runCommand)

import Devel.Paths
-- import Devel.Modules
import Devel.Watch
import Devel.Compile (compile)
import Devel.ReverseProxy


-- | Compiles and runs your WAI application.
build :: FilePath -> String ->  Bool -> SessionConfig -> (Int, Int) -> IO ()
build buildFile runFunction reverseProxy' config (srcPort, destPort) = do

  -- Either an ideBackend session or a list of errors from `build`.
  eitherSession <- compile buildFile config

  sock <- createSocket srcPort

  case eitherSession of
    Left  errorList -> do
      -- Start the warp server if the TVar is True.
      _ <- forkIO $ runServer errorList sock srcPort
      putStrLn $ "Errors at http://localhost:"++(show srcPort)

      -- Listen for changes in the current working directory.
      isDirty <- newTVarIO False
      _ <- forkIO $ watchErrored isDirty
      checkForChange isDirty

      -- close the socket so that we may create another in the new build.
      close sock

      -- Restart the whole process.
      restart buildFile runFunction reverseProxy' config (srcPort, destPort)

    Right session -> do

      --  Run the WAI application in a separate thread.
      (runActionsRunResult, threadId) <-
        run buildFile runFunction session sock destPort reverseProxy'

      -- For watching for file changes in current working directory.
      isDirty <- newTVarIO False
      
      -- List of paths to watch
      pathsToWatch <- getFilesToWatch session

      -- Watch for changes in the current working directory.
      _ <- forkIO $ watch isDirty pathsToWatch

      -- Block until change is made then carry on with program execution.
      checkForChange isDirty
      stopApp runActionsRunResult threadId sock
      restart buildFile runFunction reverseProxy' config (srcPort, destPort)

-- | Invoked when we are ready to run the compiled code.
run :: FilePath -> String ->  IdeSession -> Socket -> Int -> Bool -> IO (RunActions RunResult, ThreadId)
run buildFile runFunction session sock destPort reverseProxy' = do
  mapFunction <- getFileMap session
  buildModule <- case mapFunction buildFile of
                   Nothing -> fail "The file's module name couldn't be found"
                   Just moduleId -> return $ unpack $ moduleName moduleId

  -- Run the given ide-backend session.
  runActionsRunResult <- runStmt session buildModule runFunction

  threadId  <- forkIO $ loop runActionsRunResult
  
  _ <- threadDelay 1000
  
  case reverseProxy' of
      False -> putStrLn "Project built. Preparing to start devel server without reverse proxy"
      True -> do putStrLn $ "Project built. Preparing to start devel server..."
                 _ <- forkIO $ runServer [] sock destPort
                 return ()

  return (runActionsRunResult, threadId)

-- | Restart the whole process.
-- Like calling main in Main but first notifies that
-- it's about to restart.
restart :: FilePath -> String ->  Bool -> SessionConfig -> (Int, Int) -> IO ()
restart buildFile runFunction reverseProxy' config portPair = do
  putStrLn "\nRestarting...\n"
  build buildFile runFunction reverseProxy' config portPair

-- | Stop the currently running WAI application.
stopApp :: RunActions RunResult -> ThreadId -> Socket -> IO ()
stopApp runResult threadId sock = do
  interrupt runResult
  killThread threadId

-- | Run for as long as we need to.
loop :: RunActions RunResult -> IO ()
loop res = do
  runAction <- runWait res
  case runAction of
    Left bs -> S8.putStr bs >> loop res
    Right result -> putStrLn $ "Run result: " ++ show result

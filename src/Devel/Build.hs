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
import IdeSession.Query hiding (getEnv)
import IdeSession.State
import qualified Data.ByteString.Char8 as S8

import Control.Concurrent (forkIO, killThread, ThreadId)
import Control.Concurrent.STM.TVar
import Network.Socket

import System.Environment (lookupEnv)
import Control.Concurrent (threadDelay)
import Data.Text (unpack)
import System.Process (runCommand)

import Devel.Paths
import Devel.Modules
import Devel.Watch
import Devel.Compile (compile)
import Devel.ReverseProxy


-- | Compiles and runs your WAI application.
build :: FilePath -> String ->  Bool -> SessionConfig -> IO ()
build buildFile runFunction reverseProxy' config = do

  -- Either an ideBackend session or a list of errors from `build`.
  eitherSession <- compile buildFile config

  case eitherSession of
    Left  errorList -> do
    
      mPort <- lookupEnv "PORT"

      let port = case mPort of
                   Just p -> (read p :: Int)
                   _ -> 3000

      sock <- createSocket port

      -- Start the warp server if the TVar is True.
      _ <- forkIO $ runServer errorList sock
      putStrLn $ "Errors at http://localhost:"++(show port)

      -- Listen for changes in the current working directory.
      isDirty <- newTVarIO False
      _ <- forkIO $ watchErrored isDirty
      checkForChange isDirty

      -- close the socket so that we may create another in the new build.
      close sock

      -- Restart the whole process.
      restart buildFile runFunction reverseProxy' config

    Right session -> do
      
      --  Run the WAI application in a separate thread.
      (runActionsRunResult, threadId) <-
        run buildFile runFunction session reverseProxy'

      -- For watching for file changes in current working directory.
      isDirty <- newTVarIO False
      
      -- List of paths to watch
      pathsToWatch <- getFilesToWatch session

      -- Watch for changes in the current working directory.
      _ <- forkIO $ watch isDirty pathsToWatch

      -- Block until change is made then carry on with program execution.
      checkForChange isDirty
      stopApp runActionsRunResult threadId
      restart buildFile runFunction reverseProxy' config
      
      -- Block until change is made then carry on with program execution.
      checkForChange isDirty

      stopApp runActionsRunResult threadId

      restart buildFile runFunction reverseProxy' config

-- | Invoked when we are ready to run the compiled code.
run :: FilePath -> String ->  IdeSession -> Bool -> IO (RunActions RunResult, ThreadId)
run buildFile runFunction session reverseProxy' = do
  mapFunction <- getFileMap session
  buildModule <- case mapFunction buildFile of
                   Nothing -> fail "The file's module name couldn't be found"
                   Just moduleId -> return $ unpack $ moduleName moduleId

  -- Run the given ide-backend session.
  runActionsRunResult <- runStmt session buildModule runFunction

  threadId  <- forkIO $ loop runActionsRunResult
  
  _ <- threadDelay 1000
  

  return (runActionsRunResult, threadId)

-- | Restart the whole process.
-- Like calling main in Main but first notifies that
-- it's about to restart.
restart :: FilePath -> String ->  Bool -> SessionConfig -> IO ()
restart buildFile runFunction reverseProxy' config = do
  putStrLn "\nRestarting...\n"
  build buildFile runFunction reverseProxy' config

-- | Stop the currently running WAI application.
stopApp :: RunActions RunResult -> ThreadId -> IO ()
stopApp runResult threadId = do
  interrupt runResult
  killThread threadId

-- | Run for as long as we need to.
loop :: RunActions RunResult -> IO ()
loop res = do
  runAction <- runWait res
  case runAction of
    Left bs -> S8.putStr bs >> loop res
    Right result -> putStrLn $ "Run result: " ++ show result

{-|
Module      : Devel.Compile
Description : For building and running your WAI application.
Copyright   : (c)
License     : GPL-3
Maintainer  : njagi@urbanslug.com
Stability   : experimental
Portability : POSIX
-}

-- {-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Devel.Build 
( build
) where


import IdeSession
import qualified Data.ByteString.Char8 as S8
import Data.Text (unpack)

-- import Network.Socket (close, Socket)
import GHC.Conc (newTVarIO)
import Control.Concurrent (forkIO, killThread, ThreadId)

import Devel.Paths
import Devel.Compile
import Devel.ReverseProxy (startReverseProxy)
import Devel.Types
import Devel.Watch



-- | Compiles and calls run on your WAI application.
build :: FilePath -> String ->  Bool -> SessionConfig -> (Int, Int) -> Maybe IdeSession -> FileChange -> IO ()
build buildFile runFunction isReverseProxy sessionConfig (fromProxyPort, toProxyPort) mSession fileChange = do

  (initialSession, extensionList) <- initCompile sessionConfig mSession

  _ <- case fileChange of
         NoChange -> 
          if isReverseProxy then
            do _ <- forkIO $ startReverseProxy (fromProxyPort, toProxyPort)
               putStrLn $ "Starting devel application at http://localhost:" ++
                          show fromProxyPort
          else
            putStrLn $ "Starting app without reverse proxying at http://localhost:" ++
                       show fromProxyPort
         _  -> return ()

  (updatedSession, update) <- 
     case fileChange of
       NoChange -> compile initialSession extensionList buildFile
       _        -> return (initialSession, mempty) -- recompile initialSession

  eitherSession <- finishCompile (updatedSession, update)

  case eitherSession of
    Left _ -> do
      -- Listen for changes in the current working directory.
      isDirty <- newTVarIO False

      _ <- forkIO $ watchErrored isDirty

      -- Block until relevant change is made then carry on with program execution.
      _ <- checkForChangeErrored isDirty

      -- Stop the current app.
      putStrLn "\n\nRebuilding...\n\n"
      
      _ <- shutdownSession updatedSession

      build buildFile runFunction False sessionConfig (fromProxyPort, toProxyPort) Nothing NoChange

    Right session -> do
      -- run the session
      (runActionsRunResult, threadId) <- run session buildFile runFunction
      
      -- Start watching for file changes.
      isDirty <- newTVarIO (False, NoChange)

      -- List of paths to watch
      -- pathsToWatch <- getFilesToWatch session

      -- Watch for changes in the current working directory.
      watchId <- forkIO $ watch isDirty session

      -- Block until relevant change is made then carry on with program execution.
      newFileChange <- checkForChange isDirty
      
      killThread watchId
      
      -- Stop the current app.
      _ <- stopApp runActionsRunResult threadId
      putStrLn "\n\nRebuilding...\n\n"
      build buildFile runFunction isReverseProxy sessionConfig (fromProxyPort, toProxyPort) (Just session) newFileChange


run :: IdeSession -> FilePath -> String -> IO (RunActions RunResult, ThreadId)
run session buildFile runFunction = do
  -- Get the module name from the file path
  mapFunction <- getFileMap session
  buildModule <- case mapFunction buildFile of
                   Nothing -> fail $ "The file's module name for:"++ (show buildFile) ++"couldn't be found"
                   Just moduleId -> return $ unpack $ moduleName moduleId

  -- Run the given ide-backend session.
  runActionsRunResult <- runStmt session buildModule runFunction
  threadId <- forkIO $ loop runActionsRunResult

  return (runActionsRunResult, threadId)
  

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


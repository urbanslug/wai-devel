{-# LANGUAGE OverloadedStrings #-}
module Devel.Watch where

import Control.Monad.STM
import Control.Concurrent.STM.TVar
import Devel.Run (runBackend)
import Devel.ReverseProxy (runServer)

import System.FSNotify
import Control.Monad      (forever)
import Control.Concurrent (threadDelay, forkIO, killThread, ThreadId)

-- For use within `loop`
import IdeSession
import qualified Data.ByteString.Char8 as S8

import Data.Text (unpack)
import Control.Concurrent

compile :: IdeSession -> IO ()
compile session = do
  
  -- Run the updated session.
  runActionsRunResult <- runStmt session "Application" "main"
  
  threadId  <- forkIO $ loop runActionsRunResult
  isDirty   <- newTVarIO False
  
  _ <- forkIO $ watch isDirty
  checkForChange isDirty
  stopApp runActionsRunResult threadId
  
  bul <- newMVar True
  doCompile bul

doCompile :: MVar Bool -> IO ()
doCompile bul = do
  runBackend' <- runBackend
  case runBackend' of
    Left  errorList -> runServer bul $ toString' errorList
    Right session   -> do
      _ <- forkIO $ compile session
      putStrLn "Starting devel server http://localhost:3000"
      runServer bul []

toString' :: [SourceError] -> [String]
toString' [] = []
toString' (x: xs) = unpack (errorMsg x) : toString' xs

stopApp :: RunActions RunResult -> ThreadId -> IO ()
stopApp runResult threadId = do
  interrupt runResult
  killThread threadId
  return ()

-- | Watches for changes in the current working directory.
--   When a change is found. It modifies isDirty to True.
watch :: TVar Bool -> IO ()
watch isDirty = do
  withManager $ watch'
  return ()
  where watch' :: WatchManager -> IO StopListening
        watch' mgr = do watchTree mgr "." (const True) detectChange
                        forever $ threadDelay maxBound

        detectChange :: Event -> IO ()
        detectChange = \event -> case event of
                                   (Added    _ _) -> return ()
                                   (Modified _ _) -> atomically $ writeTVar isDirty True
                                   (Removed  _ _) -> return ()


checkForChange :: TVar Bool -> IO ()
checkForChange isDirty = do
  atomically $ do readTVar isDirty >>= check
                  writeTVar isDirty False

-- | Run for as long as we need to.
loop :: RunActions RunResult -> IO ()
loop res = do
  runAction <- runWait res
  case runAction of
    Left bs -> S8.putStr bs >> loop res
    Right result -> putStrLn $ "Run result: " ++ show result

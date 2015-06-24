{-# LANGUAGE OverloadedStrings #-}
module Devel.Watch where

import Control.Monad.STM
import Control.Concurrent.STM.TVar
import Devel.Run (runBackend)

import System.FSNotify
import Control.Monad      (forever)
import Control.Concurrent (threadDelay, forkIO, killThread)

-- For use within `loop`
import IdeSession
import qualified Data.ByteString.Char8 as S8

-- | The cannonical compile and recompile function.
compile :: IO ()
compile = do
  runResult <- runBackend
  threadId  <- forkIO $ loop runResult
  isDirty   <- newTVarIO False
  _ <- forkIO $ watch isDirty
  checkForChange isDirty
  interrupt runResult
  killThread threadId
  compile

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
  putStrLn "Starting devel server http://localhost:3000"
  runAction <- runWait res
  case runAction of
    Left bs -> S8.putStr bs >> loop res
    Right result -> putStrLn $ "Run result: " ++ show result

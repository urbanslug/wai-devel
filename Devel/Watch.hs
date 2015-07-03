{-|
Module      : Devel.Watch
Description : Watch for changes in the current working direcory.
Copyright   : (c)
License     : GPL-3
Maintainer  : njagi@urbanslug.com
Stability   : experimental
Portability : POSIX

Actually checks only for modified files.
Added or removed files don't trigger new builds.
-}
{-# LANGUAGE OverloadedStrings #-}
module Devel.Watch where

import Control.Monad.STM
import Control.Concurrent.STM.TVar
import Devel.Compile (compile)

import System.FSNotify
import Control.Monad      (forever)
import Control.Concurrent (threadDelay, forkIO, killThread, ThreadId)

-- For use within `loop`
import IdeSession
import qualified Data.ByteString.Char8 as S8


import Control.Concurrent
import System.Exit
import Control.Exception.Base

import Network.Socket

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

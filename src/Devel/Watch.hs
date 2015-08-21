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
{-# LANGUAGE OverloadedStrings, CPP #-}
module Devel.Watch where

import Control.Monad.STM
import Control.Concurrent.STM.TVar

import System.FSNotify
import System.FSNotify.Devel
import Control.Monad      (forever)
import Control.Concurrent (threadDelay)

# if __GLASGOW_HASKELL__ < 710
import qualified Filesystem.Path as FS
import System.FilePath.Posix (takeFileName)
import System.FSNotify.Devel
import Data.Text (unpack, pack)
import Filesystem.Path.CurrentOS (toText)
#endif

-- | Runs in the current working directory 
-- Watches for file changes for the specified file extenstions
-- When a change is found. It modifies isDirty to True.
watch :: TVar Bool -> [FilePath] -> IO ()
watch isDirty pathsToWatch = do
  manager <- startManagerConf defaultConfig
  _ <- watchTree
         manager 
         "." 
         (const True)

# if __GLASGOW_HASKELL__ >= 710
         (\event -> do
            let pathMod :: Event -> FilePath
                pathMod (Added path _) = path
                pathMod (Modified path _) = path
                pathMod (Removed path _) = path
            isModified <- return (any (== pathMod event) pathsToWatch)
            atomically $ writeTVar isDirty isModified)    
         
#else
         (\event -> do 
            pathMod <- case toText $ eventPath event of
                           Right text -> return $ unpack text -- Gives an abs path
                           Left text -> fail $ unpack text
            isModified <- return (any (== pathMod) pathsToWatch)
            atomically $ writeTVar isDirty isModified)
#endif
  _ <- forever $ threadDelay maxBound
  stopManager manager

-- | Runs in the current working directory 
-- Watches for file changes for the specified file extenstions
-- When a change is found. It modifies isDirty to True.
watchErrored :: TVar Bool -> IO ()
watchErrored isDirty = do
  manager <- startManagerConf defaultConfig

  _ <- treeExtAny manager "." "hamlet"  (\_ -> atomically $ writeTVar isDirty True)
  _ <- treeExtAny manager "." "shamlet" (\_ -> atomically $ writeTVar isDirty True)
  _ <- treeExtAny manager "." "lucius"  (\_ -> atomically $ writeTVar isDirty True)
  _ <- treeExtAny manager "." "julius"  (\_ -> atomically $ writeTVar isDirty True)
  _ <- treeExtAny manager "." "hs"      (\_ -> atomically $ writeTVar isDirty True)
  _ <- treeExtAny manager "." "lhs"      (\_ -> atomically $ writeTVar isDirty True)
  _ <- treeExtAny manager "." "yaml"    (\_ -> atomically $ writeTVar isDirty True)

  _ <- forever $ threadDelay maxBound
  stopManager manager


checkForChange :: TVar Bool -> IO ()
checkForChange isDirty = do
  atomically $ do readTVar isDirty >>= check
                  writeTVar isDirty False

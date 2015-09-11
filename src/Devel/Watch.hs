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

import Devel.Types

# if __GLASGOW_HASKELL__ < 710
import Data.Text (unpack)
import Filesystem.Path.CurrentOS (toText)
import qualified Filesystem.Path as FSP
#endif

-- "Smart" file watching.
watch :: TVar (Bool, FileChange) -> [FilePath] -> IO ()
watch isDirty pathsToWatch = do
  manager <- startManagerConf defaultConfig
  _ <- watchTree manager "." (const True)

-- Last argument to watchTree.
# if __GLASGOW_HASKELL__ >= 710
         (\event -> do
            let pathMod :: Event -> FileChange
                pathMod (Added path _)    = Addition path
                pathMod (Modified path _) = Modification path
                pathMod (Removed path _)  = Removal path

                getFilePath :: FileChange -> FilePath
                getFilePath (Addition path) = path
                getFilePath (Modification path) = path
                getFilePath (Removal path) = path
                getFilePath NoChange = error "Event gegenrated NoChange while file had changed."

                fileChange = pathMod event
                file = getFilePath fileChange

            isModified <- return (any (== file) pathsToWatch)
            atomically $ writeTVar isDirty (isModified, fileChange))    

#else
         (\event -> do 
            let pathMod :: Event -> FileChange
                pathMod (Added path _)    = Addition     (getPath path)
                pathMod (Modified path _) = Modification (getPath path)
                pathMod (Removed path _)  = Removal      (getPath path)

                getPath :: FSP.FilePath -> FilePath
                getPath p = case toText p of
                              Right text -> unpack text
                              Left text -> fail $ unpack text

                fileChange = pathMod event

            pathMod' <- case toText $ eventPath event of
                            Right text -> return $ unpack text -- Gives an abs path
                            Left text -> fail $ unpack text
            
            isModified <- return (any (== pathMod') pathsToWatch)
            atomically $ writeTVar isDirty (isModified, fileChange))
#endif
  _ <- forever $ threadDelay maxBound
  stopManager manager

-- | Runs in the current working directory. Watches when there's an error.
-- Watches for file changes for the specified file extenstions
-- When a change is found. It modifies isDirty to True.
watchErrored :: TVar Bool -> IO ()
watchErrored isDirty = do
  manager <- startManagerConf 
                  defaultConfig {confUsePolling= True}

  _ <- treeExtAny manager "." "hamlet"  (\_ -> atomically $ writeTVar isDirty True)
  _ <- treeExtAny manager "." "shamlet" (\_ -> atomically $ writeTVar isDirty True)
  _ <- treeExtAny manager "." "lucius"  (\_ -> atomically $ writeTVar isDirty True)
  _ <- treeExtAny manager "." "julius"  (\_ -> atomically $ writeTVar isDirty True)
  _ <- treeExtAny manager "." "hs"      (\_ -> atomically $ writeTVar isDirty True)
  _ <- treeExtAny manager "." "lhs"      (\_ -> atomically $ writeTVar isDirty True)
  _ <- treeExtAny manager "." "yaml"    (\_ -> atomically $ writeTVar isDirty True)

  _ <- forever $ threadDelay maxBound
  stopManager manager


checkForChange :: TVar (Bool, FileChange) -> IO FileChange
checkForChange isDirty = do
  atomically $ readTVar isDirty >>= \(isDirty', _) -> check isDirty'
  (_, fileChange) <- readTVarIO isDirty
  atomically $ writeTVar isDirty (False, NoChange)
  return fileChange

checkForChangeErrored :: TVar Bool -> IO ()
checkForChangeErrored isDirty = do
  atomically $ do readTVar isDirty >>= check
                  writeTVar isDirty False

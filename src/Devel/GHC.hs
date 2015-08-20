{-|
Module      : Devel.Modules
Description : For handling modules and Filepath matters.
Copyright   : (c)
License     : GPL-3
Maintainer  : njagi@urbanslug.com
Stability   : experimental
Portability : POSIX

Uses the GHC package to parse .hi files.
Will hopefully be moved upstream to ide-backend.
-}

{-# LANGUAGE OverloadedStrings #-}

module Devel.GHC where

import qualified Data.ByteString.Char8 as C8
import           Control.Monad.IO.Class (liftIO)
-- import Control.Monad (mapM)

printDeps :: IO ()
printDeps = do
  str <- getDependentFiles "Handler/Home.dump-hi"
  _ <- mapM putStrLn str
  return ()

-- Get back a list of dependent files that a module depends on.
-- Takes a path to the .dump-hi file.
getDependentFiles :: FilePath -> IO [FilePath]
getDependentFiles dumpHiFp = do
  dumpHI <- liftIO $ fmap C8.lines (C8.readFile dumpHiFp)
  let thDeps' = filter ("addDependentFile \"" `C8.isPrefixOf`) dumpHI
      thDeps = map (C8.dropWhile (/='\"')) thDeps'
  return $ map C8.unpack thDeps

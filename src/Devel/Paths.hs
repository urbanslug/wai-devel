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

module Devel.Paths where

import System.Directory (getCurrentDirectory, doesDirectoryExist, getDirectoryContents)
import Control.Monad (forM)
import System.FilePath.Glob
import System.FilePath ((</>))
import Data.List
import Devel.Modules
import System.FilePath.Posix (replaceExtension, dropExtension)
import IdeSession
import qualified Data.ByteString.Char8 as C8
import Control.Monad.IO.Class
import System.FilePath (pathSeparator)


getFilesToWatch :: IdeSession -> IO [FilePath]
getFilesToWatch session = do
  dir <- getCurrentDirectory
  srcPaths <- getPathsForCompiledFiles session
  let replaceWithHiDump :: FilePath -> FilePath
      replaceWithHiDump srcFile =  replaceExtension  srcFile ".dump-hi"
      withHiDumpExt = map replaceWithHiDump srcPaths
  thDeps' <- mapM parseHi withHiDumpExt
  -- Removing quotes.
  mixedThDeps  <- return $ map (takeWhile (/='\"') . dropWhile (=='\"') . dropWhile (/='\"')) $ concat thDeps'
  -- make rel paths absolute and leave the absolute ones intact
  -- mixedThDeps meaning there are both absolute and relative paths here.
  let makePathsAbsolute :: FilePath -> FilePath
      makePathsAbsolute fp@(x:xs)
        | x == pathSeparator = fp
        | otherwise = dir ++ [pathSeparator] ++ fp
      thDeps = map makePathsAbsolute mixedThDeps
  
  -- Add the cabal file path to paths to watch for.
  cabalFile <- getCabalFile

  return $ cabalFile : srcPaths ++ thDeps 

getCabalFile :: IO FilePath
getCabalFile = do
  list <- glob "*cabal"
  case list of
    [] -> fail "No cabal file."
    (cabalFile:_) -> return cabalFile

parseHi :: FilePath -> IO [FilePath]
parseHi path = do
  dumpHI <- liftIO $ fmap C8.lines (C8.readFile path)
  let thDeps' =
          -- The dependent file path is surrounded by quotes but is not escaped.
          -- It can be an absolute or relative path.
          filter ("addDependentFile \"" `C8.isPrefixOf`) dumpHI
  return $ map C8.unpack thDeps'

-- | Match the possible compiled files agaist the actual source files.
-- Gives you the actual source files.
getPathsForCompiledFiles :: IdeSession -> IO [FilePath]
getPathsForCompiledFiles session = do
  compiledNoExt <- getCompiledFiles session
  allSourceFiles <- getManagedFiles' -- With extensions
  return $ matchCompiledFiles compiledNoExt allSourceFiles
  where matchCompiledFiles :: [FilePath] -> [FilePath] -> [FilePath]
        matchCompiledFiles _ [] = []
        matchCompiledFiles [] _ = []
        matchCompiledFiles list@(x:xs) (y:ys)
          | x == (dropExtension y) = y : matchCompiledFiles xs ys
          | otherwise = matchCompiledFiles list (ys ++ [y])

-- | These are the source and data files.
-- Not related or to be confused with ide-backend's
-- getManagedFiles
getManagedFiles' :: IO [FilePath]
getManagedFiles' =  do
  dir <- getCurrentDirectory
  getRecursiveContents dir

-- | Get the files that actually exist in the given dir.
-- In this case it's called with the source dirs
getRecursiveContents :: FilePath -> IO [FilePath]
getRecursiveContents topdir = do
  names <- getDirectoryContents topdir
  
  -- We want to take these files out.
  let patterns = [ (compile "*.*~")
                 , (compile "*.hi")
                 , (compile "*.dump-hi")
                 , (compile "*.o")
                 , (compile "*.dyn_o")
                 , (compile "*.dyn_hi")
                 , (compile "*.so")
                 , (compile "*.conf")
                 , (compile "*.h")
                 , (compile "*.a")
                 , (compile "*.inplace")
                 , (compile "*.cache")
                 , (compile "*.*.el")
                 ]
  (x, _) <- globDir patterns topdir

  let properNames = filter (`notElem` [".", ".."]) names
  paths <- forM properNames $ \name -> do
   let path = topdir </> name
   isDirectory <- doesDirectoryExist path
   if isDirectory
     then getRecursiveContents path
     else return $ [path] \\ (concat x)
  return (concat paths)

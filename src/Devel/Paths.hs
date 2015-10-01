{-|
Module      : Devel.Paths
Description : For filepath related matters.
Copyright   : (c)
License     : GPL-3
Maintainer  : njagi@urbanslug.com
Stability   : experimental
Portability : POSIX

Uses the GHC package to parse .hi files.
Will hopefully be moved upstream to ide-backend.
-}

{-# LANGUAGE OverloadedStrings #-}

module Devel.Paths 
( getFilesToWatch
, getCabalFile
, getRecursiveContents
, getRecursiveContentsRelative
) where

-- local imports
import Devel.Modules (getCompiledFiles)

import IdeSession
import System.FilePath.Glob

import Data.List ((\\))
import System.FilePath ((</>))
import System.Directory (getCurrentDirectory, doesDirectoryExist, getDirectoryContents)
import Control.Monad (forM)
import Control.Concurrent (forkIO, killThread, threadDelay)
import System.FilePath.Posix (replaceExtension, dropExtension, takeExtensions)
import Control.Monad.IO.Class (liftIO)
import System.FilePath (pathSeparator)
import System.Directory (removeFile)

import qualified Data.ByteString.Char8 as C8


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
      makePathsAbsolute [] = dir ++ [pathSeparator]
      makePathsAbsolute fp@(x:_)
        | x == pathSeparator = fp
        | otherwise = dir ++ [pathSeparator] ++ fp
      thDeps = map makePathsAbsolute mixedThDeps
  
  -- Add the cabal file path to paths to watch for.
  cabalFile <- getCabalFile
  -- Clean up after GHC
  -- delitterId <- forkIO $ delitter
  -- threadDelay 1000
  -- killThread delitterId
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
  cwd <- getCurrentDirectory
  getRecursiveContents cwd

-- | Get the files that actually exist in the given dir.
-- In this case it's called with the source dirs
getRecursiveContents :: FilePath -> IO [FilePath]
getRecursiveContents topdir = do
  names <- getDirectoryContents topdir
  
  -- We want to take these files out.
  let patterns :: [Pattern]
      patterns = [ (compile "*.*~")
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
                 , (compile ".*")
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

getRecursiveContentsRelative :: FilePath -> IO [FilePath]
getRecursiveContentsRelative topdir = do
  names <- getDirectoryContents topdir

  -- We want to take these files out.
  let patterns :: [Pattern]
      patterns = [ (compile "*.*~")
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
                 , (compile ".*")
                 ]
  (x, _) <- globDir patterns topdir

  let properNames = filter (`notElem` [".", ".."]) names
  paths <- forM properNames $ \name -> do
   let path = (makePathRelative topdir) </> name
   isDirectory <- doesDirectoryExist path
   if isDirectory
     then getRecursiveContents path
     else return $ [path] \\ (concat x)
  return (concat paths)
  where makePathRelative :: FilePath -> FilePath
        makePathRelative topDir 
          | topDir == "." = ""
          | otherwise = topDir
          
-- Clean up after ghc -ddump-hi -ddump-to-file
delitter :: IO ()
delitter = do
  cwd <- getCurrentDirectory
  litter <- getLitter cwd
  mapM_ del litter

  where getLitter :: FilePath -> IO [FilePath]
        getLitter topdir = do
          names <- getDirectoryContents topdir

          -- We want to leave these files out of the list
          let patterns :: [Pattern]
              patterns = [ (compile "*.*~")
                         , (compile "*.hs")
                         , (compile "*.txt")
                         , (compile "*.o")
                         , (compile "*.img")
                         , (compile "*.ico")
                         , (compile "*.so")
                         , (compile "*.conf")
                         , (compile "*.h")
                         , (compile "*.a")
                         , (compile "*.lhs")
                         , (compile "*.inplace")
                         , (compile "*.cache")
                         , (compile "*.*.el")
                         , (compile ".*")
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

        del :: FilePath -> IO ()
        del fp = do
          case   ((takeExtensions fp) == ".dump-hi" )
              || ((takeExtensions fp) == ".dyn_o" )
              || ((takeExtensions fp) == ".o" )
              || ((takeExtensions fp) == ".dyn_hi" )
              || ((takeExtensions fp) == ".hi" ) of True  -> removeFile fp
                                                    False -> return ()
          

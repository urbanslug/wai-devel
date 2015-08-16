{-|
Module      : Devel.Modules
Description : For handling modules and Filepath matters.
Copyright   : (c)
License     : GPL-3
Maintainer  : njagi@urbanslug.com
Stability   : experimental
Portability : POSIX
            
Recusively finding all the modules that have been added to the list of targets.
Finding all the data files that these modules depend on.
-}

{-# LANGUAGE PackageImports #-}

module Devel.Modules where

import Distribution.PackageDescription
import Distribution.PackageDescription.Parse
import "Glob" System.FilePath.Glob (glob)
import Distribution.PackageDescription.Configuration
--import Distribution.ModuleName (toFilePath)
import IdeSession
import System.FilePath (pathSeparator)
import Data.List (intersperse)
import Data.Text (unpack)

import Control.Monad (forM)
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>))
import System.FilePath.Posix (dropExtension)

-- | Match the possible compiled files agaist the actual source files.
-- Gives you the actual source files.
getPathsForCompiledFiles :: IdeSession -> IO [FilePath]
getPathsForCompiledFiles session = do
  compiledNoExt <- getCompiledFiles session
  allSourceFiles <- getAllSourceFiles -- With extensions
  return $ matchCompiledFiles compiledNoExt allSourceFiles
  where matchCompiledFiles :: [FilePath] -> [FilePath] -> [FilePath]
        matchCompiledFiles _ [] = []
        matchCompiledFiles [] _ = []
        matchCompiledFiles list@(x:xs) (y:ys)
          | x == (dropExtension y) = y : matchCompiledFiles xs ys
          | otherwise = matchCompiledFiles list (ys ++ [y])

-- |The filepaths to modules we're compiling. Without the extensions
-- i.e. The modules from the package we are compiling
-- That we are compiling.
getCompiledFiles :: IdeSession -> IO [FilePath]
getCompiledFiles session = do
  let getLocalLoadedModules = getLoadedModules

  moduleList <- getLocalLoadedModules session

  let toFilePath :: ModuleName -> FilePath
      toFilePath moduleName' = intersperse pathSeparator $ unpack moduleName'

      relativePaths = map toFilePath moduleList

  sourceDirs <- getSourceDirs
  return $ zipWith (++) sourceDirs relativePaths

-- | Get all the source files in the hs-source-dirs.
getAllSourceFiles :: IO [FilePath]
getAllSourceFiles = do
  sourceDirs <- getSourceDirs
  pathList <- mapM getRecursiveContents sourceDirs
  return $ concat pathList

-- | Get the files that actually exist in the given dir.
-- In this case it's called with the source dirs
getRecursiveContents :: FilePath -> IO [FilePath]
getRecursiveContents topdir = do
  names <- getDirectoryContents topdir
  let properNames = filter (`notElem` [".", ".."]) names
  paths <- forM properNames $ \name -> do
    let path = topdir </> name
    isDirectory <- doesDirectoryExist path
    if isDirectory
      then getRecursiveContents path
      else return [path]
  return (concat paths)

-- | This is actually a flattened package description.
-- Flattening let's us get information from the library section
-- of a cabal file.
-- http://stackoverflow.com/questions/22785670/retrieving-names-of-modules-included-in-a-cabal-package-from-parsing-the-cabal
getPackageDecription :: IO PackageDescription
getPackageDecription = do
   list <- glob "*cabal"

   cabalFilePath <- case list of
                        [] -> fail "cabal file not found."
                        (x:_) -> return x

   cabalFile <- readFile cabalFilePath

   (genericPackageDescription, _) <- 
     case parsePackageDescription cabalFile of
       ParseOk warnList genPackDesc -> return (genPackDesc, warnList)
       ParseFailed parseError -> fail $ "Error parsing cabal file: " ++ show parseError

   let packageDescription' :: PackageDescription
       packageDescription' = flattenPackageDescription genericPackageDescription

   return packageDescription'


-- | The list of directories that hold the source files.
getSourceDirs :: IO [FilePath]
getSourceDirs = do
  packageDescription' <- getPackageDecription
  return $ concatMap hsSourceDirs $ allBuildInfo packageDescription'

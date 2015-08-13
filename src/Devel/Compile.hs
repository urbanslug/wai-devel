{-|
Module      : Devel.Build
Description : Attempts to compile the WAI application.
Copyright   : (c)
License     : GPL-3
Maintainer  : njagi@urbanslug.com
Stability   : experimental
Portability : POSIX

compile compiles the app to give:
Either a list of source errors or an ide-backend session.
-}

{-# LANGUAGE PackageImports #-}

module Devel.Compile (compile) where

import IdeSession

-- Used internally for showing errors.
import Data.Text (unpack)

-- From Cabal-ide-backend
-- for parsing the cabal file and extracting lang extensions used.
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse
import Distribution.PackageDescription.Configuration
import Devel.Types

import Control.Monad (forM)
import System.Directory (doesDirectoryExist, doesFileExist, getDirectoryContents)
import System.FilePath ((</>))
import Data.Monoid ((<>))
import System.FilePath.Glob (glob)


compile :: SessionConfig -> IO (Either [SourceError'] IdeSession)
compile config = do

  session <- initSession
             defaultSessionInitParams
             config

  extensionList <- extractExtensions

  testDir <- doesDirectoryExist "test"

  targetFiles' <- case testDir of
                   True  -> getRecursiveContents "test"
                   False -> return []

  isDevelMain <- doesFileExist "app/DevelMain.hs"

  targetFiles <- case isDevelMain of
                    True -> return $ targetFiles' ++ ["app/DevelMain.hs"]
                    _    -> return $ targetFiles'


  -- Description of session updates.
  let targetList = (TargetsExclude targetFiles :: Targets)
      update = updateTargets targetList
               <> updateCodeGeneration True
               <> updateGhcOpts (["-Wall"] ++ extensionList)

  -- Actually update the session.
  updateSession session update print

  -- Custom error showing.
  errorList' <- getSourceErrors session

  errorList  <- case filterErrors errorList' of
                    [] -> return []
                    _  -> return $ prettyPrint errorList'

  --  We still want to see errors and warnings on the terminal.
  mapM_ putStrLn $ prettyPrint errorList'

  return $ case errorList of
             [] -> Right session  
             _  -> Left  errorList


-- | Remove the warnings from [SourceError] if any.
-- Return an empty list if there are no errors and only warnings
-- Return non empty list if there are errors.
filterErrors :: [SourceError] -> [SourceError]
filterErrors [] = []
filterErrors (x:xs) = case errorKind x  of
             KindWarning -> filterErrors xs
             _ -> x : filterErrors xs


prettyPrint :: [SourceError] -> [SourceError']
prettyPrint [] = []
prettyPrint (x: xs) = 
  case errorKind x  of
    KindWarning -> ("Warning: " ++ (show (errorSpan x)) ++ " " ++ (unpack (errorMsg x))) : prettyPrint xs
    KindError   -> ("Error: " ++ (show (errorSpan x)) ++ " " ++ (unpack (errorMsg x)))  : prettyPrint xs
    KindServerDied -> (show (errorKind x)) : prettyPrint xs


-- | Parse the cabal file to extract the cabal extensions in use.
extractExtensions :: IO [String]
extractExtensions = do
              list <- glob "*cabal"
              cabalFilePath <- case list of
                                 [] -> fail "No cabal file."
                                 (x:_) -> return x
              cabalFile <- readFile cabalFilePath

              let unsafePackageDescription = parsePackageDescription cabalFile

                  genericPackageDescription = case unsafePackageDescription of
                                            ParseOk _ a -> a
                                            _           -> error "failed package description."

                  packDescription = flattenPackageDescription genericPackageDescription
                  sanitize = last . words

              allExt <- return $ usedExtensions $ head $ allBuildInfo packDescription
              listOfExtensions <- return $ map sanitize $ map show allExt
              return $ map ((++) "-X") listOfExtensions


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

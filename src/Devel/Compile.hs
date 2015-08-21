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

{-# LANGUAGE PackageImports, OverloadedStrings #-}

module Devel.Compile (compile) where

-- From Cabal-ide-backend
-- for parsing the cabal file and extracting lang extensions used.
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse
import Distribution.PackageDescription.Configuration
import Language.Haskell.Extension

-- Used internally for showing errors.
import Data.Text (unpack)

import Data.Monoid ((<>))
-- import System.FilePath.Glob (glob)
import System.Directory (createDirectoryIfMissing, getCurrentDirectory)
-- import Control.Monad (join)

import IdeSession
import Devel.Paths
import Devel.Types

compile :: FilePath -> SessionConfig -> IO (Either [SourceError'] IdeSession)
compile buildFile config = do

  session <- initSession
             defaultSessionInitParams
             config

  extensionList <- extractExtensions
  
  -- For .dump-hi files
  dir <- getCurrentDirectory
  let dumpDir = (dir ++ "/.dist/dump-hi/")
  _ <- createDirectoryIfMissing True dumpDir


  -- Description of session updates.
  let targetList = (TargetsInclude [buildFile] :: Targets)
      update = updateTargets targetList
               <> updateCodeGeneration True
               <> updateGhcOpts (["-ddump-hi", "-ddump-to-file", ("-dumpdir="++dumpDir)] ++ ["-Wall"] ++ extensionList)

  -- Actually update the session.
  updateSession session update print

  -- Custom error showing.
  errorList' <- getSourceErrors session

  let errorList = case filterErrors errorList' of
                    [] -> []
                    _  -> prettyPrintErrors errorList'


  --  We still want to see errors and warnings on the terminal.
  mapM_ putStrLn $ prettyPrintErrors errorList'

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

prettyPrintErrors :: [SourceError] -> [SourceError']
prettyPrintErrors [] = []
prettyPrintErrors (x: xs) = 
  case errorKind x  of
    KindWarning -> ("Warning: " ++ (show (errorSpan x)) ++ " " ++ (unpack (errorMsg x))) : prettyPrintErrors xs
    KindError   -> ("Error: " ++ (show (errorSpan x)) ++ " " ++ (unpack (errorMsg x)))  : prettyPrintErrors xs
    KindServerDied -> (show (errorKind x)) : prettyPrintErrors xs

-- | Parse the cabal file to extract the cabal extensions in use.
extractExtensions :: IO [String]
extractExtensions = do               
              cabalFilePath <- getCabalFile
              cabalFile <- readFile cabalFilePath

              let unsafePackageDescription = parsePackageDescription cabalFile

                  genericPackageDescription = case unsafePackageDescription of
                                            ParseOk _ a -> a
                                            _           -> error "failed package description."

                  packDescription = flattenPackageDescription genericPackageDescription

              rawExt <- return $ usedExtensions $ head $ allBuildInfo packDescription
              let parseExtension :: Extension -> String
                  parseExtension (EnableExtension extension) =  "-X" ++ (show extension)
                  parseExtension (DisableExtension extension) = "-XNo" ++ (show extension)
                  parseExtension (UnknownExtension extension) = "-X" ++ (show extension)

                  extensions = map parseExtension rawExt
              return extensions

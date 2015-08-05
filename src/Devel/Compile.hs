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
import Data.Monoid ((<>))

-- import "Glob" System.FilePath.Glob (glob)
-- System.FilePath.Glob from package "Glob" 
-- Weirdly conflicts with System.FilePath.Glob from "filemanip"
import System.FilePath.Glob (glob)

-- Used internally for showing errors.
import Data.Text (unpack)

-- From Cabal-ide-backend
-- for parsing the cabal file and extracting lang extensions used.
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse
import Distribution.PackageDescription.Configuration
import Devel.Types

compile :: IO (Either [SourceError'] IdeSession)
compile = do

  -- Initializing the session.
  config <- sessionConfigFromEnv
  session <- initSession
             defaultSessionInitParams
             config

  extensionList <- extractExtensions

  -- Description of session updates.
  let update = updateCodeGeneration True
               <> updateGhcOpts (["-Wall"] ++ extensionList)

  -- Actually update the session.
  updateSession session update print

  -- Custom error showing.
  errorList' <- getSourceErrors session

  errorList <- case filterErrors errorList' of
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

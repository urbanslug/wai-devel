{-# LANGUAGE PackageImports #-}

module Devel.Run (runBackend) where

import IdeSession
import Data.Monoid ((<>))

-- import "Glob" System.FilePath.Glob (glob)
-- System.FilePath.Glob from package "Glob" Weirdly conflicts with System.FilePath.Glob from "filemanip"
import System.FilePath.Glob (glob)
import System.Directory (getCurrentDirectory)

-- Used internally for showing errors.
import Data.Text (unpack)

-- From Cabal-ide-backend
-- for parsing the cabal file and extracting lang extensions used.
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse
import Distribution.PackageDescription.Configuration

import System.Process (rawSystem)
import System.Directory (doesFileExist)
import System.Exit (ExitCode(ExitSuccess))

runBackend :: IO (Either [SourceError] IdeSession)
runBackend = do

             dir <- getCurrentDirectory

             -- Check if configured. If not, configure.
             isConf <- doesFileExist $ dir ++ "/dist/setup-config"
             case isConf of
               True  -> return ExitSuccess 
               False -> rawSystem "cabal" configFlags

             -- Initializing the session.
             session <- initSession
                        defaultSessionInitParams
                        defaultSessionConfig
                        {configLocalWorkingDir = Just dir}

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
                            _ -> return errorList'
                            
             printErrors errorList'

             return $ case errorList of
                        [] -> Right session  
                        _  -> Left errorList

filterErrors :: [SourceError] -> [SourceError]
filterErrors [] = []
filterErrors (x:xs) = case errorKind x  of
             KindWarning -> filterErrors xs
             _ -> x : filterErrors xs

-- Pretty print errors.
printErrors :: [SourceError] -> IO ()
printErrors [] = return ()
printErrors (x: xs) = putStrLn (unpack (errorMsg x))  >> printErrors xs

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


configFlags :: [String]
configFlags = [ "configure"
              , "-flibrary-only"
              , "--disable-tests"
              , "--disable-benchmarks"
              , "-fdevel"
              , "--disable-library-profiling"
              , "--with-ld=yesod-ld-wrapper"
              , "--with-ghc=yesod-ghc-wrapper"
              , "--with-ar=yesod-ar-wrapper"
              , "--with-hc-pkg=ghc-pkg"]

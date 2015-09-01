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

{-# LANGUAGE PackageImports, OverloadedStrings, TemplateHaskell #-}

module Devel.Compile (compile) where

-- Almost everything is dependent on ide-backend.
import IdeSession

-- From Cabal-ide-backend
-- for parsing the cabal file and extracting lang extensions used.
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse
import Distribution.PackageDescription.Configuration
import Language.Haskell.Extension

-- Used internally for showing errors.
import Data.Text (unpack)

-- Utility functions
import Data.Monoid ((<>))
import System.Directory (createDirectoryIfMissing)

-- Local imports
import Devel.Paths
import Devel.Types

-- reverse proxying
import Network.HTTP.ReverseProxy
import Network.Wai (Application, responseBuilder, responseLBS)
import Network.HTTP.ReverseProxy (WaiProxyResponse(WPRProxyDest, WPRResponse), ProxyDest(ProxyDest), waiProxyTo)
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Network.HTTP.Types (status200)
import Text.Hamlet (shamletFile)
import Text.Blaze.Html.Renderer.Utf8 (renderHtmlBuilder)

import Network.Wai.Handler.Warp
import Control.Exception

import Data.Text (Text, pack)
import Network.Wai (responseBuilder)
import Text.Blaze.Html.Renderer.Utf8 (renderHtmlBuilder)
import Yesod.Core.Handler

compile :: FilePath -> SessionConfig -> IO (Either [SourceError'] IdeSession)
compile buildFile config = do

  session <- initSession
             defaultSessionInitParams
             config

  extensionList <- extractExtensions
  let dumpDir = ".dist/dump-hi"
  _ <- createDirectoryIfMissing True dumpDir


  -- Description of session updates.
  let targetList = (TargetsInclude [buildFile] :: Targets)
      update = updateTargets targetList
               <> updateCodeGeneration True
               <> updateGhcOpts (["-ddump-hi", "-ddump-to-file", ("-dumpdir "++dumpDir)] ++ ["-Wall"] ++ extensionList)

  -- Actually update the session.
  updateSession session update streamUpdateResult

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
              
streamUpdateResult :: UpdateStatus -> IO ()
streamUpdateResult _ = do
  app <- (respond $ responseLBS status200 [] "Hello World")
  run 4000 app
{-
streamUpdateResult :: UpdateStatus -> IO ()
streamUpdateResult (UpdateStatusFailed text) = fail $ unpack text
streamUpdateResult UpdateStatusRequiredRestart = fail "Requires restart"
streamUpdateResult (UpdateStatusCrashRestart text) = fail $ "Restart. Crashed with: " ++ unpack text
streamUpdateResult (UpdateStatusServerDied text) = fail $ "Server died: "++ unpack text
streamUpdateResult UpdateStatusDone = print "Update done."
streamUpdateResult (UpdateStatusProgress progress) = streamProgress progress
  where streamProgress :: Progress -> IO ()
        streamProgress progress = do 
          _ <- print $ progressStep progress
          _ <- print $ progressNumSteps progress
          _ <- print $ unpack $ getMsgFromMaybe $ progressOrigMsg progress
          toBrowser $ unpack $ progressText $ progressParsedMsg progress

        progressText :: Maybe Text -> Text
        progressText Nothing = "No progress text"
        progressText (Just text) = text
        getMsgFromMaybe :: Maybe Text -> Text
        getMsgFromMaybe Nothing = "Original Message: Nothing."
        getMsgFromMaybe (Just msg) = pack $ "Original Message: " ++ unpack msg

toBrowser :: String -> IO ()
toBrowser progress = do
  mgr <- newManager defaultManagerSettings
  let error500 :: SomeException -> Application
      error500 _ _ respond = respond $
        responseBuilder
        status503
        [("content-type", "text/html; charset=utf-8")]
        (renderHtmlBuilder $(shamletFile "build.hamlet"))
  app <- return $ waiProxyTo
                   (const $ return $ WPRProxyDest $ ProxyDest "127.0.0.1" 4001)
                   error500
                   mgr
  run 4000 app 

-}

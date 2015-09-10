{-|
Module      : Devel.Compile
Description : Attempts to compile the WAI application.
Copyright   : (c)
License     : GPL-3
Maintainer  : njagi@urbanslug.com
Stability   : experimental
Portability : POSIX

Compile compiles the app to give:
Either a list of source errors or an ide-backend session.
-}

{-# LANGUAGE PackageImports, OverloadedStrings, TemplateHaskell #-}

module Devel.Compile 
( initCompile
, compile
, recompile
, finishCompile
) where

-- Almost everything is dependent on ide-backend.
import IdeSession

-- From Cabal-ide-backend
-- for parsing the cabal file and extracting lang extensions used.
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse
import Distribution.PackageDescription.Configuration
import Language.Haskell.Extension

-- Used internally for showing errors.
import Data.Text (unpack, Text)

-- Utility functions
import Data.Monoid ((<>))

-- Local imports
import Devel.Paths
import Devel.Types
import Devel.ReverseProxy (checkPort)
import Devel.WebSocket

-- web sockets
-- import GHC.Conc (forkIO)
import qualified Network.WebSockets as WS
import qualified Data.ByteString.Char8 as BS
import Network.Socket (close, Socket)
import Data.IORef

import qualified Network.WebSockets as WS
import Network.Socket      (withSocketsDo, accept, socket)
import Data.IORef
import IdeSession (UpdateStatus)

-- recompile
import System.FilePath.Posix (takeExtension)

-------------------------
import Network.Wai (Application, responseBuilder, responseLBS)
import Network.HTTP.ReverseProxy (WaiProxyResponse(WPRProxyDest), ProxyDest(ProxyDest), waiProxyTo)
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Network.HTTP.Types (status200)
import Text.Hamlet (shamletFile)
import Text.Blaze.Html.Renderer.Utf8 (renderHtmlBuilder)

import Network.Wai.Handler.Warp
import Control.Exception

import qualified Network.WebSockets as WS
import qualified Network.Socket as Sock
import Data.Streaming.Network
import GHC.Conc
------------------------


-- Initialize the compilation process.
initCompile :: SessionConfig -> IO (IdeSession, [GhcExtension])
initCompile sessionConfig = do

  -- Initialize the session
  session <- initSession
               defaultSessionInitParams
               sessionConfig

  extensionList <- getExtensions

  return (session, extensionList)

compile :: IdeSession -> [GhcExtension] -> FilePath -> IO (IdeSession, IdeSessionUpdate)
compile session extensionList buildFile = do

  -- Description of session updates.
  let targetList = (TargetsInclude [buildFile] :: Targets)
      update = updateTargets targetList
               <> updateCodeGeneration True
               <> updateGhcOpts (["-ddump-hi", "-ddump-to-file"] ++ ["-Wall"] ++ extensionList)

  return (session, update)


recompile :: IdeSession -> FileChange -> IO (IdeSession, IdeSessionUpdate)
recompile session fileChange = do

  let update = case fileChange of
                 Addition fileForUpdate -> 
                   case (takeExtension fileForUpdate == ".lhs") || (takeExtension fileForUpdate == ".hs") of
                     True  -> updateSourceFileFromFile fileForUpdate
                     False -> updateDataFileFromFile fileForUpdate fileForUpdate
                 Modification fileForUpdate -> 
                   case (takeExtension fileForUpdate == ".lhs") || (takeExtension fileForUpdate == ".hs") of
                     True  -> updateSourceFileFromFile fileForUpdate
                     False -> updateDataFileFromFile fileForUpdate fileForUpdate
                 Removal fileForUpdate -> 
                   case (takeExtension fileForUpdate == ".lhs") || (takeExtension fileForUpdate == ".hs") of
                     True  -> updateSourceFileDelete fileForUpdate
                     False -> updateDataFileDelete fileForUpdate
                 NoChange -> error "Couldn't rebuild application. Failed update. Please report as a bug."
                 
  return (session, update)

finishCompile :: (IdeSession, IdeSessionUpdate) -> IO (Either [SourceError'] IdeSession)
finishCompile (session, update) = do

  _ <- updateSession session update print

  -- Customizing error showing.
  errorList' <- getSourceErrors session
  let errorList = case filterErrors errorList' of
                    [] -> []
                    _  -> prettyPrintErrors errorList'

  -- We still want to see errors and warnings on the terminal.
  mapM_ putStrLn $ prettyPrintErrors errorList'

  return $ case errorList of
    [] -> Right session
    _  -> Left  errorList

----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

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

-- | Parse the cabal file to get the ghc extensions in use.
getExtensions :: IO [GhcExtension]
getExtensions = do               
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

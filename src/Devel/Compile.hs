{-|
Module      : Devel.Compile
Description : Attempts to compile the WAI application.
Copyright   : (c) 2015 Njagi Mwaniki
License     : MIT
Maintainer  : njagi@urbanslug.com
Stability   : experimental
Portability : POSIX

Compile compiles the app to give:
Either a list of source errors or an ide-backend session.
-}

{-# LANGUAGE OverloadedStrings #-}

module Devel.Compile 
( initCompile
, compile
, finishCompile
) where

-- import Devel.WebSockets (runWsServer)

-- The backbone library of ide-backend.
-- Almost everything is dependent on ide-backend.
import IdeSession

-- From Cabal-ide-backend
-- for parsing the cabal file and extracting lang extensions used.
import Distribution.PackageDescription
import Distribution.ModuleName
import Distribution.PackageDescription.Parse
import Distribution.PackageDescription.Configuration
import Language.Haskell.Extension

-- Used internally for showing errors.
import Data.Text (unpack, pack)

-- Utility functions
import Data.Monoid ((<>))

-- Local imports
import Devel.Paths
import Devel.Types

import System.FilePath.Posix (takeExtension, pathSeparator)
import System.Directory (doesFileExist)
import Data.List (union, delete, isInfixOf, nub)
import Data.Maybe (fromMaybe)
import Control.Monad (filterM)

-- WS stuff
import qualified Network.WebSockets as WS
import Control.Concurrent -- (forkIO, MVar)
import qualified Data.Text as T

import Network.Socket
import Data.Streaming.Network (bindPortTCP)
import qualified Network.Wai as Wai
import Network.HTTP.Types (status200)
import qualified Network.Wai.Handler.WebSockets as WW
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Socket as Socket

import Devel.ReverseProxy (checkPort)

-- |Initialize the compilation process.
initCompile :: [String] -> SessionConfig -> Maybe IdeSession -> IO (IdeSession, [GhcExtension], [FilePath], [FilePath])
initCompile watchDirectories sessionConfig mSession = do
  -- Initialize the session
  session <- case mSession of
               Just session -> return session
               Nothing -> initSession
                            defaultSessionInitParams
                            sessionConfig

  -- This is "rebuilding" the cabal file.
  (extensionList, srcDir, cabalSrcList) <- getExtensions
  sourceList <- getSourceList srcDir cabalSrcList
  additionalWatchList <- fmap concat (mapM getRecursiveContents watchDirectories)
  return (session, extensionList, sourceList, additionalWatchList)

getSourceList :: [FilePath] -> [FilePath] -> IO [FilePath]
getSourceList srcDir cabalSrcList = do

  fileList' <- mapM getRecursiveContents srcDir

  let -- Remove duplicate values. 
      fileList = foldr union [] fileList'
      -- Remove files in test dir.

      fileListNoTests = filter (not.(\x -> isInfixOf "test/" x || isInfixOf "cabal-sandbox/" x || isInfixOf "stack-work/" x)) fileList

      -- Add both lists together.
      fileListCombined = fileListNoTests ++ cabalSrcList

      -- Remove all non hs and non .lhs files.
      -- also remove "app/devel.hs" because it has an extra main function
      sourceList' = filter 
                      (\f -> let ext = takeExtension f in ext == ".lhs" || ext == ".hs") 
                      fileListCombined
      -- nub to remove duplicates yet again.
      sourceList = nub $ delete "app/DevelMain.hs" $ delete "app/devel.hs" sourceList'


  return sourceList


-- | Contains code regarding target files and coming up with the IdeSession update.
compile :: IdeSession -> FilePath -> [GhcExtension] -> [FilePath] -> IO (IdeSession, IdeSessionUpdate)
compile session buildFile extensionList sourceList = do

  -- Description of session updates.
  let targetList = TargetsInclude (if buildFile `elem` sourceList
                                      then sourceList
                                      else buildFile : sourceList) :: Targets
      update = updateTargets targetList
               <> updateCodeGeneration True
               <> updateGhcOpts (["-Wall", "-ddump-hi", "-ddump-to-file"] ++ extensionList)

  return (session, update)


-- |The final part of the compilation process.
finishCompile :: (IdeSession, IdeSessionUpdate) -> IO (Either [SourceError'] IdeSession)
finishCompile (session, update) = do

  mUpd <- newEmptyMVar :: IO (MVar String)

  let printToBrowser :: UpdateStatus -> IO ()
      printToBrowser upd' = let upd = show upd'
                            in putMVar mUpd upd

      handleConnection :: WS.PendingConnection -> IO () -- :: ServerApp
      handleConnection pending = do
        conn <- WS.acceptRequest pending
        loop conn
        where loop :: WS.Connection -> IO () 
              loop conn' = do
                upd <- takeMVar mUpd
                WS.sendTextData conn' $ T.pack upd
                putStrLn upd
                loop conn'
  
      backupApp :: Wai.Application
      backupApp _ respond = respond $ Wai.responseLBS status200 [] "Please make a WebSocket request."

      app :: Wai.Application 
      app = WW.websocketsOr WS.defaultConnectionOptions handleConnection backupApp

  notBound <- checkPort 5002
  
  if notBound
     then do
       sock <- createSocket 5002
       tId <- forkIO $ Warp.runSettingsSocket Warp.defaultSettings sock app 
       _ <- updateSession session update printToBrowser
       killThread tId
       Socket.close sock
     else 
       updateSession session update print

  -- tId <- forkIO $ WS.runServer "127.0.0.1" 5002 handleConnection

  
  
  -- Customizing error showing.
  errorList' <- getSourceErrors session
  let errorList = case filterErrors errorList' of
                    [] -> []
                    _  -> prettyPrintErrors errorList'

  -- We still want to see errors and warnings on the terminal.
  mapM_ (putMVar mUpd) (prettyPrintErrors errorList')
  mapM_ putStrLn $ prettyPrintErrors errorList'

  case prettyPrintErrors errorList' of
    [] -> return ()
    _ -> shutdownSession session

  return $ case errorList of
    [] -> Right session
    _  -> Left  errorList


-- -----------------------------------------------------------
--   Utility functions.
-- -----------------------------------------------------------

createSocket :: Int -> IO Socket
createSocket port = do
  sock <- bindPortTCP port "*4"

  -- Tell the OS *not* to reserve the socket after your program exits.
  setSocketOption sock ReuseAddr 1

  return sock

-- updateSession :: IdeSession -> IdeSessionUpdate -> (Progress -> IO ()) -> IO ()
-- updateSession https://hackage.haskell.org/package/ide-backend-0.9.0.11/docs/IdeSession.html#v:updateSession
-- Progress https://hackage.haskell.org/package/ide-backend-0.9.0.11/docs/IdeSession.html#t:Progress
{-
printToBrowser :: UpdateStatus -> IO ()
printToBrowser update = do
  print update
  runWsServer $ pack $ show update
-}

-- | Parse the cabal file to get the ghc extensions in use.
getExtensions :: IO ([GhcExtension], [FilePath], [FilePath])
getExtensions = do               
  cabalFilePath <- getCabalFile
  cabalFile <- readFile cabalFilePath

  let unsafePackageDescription = parsePackageDescription cabalFile

      genericPackageDescription = case unsafePackageDescription of
                                ParseOk _ a -> a
                                _           -> error "failed package description."

      packDescription = flattenPackageDescription genericPackageDescription

      rawExt = usedExtensions $ head $ allBuildInfo packDescription

      lib = fromMaybe emptyLibrary $ library packDescription

      -- I think it would be wise to avoid src files under executable to avoid conflict.
      srcDir  = hsSourceDirs $ libBuildInfo lib
      srcList = extraSrcFiles packDescription


      parseExtension :: Extension -> String
      parseExtension (EnableExtension extension) =  "-X" ++ show extension
      parseExtension (DisableExtension extension) = "-XNo" ++ show extension
      parseExtension (UnknownExtension extension) = "-X" ++ show extension

      extensions = map parseExtension rawExt

      -- Now we handle source files from executable section here.
      execList = executables packDescription
  
  paths <- mapM getPathList execList 

  return (extensions, srcDir, (srcList ++ (concat paths)))
  where 
    getPathList :: Executable -> IO [FilePath]
    getPathList exec =
      let mainModule' = modulePath exec
          bInfo = buildInfo exec
          execModuleList = otherModules bInfo
          srcDirsList = hsSourceDirs bInfo
          execSrcFileList = map toFilePath execModuleList
          nonPaths = [dir ++ (pathSeparator : fp) | fp <- execSrcFileList, dir <- srcDirsList]
          paths' = map (++ (pathSeparator : mainModule') ) srcDirsList -- For the main module
                   ++ [x++y | x <- nonPaths, y <- [".hs", ".lhs"]]

      in filterM doesFileExist paths'



-- | Remove the warnings from [SourceError] if any.
-- Return an empty list if there are no errors and only warnings
-- Return non empty list if there are errors.
filterErrors :: [SourceError] -> [SourceError]
filterErrors [] = []
filterErrors (x:xs) = 
  case errorKind x  of
    KindWarning -> filterErrors xs
    _ -> x : filterErrors xs

prettyPrintErrors :: [SourceError] -> [SourceError']
prettyPrintErrors [] = []
prettyPrintErrors (x: xs) = 
  case errorKind x  of
    KindWarning -> ("Warning: " ++ show (errorSpan x) ++ " " ++ unpack (errorMsg x)) : prettyPrintErrors xs
    KindError   -> ("Error: " ++ show (errorSpan x) ++ " " ++ unpack (errorMsg x))  : prettyPrintErrors xs
    KindServerDied -> show (errorKind x) : prettyPrintErrors xs

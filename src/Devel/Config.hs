{-|
Module      : Devel.Config
Description : To have wai-devel depend on it's environment a lot less.
Copyright   : (c)
License     : GPL-3
Maintainer  : njagi@urbanslug.com
Stability   : experimental
Portability : POSIX

Currently we'll query for this information from the stack binary.

Ideal case it to use the stack library to figure out depends and stuff.
Depending on the stack library causes a breakage in the dependecy tree.
As of now I can't find a single function (or set of functions) that fetches this information from stack.
Closest thing is in the stack Main module.
        
Will be rewritten to depend on the stack library.
-}


module Devel.Config (setConfig) where

import System.Process (readProcessWithExitCode)
import System.Environment (unsetEnv, setEnv)
import System.Exit (ExitCode(..))

setConfig :: IO ()
setConfig = do
  (path, pkgDb) <- getConfig
  _ <- unsetEnv "PATH"
  _ <- unsetEnv "GHC_PACKAGE_PATH"
  _ <- setEnv "PATH" path
  setEnv "GHC_PACKAGE_PATH" pkgDb


type PATH = String
type PACKAGEDB = String
type Config = (PATH, PACKAGEDB)

getConfig :: IO Config
getConfig = do
  (exitCode, stdout, stderr) <- readProcessWithExitCode "stack" ["path"] ""
  case exitCode of
    ExitSuccess   -> parseConfig stdout
    ExitFailure _ -> fail stderr
  where 
    parseConfig :: String -> IO Config
    parseConfig stdout = do
      let outputList = lines stdout
          tupleList = map (span (/=':') ) outputList
          path = concatMap getPath tupleList
          pkgDb = concatMap getPkgDb tupleList ++ ":"
      return (path, pkgDb)


getPath :: (String, String) -> String
getPath (key,value)
        | key == "bin-path" = dropWhile (==':') $ filter (/=' ') value
        | otherwise = ""

getPkgDb :: (String, String) -> String
getPkgDb (key,value)
          | key == "snapshot-pkg-db" = dropWhile (==':') $ filter (/=' ') value
          | otherwise = ""

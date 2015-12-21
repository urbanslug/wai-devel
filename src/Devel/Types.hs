{-|
Module      : Devel.Build
Description : Attempts to compile the WAI application.
Copyright   : (c) 2015 Njagi Mwaniki
License     : MIT
Maintainer  : njagi@urbanslug.com
Stability   : experimental
Portability : POSIX

compile compiles the app to give:
Either a list of source errors or an ide-backend session.
-}

module Devel.Types where

type SourceError' = String

type GhcExtension = String

data FileChange = 
    Addition     FilePath
  | Modification FilePath
  | Removal      FilePath
  | NoChange
  deriving (Show, Eq)

type PATH = String

type PACKAGEDB = String

type Config = (PATH, PACKAGEDB)

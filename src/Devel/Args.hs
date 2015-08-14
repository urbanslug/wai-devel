{-|
Module      : Devel.Args
Description : For handling command line arguments.
Copyright   : (c)
License     : GPL-3
Maintainer  : njagi@urbanslug.com
Stability   : experimental
Portability : POSIX

No additional info.
-}
module Devel.Args where

import Options.Applicative

-- | Command line arguments for yesod devel.
-- All arguments are optional.
data CmdArgs = CmdArgs
  { interfaceFile :: Maybe FilePath
  , buildFile :: Maybe FilePath
  , runFunction :: Maybe String
  -- By deafult reverse proxy should be True
  , reverseProxy :: Bool 
  }

cmdArgs :: Parser CmdArgs
cmdArgs = CmdArgs
        <$> optional (strOption
             (long "file"
               <> short 'f'
               <> metavar "FILEPATH"
               <> help "The path file with the function you want to run"))
        <*> optional (strOption
             (long "module"
               <> short 'm'
               <> metavar "MODULE"
               <> help "The module you want run"))
        <*> optional (strOption
             (long "show-iface"
               <> metavar "FILE"
               <> help "same as `ghc --show-iface`" ))
        <*> flag True False
              (long "reverse-proxy"
                <> short 'r'
                <> help "use `-r` to disable reverse proxying." )

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
  , reverseProxy :: Bool
  }

cmdArgs :: Parser CmdArgs
cmdArgs = CmdArgs
        <$> optional (strOption
             (long "show-iface"
               <> metavar "FILE"
               <> help "same as `ghc --show-iface`" ))
        <*> flag True False
              (long "reverse-proxy"
                <> short 'r'
                <> help "use `-r` to disable reverse proxying." )

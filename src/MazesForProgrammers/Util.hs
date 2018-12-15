{-# LANGUAGE CPP #-}

module MazesForProgrammers.Util
    ( shellOpen
    ) where

import           System.Posix.Escape
import           System.Process
import           Text.Printf

shellOpen :: FilePath -> IO ()
shellOpen p =
#if defined(darwin_HOST_OS)
    let cmd = printf "open %s" (escape p)
#else
    let cmd = printf "xdg-open %s" (escape p)
#endif
    in callCommand cmd

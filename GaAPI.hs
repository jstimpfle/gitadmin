{-# LANGUAGE GADTs #-}

module GaAPI where

import Control.Applicative ((<$>), (<*>))
import System.IO (stdin, stdout, stderr)
import System.Posix.IO (handleToFd)
import System.Posix.Terminal (queryTerminal)

import Sshkey
import Types
import GaErr
import GaCtx
import GaCmd
import GaAction
import GaIO

mk_ctx :: Username -> IO GaCtx
mk_ctx username = GaCtx "/home/jfs" undefined
                   stdin stdout stderr
                   (Userid 0) (Username "root")
                   <$> (queryTerminal =<< handleToFd stdin)
                   <*> (queryTerminal =<< handleToFd stdout)
                   <*> (queryTerminal =<< handleToFd stderr)

run_cmd :: Username -> GaCmd () -> IO ()
run_cmd username cmd =
 do ctx <- mk_ctx username
    run_gaio ctx $ compileGaAction $ compile cmd

{-# LANGUAGE GADTs #-}

module GaAction where

import Control.Applicative (Applicative, pure, (<*>))
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import System.IO (getLine, hPutStrLn)

import DbAction
import GaIO
import GaCtx
import GaTransaction

data GaAction a where
    GaSeq :: GaAction a -> (a -> GaAction b) -> GaAction b
    GaPure :: a -> GaAction a
    ReadLineStdin :: GaAction String
    ToStderrIfTTY :: String -> GaAction ()
    LogStderr :: String -> GaAction ()
    PrintStdout :: String -> GaAction ()
    RunTransaction :: GaTransaction a -> GaAction a

instance Functor GaAction where
    fmap f ga = GaSeq ga (return . f)

instance Applicative GaAction where
    pure = GaPure
    mf <*> ma = do f <- mf; a <- ma; return (f a)

instance Monad GaAction where
    (>>=) = GaSeq
    return = GaPure

compileGaAction :: GaAction a -> GaIO a
compileGaAction = go where
    go :: GaAction a -> GaIO a
    go (GaSeq m f)        = go m >>= (go . f)
    go (GaPure a)         = return a
    go (ReadLineStdin)    = liftIO $ getLine
    go (ToStderrIfTTY s)  = do x <- get_ctx
                               when (is_stderr_tty x)
                                 (liftIO $ hPutStrLn (stderr_handle x) s)
    go (LogStderr s)      = do x <- get_ctx
                               liftIO $ hPutStrLn (stderr_handle x) s
    go (PrintStdout s)    = do x <- get_ctx
                               liftIO $ hPutStrLn (stdout_handle x) s
    go (RunTransaction a) = compileDbAction $ compileGaTransaction a

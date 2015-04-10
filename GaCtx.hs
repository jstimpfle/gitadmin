module GaCtx
  ( GaCtx (..)
  , Connection, Handle, FilePath, Userid, Username
  )
where

import Types
import System.IO (Handle)
import Database.HDBC.Sqlite3 (Connection)

data GaCtx
  = GaCtx
  { baseDir :: FilePath
  , dbHandle :: Connection
  , stdin_handle :: Handle
  , stdout_handle :: Handle
  , stderr_handle :: Handle
  , uid :: Userid
  , logname :: Username
  , is_stdin_tty :: Bool
  , is_stdout_tty :: Bool
  , is_stderr_tty :: Bool
  }

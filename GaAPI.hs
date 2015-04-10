module GaAPI where

import Control.Applicative ((<$>), (<*>))
import System.IO (stdin, stdout, stderr)
import System.Posix.Terminal (queryTerminal)

import Types
import GaCmd
import GaCtx
import GaIO
import GaAction
import Db

run_cmd :: Username -> GaCmd () -> IO ()
run_cmd username cmd =
 do conn <- db_connect "gitadmin-basedir/db.sqlite3"
    ctx <- GaCtx "gitadmin-basedir" conn stdin stdout stderr
                    (Userid 0) (Username "root")
                    <$> queryTerminal 0 <*> queryTerminal 1 <*> queryTerminal 2
    run_gaio ctx $ compileGaAction $ compile cmd
    db_disconnect conn

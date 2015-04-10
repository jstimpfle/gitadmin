{-# LANGUAGE GADTs #-}

module Initialize
  ( initialize
  )
where

import Control.Applicative (Applicative, pure, (<$>), (<*>))
import Control.Monad (when)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Reader (ReaderT, runReaderT, ask)
import Database.HDBC
import Database.HDBC.Sqlite3
import System.Directory (getDirectoryContents, doesDirectoryExist, doesFileExist)
import System.FilePath.Posix (joinPath)

data InitCtx = InitCtx { baseDir :: FilePath }

data InitAction a where
    Pure :: a -> InitAction a
    Seq  :: InitAction a -> (a -> InitAction b) -> InitAction b
    GetCtx :: InitAction InitCtx
    NeedEmptyDir :: FilePath -> InitAction ()
    MakeDb :: String -> FilePath -> InitAction ()

instance Functor InitAction where
    fmap f = (>>= Pure . f)

instance Applicative InitAction where
    pure = Pure
    af <*> ax = af >>= flip fmap ax

instance Monad InitAction where
    return = Pure
    (>>=) = Seq

initaction :: InitAction ()
initaction =
 do bd <- baseDir <$> GetCtx
    NeedEmptyDir $ joinPath [bd, "repos"]
    NeedEmptyDir $ joinPath [bd, "publicro"]
    MakeDb dbschema $ joinPath [bd, "gitadmin.sqlite3"]

dbschema :: String
dbschema = "CREATE TABLE t (id integer primary key, mycol integer not null)"

compile :: InitAction a -> ReaderT InitCtx IO a
compile (Pure a) = return a
compile (Seq ma f) = compile ma >>= compile . f
compile GetCtx = ask
compile (NeedEmptyDir path) = lift $
 do doesDirectoryExist path
        >>= (flip (when . not) $ error $ path ++ " doesn't exist")
    getDirectoryContents path
        >>= (return . filter (not . dotlinks))
        >>= (flip (when . not . null) $ error $ path ++ " is not empty.")
    where dotlinks x = x `elem` [".", ".."]
compile (MakeDb schema path) = lift $
 do doesFileExist path
        >>= (flip when $ error $ path ++ " exists; won't attempt to create database")
    conn <- connectSqlite3 path
    run conn "PRAGMA FOREIGN_KEYS = ON" []
    run conn schema []
    run conn "COMMIT" []
    return ()

initialize :: IO ()
initialize = runReaderT (compile initaction) (InitCtx "gitadmin-basedir")

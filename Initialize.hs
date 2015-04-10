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
import System.Exit (ExitCode(..))
import System.FilePath.Posix (joinPath)
import System.IO.Error (catchIOError)

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
    MakeDb dbschema $ joinPath [bd, "db.sqlite3"]

compile :: InitAction a -> ReaderT InitCtx IO a
compile (Pure a) = return a
compile (Seq ma f) = compile ma >>= compile . f
compile GetCtx = ask
compile (NeedEmptyDir path) = lift $
 do doesDirectoryExist path
        >>= (flip (when . not) $ error $ path ++ " doesn't exist")
    getDirectoryContents path
        >>= (return . filter (not . dots))
        >>= (flip (when . not . null) $ error $ path ++ " is not empty.")
    where dots x = x `elem` [".", ".."]
compile (MakeDb schema path) = lift $
 do doesFileExist path
        >>= (flip when $ error $ path ++ " exists; won't attempt to create database")
    conn <- connectSqlite3 path
    run conn "PRAGMA FOREIGN_KEYS = ON" []
    runRaw conn schema
    run conn "COMMIT" []
    return ()

initialize :: IO ExitCode
initialize =
 do runReaderT (compile initaction) (InitCtx "gitadmin-basedir")
    return ExitSuccess
 `catchIOError` (const $ return $ ExitFailure 1)

dbschema :: String
dbschema = "\
\CREATE TABLE domain\n\
\( id INTEGER PRIMARY KEY\n\
\, name VARCHAR NOT NULL\n\
\, comment VARCHAR NOT NULL\n\
\--\n\
\, UNIQUE (name)\n\
\);\n\
\\n\
\CREATE TABLE user\n\
\( id INTEGER PRIMARY KEY\n\
\, name VARCHAR NOT NULL\n\
\, comment VARCHAR NOT NULL\n\
\--\n\
\, UNIQUE (name)\n\
\);\n\
\\n\
\CREATE TABLE repo\n\
\( id INTEGER PRIMARY KEY\n\
\, domainid INTEGER NOT NULL\n\
\, name VARCHAR NOT NULL\n\
\, comment VARCHAR NOT NULL\n\
\--\n\
\, FOREIGN KEY (domainid) REFERENCES domain(id)\n\
\, UNIQUE (domainid, name)\n\
\);\n\
\\n\
\CREATE TABLE permissions\n\
\( repoid INTEGER NOT NULL\n\
\, userid INTEGER NOT NULL\n\
\, mode INTEGER NOT NULL  -- 0 or 1 (R or RW)\n\
\--\n\
\, FOREIGN KEY (repoid) REFERENCES repo(id)\n\
\, FOREIGN KEY (userid) REFERENCES user(id)\n\
\, UNIQUE (repoid, userid)\n\
\);\n\
\\n\
\CREATE TABLE admin\n\
\( domainid INTEGER NOT NULL\n\
\, userid INTEGER NOT NULL\n\
\--\n\
\, FOREIGN KEY (domainid) REFERENCES domain(id)\n\
\, FOREIGN KEY (userid) REFERENCES user(id)\n\
\, UNIQUE (domainid, userid)\n\
\);\n\
\\n\
\CREATE TABLE sshkey\n\
\( id INTEGER PRIMARY KEY\n\
\, userid INTEGER NOT NULL\n\
\, algo VARCHAR NOT NULL\n\
\, key VARCHAR NOT NULL\n\
\, comment VARCHAR NOT NULL\n\
\--\n\
\, FOREIGN KEY (userid) REFERENCES user(id)\n\
\, UNIQUE (key)\n\
\);\n\
\\n\
\CREATE TABLE publicro\n\
\( repoid INTEGER NOT NULL UNIQUE\n\
\--\n\
\, FOREIGN KEY (repoid) REFERENCES repo(id)\n\
\);\n\
\\n\
\INSERT INTO user (id, name, comment) VALUES (0, \"root\", \"\");\n\
\"

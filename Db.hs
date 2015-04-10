{-# LANGUAGE LambdaCase #-}

module Db
  ( db_connect, db_disconnect
  , db_add_sshkey
  , db_remove_sshkey
  , db_ls_sshkeys
  , db_ls_users
  , db_ls_repos
  , db_create_domain
  , db_rename_domain
  , db_delete_domain
  , db_create_user
  , db_rename_user
  , db_delete_user
  , db_create_repo
  , db_rename_repo
  , db_delete_repo
  , db_grant_admin
  , db_revoke_admin
  , db_set_perm
  )
where

import Control.Applicative ((<$>))
import Control.Monad (when, mapM_)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Trans (liftIO)
import Database.HDBC (SqlValue(..), run, quickQuery', disconnect)
import Database.HDBC.Sqlite3 (Connection, connectSqlite3)

import GaErr
import GaCtx
import GaIO (GaIO(..), get_ctx)
import Types

-- =====================================================================
-- connect and disconnect. Maybe this should go somewhere else
-- =====================================================================

db_connect :: FilePath -> IO Connection
db_connect path =
 do conn <- connectSqlite3 path
    run conn "PRAGMA FOREIGN_KEYS = ON" []
    return conn

db_disconnect :: Connection -> IO ()
db_disconnect conn =
 do run conn "COMMIT" []
    disconnect conn

-- =====================================================================
-- Basic DB functionality
-- =====================================================================

type Stmt = String

db_get_conn :: GaIO Connection
db_get_conn = dbHandle <$> get_ctx

db_select :: Stmt -> [SqlValue] -> GaIO [[SqlValue]]
db_select a b = db_get_conn >>= \conn -> liftIO (quickQuery' conn a b)

db_select_one_or_none :: Stmt -> [SqlValue] -> GaIO (Maybe [SqlValue])
db_select_one_or_none a b =
 do r <- db_select a b
    case take 2 r of
        [] -> return Nothing
        x:[] -> return (Just x)
        _ -> throwError $ DbErr "Expected only a single return row"

db_run :: Stmt -> [SqlValue] -> GaIO Integer
db_run a b = db_get_conn >>= \conn -> liftIO (run conn a b)

db_insert :: Stmt -> [SqlValue] -> GaIO ()
db_insert a b = const () <$> db_run a b

db_one :: Integer -> GaIO ()
db_one = \case
    1 -> return ()
    n -> throwError $ DbErr $ "Expected exactly one changed row, but got: " ++ show n

db_one_or_none :: Integer -> GaIO Bool
db_one_or_none = \case
    0 -> return False
    1 -> return True
    n -> throwError $ DbErr $ "Expected 0 or 1 changed rows, but got: " ++ show n

db_insert_or_ignore :: Stmt -> [SqlValue] -> GaIO Bool
db_insert_or_ignore a b = db_run a b >>= db_one_or_none

db_insert_many :: Stmt -> [[SqlValue]] -> GaIO ()
db_insert_many a bs = mapM_ (db_insert a) bs

db_update_one_or_none :: Stmt -> [SqlValue] -> GaIO Bool
db_update_one_or_none a b = db_run a b >>= db_one_or_none

db_delete :: Stmt -> [SqlValue] -> GaIO Integer
db_delete a b = db_run a b

db_delete_one :: Stmt -> [SqlValue] -> GaIO ()
db_delete_one a b = db_run a b >>= db_one

db_delete_one_or_ignore :: Stmt -> [SqlValue] -> GaIO Bool
db_delete_one_or_ignore a b = db_run a b >>= db_one_or_none

-- =====================================================================
-- some common functionality
-- =====================================================================

db_need_priv :: Stmt -> [SqlValue] -> String -> GaIO ()
db_need_priv stmt vals desc =
 do r <- db_select_one_or_none stmt vals
    case r of
        Nothing -> throwError $ AuthErr $ "Privilege test failed: " ++ desc
        Just _  -> return ()

db_need_admin_priv :: Domain -> GaIO ()
db_need_admin_priv (Domain (Domainid domainid) (Domainname domainname) _) =
 do Userid userid <- uid <$> get_ctx
    Username username <- logname <$> get_ctx
    db_need_priv "SELECT 1 FROM admin WHERE domainid = ? AND userid = ?"
                [SqlInteger userid, SqlString username]
                 $ username ++ " must be admin of domain " ++ domainname

db_need_root_priv :: GaIO ()
db_need_root_priv =
 do Userid userid <- uid <$> get_ctx
    when (userid /= 0)
        (throwError $ AuthErr $ "Need root privileges for this operation")

db_get_domain :: Domainname -> GaIO Domain
db_get_domain (Domainname domainname) =
 do r <- fmap toDomain <$> db_select_one_or_none
                "SELECT id, name, comment FROM domain WHERE name = ?"
                [SqlString domainname]
    case r of
        Nothing -> throwError $ DbErr $ "No such domain: " ++ domainname
        Just x  -> return x
 where
    toDomain [SqlInteger id, SqlString name, SqlString comment] =
        Domain (Domainid id) (Domainname name) (Domaincomment comment)

db_get_user :: Username -> GaIO User
db_get_user (Username username) =
 do r <- fmap toUser <$> db_select_one_or_none
                "SELECT id, name, comment FROM user WHERE name = ?"
                [SqlString username]
    case r of
        Nothing -> throwError $ DbErr $ "No such user: " ++ username
        Just x  -> return x
 where
    toUser [SqlInteger id, SqlString name, SqlString comment] =
        User (Userid id) (Username name) (Usercomment comment)

db_get_repo :: Domainname -> Reponame -> GaIO Repo
db_get_repo (Domainname domain) (Reponame repo) =
 do r <- fmap toRepo <$> db_select_one_or_none
                "SELECT id, domainid, name, comment FROM repo WHERE name = ?"
                [SqlString repo]
    case r of
        Nothing -> throwError $ DbErr $ "No such repo: " ++ repo
        Just x  -> return x
 where
    toRepo [SqlInteger id, SqlInteger domainid,
            SqlString name, SqlString comment] =
        Repo (Repoid id) (Domainid domainid)
                (Reponame name) (Repocomment comment)

-- =====================================================================
-- extern functionality
-- =====================================================================

db_add_sshkey :: Sshkey -> GaIO Bool
db_add_sshkey (Sshkey (SshkeyAlgo algo) (SshkeyKey key) (SshkeyComment comment)) =
 do Userid userid <- uid <$> get_ctx
    db_insert_or_ignore
        "INSERT INTO sshkey (userid, algo, key, comment) VALUES (?, ?, ?, ?)"
        [SqlInteger userid, SqlString algo, SqlString key, SqlString comment]

db_remove_sshkey :: Sshkey -> GaIO Bool
db_remove_sshkey (Sshkey (SshkeyAlgo algo) (SshkeyKey key) (SshkeyComment comment)) =
 do Userid userid <- uid <$> get_ctx
    db_delete_one_or_ignore
        "DELETE FROM sshkey WHERE key = ?"
        [SqlString key]

db_ls_sshkeys :: GaIO [Sshkey]
db_ls_sshkeys =
 do Userid user <- uid <$> get_ctx
    map toSshkey <$> db_select
        "SELECT algo, key, comment FROM sshkey WHERE userid = ?"
        [SqlInteger user]
 where
    toSshkey [SqlString algo, SqlString key, SqlString comment] =
        Sshkey (SshkeyAlgo algo) (SshkeyKey key) (SshkeyComment comment)
    toSshkey _ = Sshkey (SshkeyAlgo "nix") (SshkeyKey "nix") (SshkeyComment "nix")

db_ls_users :: GaIO [User]
db_ls_users =
 do Userid user <- uid <$> get_ctx
    map toUser <$> db_select "SELECT id, name, comment FROM user" []
 where
    toUser [SqlInteger userid, SqlString name, SqlString comment] =
        User (Userid userid) (Username name) (Usercomment comment)

db_ls_repos :: Domainname -> GaIO [Repo]
db_ls_repos domain =
 do Userid userid <- uid <$> get_ctx
    d@(Domain (Domainid domainid) _ _) <- db_get_domain domain
    db_need_admin_priv d
    map toRepo <$> db_select
        "SELECT id, domainid, name, comment FROM repo WHERE domainid = ?"
        [SqlInteger domainid]
 where
    toRepo [SqlInteger id, SqlInteger domainid,
            SqlString name, SqlString comment] =
        Repo (Repoid id) (Domainid domainid)
                (Reponame name) (Repocomment comment)

db_create_domain :: Domainname -> Domaincomment -> GaIO ()
db_create_domain (Domainname name) (Domaincomment comment) =
 do db_need_root_priv
    db_insert "INSERT INTO domain (name, comment) VALUES (?, ?)"
                [SqlString name, SqlString comment]

db_rename_domain :: Domainname -> Domainname -> Domaincomment -> GaIO Bool
db_rename_domain domainname (Domainname name') (Domaincomment comment') =
 do d@(Domain (Domainid domainid) _ _) <- db_get_domain domainname
    db_need_admin_priv d
    db_update_one_or_none "UPDATE domain SET name = ?, comment = ? WHERE id = ?"
                [SqlString name', SqlString comment', SqlInteger domainid]

db_delete_domain :: Domainname -> GaIO ()
db_delete_domain name =
 do d@(Domain (Domainid domainid) _ _) <- db_get_domain name
    db_need_admin_priv d
    db_delete_one "DELETE FROM admin WHERE domainid = ?" [SqlInteger domainid]
    db_delete_one "DELETE FROM domain WHERE id = ?" [SqlInteger domainid]

db_create_user :: Username -> Usercomment -> GaIO ()
db_create_user (Username name) (Usercomment comment) =
 do db_need_root_priv
    db_insert "INSERT INTO user (name, comment) VALUES (?, ?)"
                [SqlString name, SqlString comment]

db_rename_user :: Username -> Username -> Usercomment -> GaIO Bool
db_rename_user username (Username name') (Usercomment comment') =
 do db_need_root_priv
    User (Userid userid) _ _ <- db_get_user username
    db_update_one_or_none "UPDATE user SET name = ?, comment = ? WHERE id = ?"
                [SqlString name', SqlString comment', SqlInteger userid]

db_delete_user :: Username -> GaIO ()
db_delete_user username =
 do db_need_root_priv
    User (Userid userid) _ _ <- db_get_user username
    db_delete_one "DELETE FROM user WHERE id = ?" [SqlInteger userid]

db_create_repo :: Domainname -> Reponame -> Repocomment -> GaIO ()
db_create_repo domainname (Reponame name) (Repocomment comment) =
 do d@(Domain (Domainid domainid) _ _) <- db_get_domain domainname
    db_need_admin_priv d
    db_insert "INSERT INTO repo (domainid, name, comment) VALUES (?, ?, ?)"
                [SqlInteger domainid, SqlString name, SqlString comment]

db_rename_repo :: Domainname -> Reponame -> Domainname -> Reponame -> Repocomment -> GaIO Bool
db_rename_repo domainname reponame domainname' (Reponame repo') (Repocomment comment') =
 do d <- db_get_domain domainname
    r@(Repo (Repoid repoid) _ _ _) <- db_get_repo domainname reponame
    d'@(Domain (Domainid domainid') _ _) <- db_get_domain domainname'
    db_need_admin_priv d
    db_need_admin_priv d'
    db_update_one_or_none "UPDATE repo SET domainid = ?, name = ?, comment = ?\
                          \ WHERE id = ?"
                [SqlInteger domainid', SqlString repo', SqlString comment',
                    SqlInteger repoid]

db_delete_repo :: Domainname -> Reponame -> GaIO ()
db_delete_repo domainname reponame =
 do d <- db_get_domain domainname
    db_need_admin_priv d
    r@(Repo (Repoid repoid) _ _ _) <- db_get_repo domainname reponame
    db_insert "DELETE FROM repo WHERE id = ?"
                [SqlInteger repoid]

db_grant_admin :: Domainname -> Username -> GaIO Bool
db_grant_admin domainname username =
 do d@(Domain (Domainid domainid) _ _) <- db_get_domain domainname
    db_need_admin_priv d
    u@(User (Userid userid) _ _) <- db_get_user username
    db_insert_or_ignore
            "INSERT INTO admin (domainid, userid) VALUES (?, ?)"
            [SqlInteger domainid, SqlInteger userid]

db_revoke_admin :: Domainname -> Username -> GaIO Bool
db_revoke_admin domainname username =
 do d@(Domain (Domainid domainid) _ _) <- db_get_domain domainname
    db_need_admin_priv d
    u@(User (Userid userid) _ _) <- db_get_user username
    db_delete_one_or_ignore
            "DELETE FROM admin WHERE domainid = ? AND userid = ?"
            [SqlInteger domainid, SqlInteger userid]

db_set_perm :: Domainname -> Reponame -> Username -> Perm -> GaIO ()
db_set_perm domainname reponame username perm =
 do d@(Domain (Domainid domainid) _ _) <- db_get_domain domainname
    db_need_admin_priv d
    r@(Repo (Repoid repoid) _ _ _) <- db_get_repo domainname reponame
    u@(User (Userid userid) _ _) <- db_get_user username
    db_delete_one_or_ignore
            "DELETE FROM permissions WHERE repoid = ? AND userid = ?"
            [SqlInteger repoid, SqlInteger userid]
    case perm of
        PermNone -> return ()
        PermR  -> db_insert
                    "INSERT INTO permissions (repoid, userid, mode) VALUES (?, ?, 0)"
                    [SqlInteger repoid, SqlInteger userid]
        PermRW -> db_insert
                    "INSERT INTO permissions (repoid, userid, mode) VALUES (?, ?, 1)"
                    [SqlInteger repoid, SqlInteger userid]

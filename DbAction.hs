{-# LANGUAGE GADTs #-}

module DbAction where

import Control.Applicative (Applicative, pure, (<*>))

import GaCtx
import GaIO
import Types
import qualified FsAction as Fs
import Db

data DbAction a where
    DbActionPure :: a -> DbAction a
    DbActionSeq  :: DbAction a -> (a -> DbAction b) -> DbAction b
    GetCtx       :: DbAction GaCtx
    FsAction     :: Fs.FsAction -> DbAction ()
    AddSshkey    :: Sshkey -> DbAction Bool
    RemoveSshkey :: Sshkey -> DbAction Bool
    LsSshkeys    :: DbAction [Sshkey]
    LsUsers      :: DbAction [User]
    LsRepos      :: Domainname -> DbAction [Repo]
    CreateDomain :: Domainname -> Domaincomment -> DbAction ()
    RenameDomain :: Domainname -> Domainname -> Domaincomment -> DbAction ()
    DeleteDomain :: Domainname -> DbAction ()
    CreateUser   :: Username -> Usercomment -> DbAction ()
    RenameUser   :: Username -> Username -> Usercomment -> DbAction ()
    DeleteUser   :: Username -> DbAction ()
    CreateRepo   :: Domainname -> Reponame -> Repocomment -> DbAction ()
    RenameRepo   :: Domainname -> Reponame -> Domainname -> Reponame -> Repocomment -> DbAction ()
    DeleteRepo   :: Domainname -> Reponame -> DbAction ()
    GrantAdmin   :: Domainname -> Username -> DbAction Bool
    RevokeAdmin  :: Domainname -> Username -> DbAction Bool
    SetPerm      :: Domainname -> Reponame -> Username -> Perm -> DbAction ()

instance Functor DbAction where
    fmap f = (>>= return . f)

instance Applicative DbAction where
    af <*> ax = af >>= flip fmap ax
    pure = DbActionPure

instance Monad DbAction where
    (>>=) = DbActionSeq
    return = DbActionPure

compileDbAction :: DbAction a -> GaIO a
compileDbAction (DbActionPure a)       = return a
compileDbAction (DbActionSeq ma f)     = compileDbAction ma >>= compileDbAction . f
compileDbAction (GetCtx)               = get_ctx
compileDbAction (FsAction fa)          = Fs.compileFsAction fa
compileDbAction (AddSshkey key)        = db_add_sshkey key
compileDbAction (RemoveSshkey key)     = db_remove_sshkey key
compileDbAction (LsSshkeys)            = db_ls_sshkeys
compileDbAction (LsUsers)              = db_ls_users
compileDbAction (LsRepos d)            = db_ls_repos d
compileDbAction (CreateDomain d c)     = db_create_domain d c
compileDbAction (RenameDomain d d' c') = db_rename_domain d d' c'
compileDbAction (DeleteDomain d)       = db_delete_domain d
compileDbAction (CreateUser u c)       = db_create_user u c
compileDbAction (RenameUser u u' c')   = db_rename_user u u' c'
compileDbAction (DeleteUser u)         = db_delete_user u
compileDbAction (CreateRepo d r c)     = db_create_repo d r c
compileDbAction (RenameRepo d r d' r' c') = db_rename_repo d r d' r' c'
compileDbAction (DeleteRepo d r)       = db_delete_repo d r
compileDbAction (GrantAdmin d u)       = db_grant_admin d u
compileDbAction (RevokeAdmin d u)      = db_revoke_admin d u
compileDbAction (SetPerm d r u p)      = db_set_perm d r u p

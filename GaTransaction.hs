{-# LANGUAGE GADTs #-}

module GaTransaction where

import Control.Applicative (Applicative, pure, (<$>), (<*>))

import GaIO
import GaCtx
import Sshkey
import Types
import qualified DbAction as Db
import qualified FsAction as Fs

-- Blocking I/O inside transactions must be avoided. Therefore this closed set
-- is defined.

data GaTransaction a where
  DbSeq        :: GaTransaction a -> (a -> GaTransaction b) -> GaTransaction b
  DbPure       :: a -> GaTransaction a
  GetCtx       :: GaTransaction GaCtx
  LogMesg      :: String -> GaTransaction String
  AddSshkey    :: Sshkey -> GaTransaction Bool
  RemoveSshkey :: Sshkey -> GaTransaction Bool
  LsSshkeys    :: GaTransaction [Sshkey]
  LsUsers      :: GaTransaction [User]
  LsRepos      :: Domainname -> GaTransaction [Repo]
  CreateDomain :: Domainname -> Domaincomment -> GaTransaction ()
  RenameDomain :: Domainname -> Domainname -> Domaincomment -> GaTransaction Bool
  DeleteDomain :: Domainname -> GaTransaction ()
  CreateUser   :: Username -> Usercomment -> GaTransaction ()
  RenameUser   :: Username -> Username -> Usercomment -> GaTransaction Bool
  DeleteUser   :: Username -> GaTransaction ()
  CreateRepo   :: Domainname -> Reponame -> Repocomment -> GaTransaction ()
  RenameRepo   :: Domainname -> Reponame -> Domainname -> Reponame -> Repocomment -> GaTransaction Bool
  DeleteRepo   :: Domainname -> Reponame -> GaTransaction ()
  GrantAdmin   :: Domainname -> Username -> GaTransaction Bool
  RevokeAdmin  :: Domainname -> Username -> GaTransaction Bool
  SetPerm      :: Domainname -> Reponame -> Username -> Perm -> GaTransaction ()

instance Functor GaTransaction where
  fmap f ga = DbSeq ga (return . f)

instance Applicative GaTransaction where
  pure = DbPure
  mf <*> ma = do f <- mf; a <- ma; return (f a)

instance Monad GaTransaction where
  (>>=) = DbSeq
  return = DbPure

compileGaTransaction :: GaTransaction a -> Db.DbAction a
compileGaTransaction = go where
  go (DbSeq m f)               = compileGaTransaction m >>= compileGaTransaction . f
  go (DbPure a)                = return a
  go (GetCtx)                  = Db.GetCtx
  go (AddSshkey key)           = do b <- Db.AddSshkey key
                                    Db.LsSshkeys >>= Db.FsAction . Fs.UpdateSshkeys
                                    return b
  go (RemoveSshkey key)        = do b <- Db.RemoveSshkey key
                                    Db.LsSshkeys >>= Db.FsAction . Fs.UpdateSshkeys
                                    return b
  go (LsSshkeys)               = Db.LsSshkeys
  go (LsUsers)                 = Db.LsUsers
  go (LsRepos d)               = Db.LsRepos d
  go (CreateDomain d c)        = Db.CreateDomain d c
                                 >> Db.FsAction (Fs.CreateDomain d c)
  go (RenameDomain d d' c')    = do b <- Db.RenameDomain d d' c'
                                    Db.FsAction (Fs.RenameDomain d d' c')
                                    return b
  go (DeleteDomain d)          = Db.DeleteDomain d
                                 >> Db.FsAction (Fs.DeleteDomain d)
  go (CreateUser u c)          = Db.CreateUser u c
  go (RenameUser u u' c')      = Db.RenameUser u u' c'
  go (DeleteUser u)            = Db.DeleteUser u
  go (CreateRepo d r c)        = Db.CreateRepo d r c
                                 >> Db.FsAction (Fs.CreateRepo d r c)
  go (RenameRepo d r d' r' c') = do b <- Db.RenameRepo d r d' r' c'
                                    Db.FsAction (Fs.RenameRepo d r d' r' c')
                                    return b
  go (DeleteRepo d r)          = Db.DeleteRepo d r
                                 >> Db.FsAction (Fs.DeleteRepo d r)
  go (GrantAdmin d u)          = Db.GrantAdmin d u
  go (RevokeAdmin d u)         = Db.RevokeAdmin d u
  go (SetPerm d r u p)         = Db.SetPerm d r u p

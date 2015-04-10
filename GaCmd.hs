{-# LANGUAGE GADTs #-}

module GaCmd where

import Control.Applicative (Applicative, pure, (<$>), (<*>))
import Control.Monad (msum)
import Control.Monad.Trans.Either (EitherT(..), runEitherT)

import GaTransaction
import GaAction
import GaCtx
import Sshkey
import Types

data GaCmd a where
    CmdSeq :: GaCmd a -> (a -> GaCmd b) -> GaCmd b
    CmdPure :: a -> GaCmd a
    CmdWhoami :: GaCmd Username
    CmdAddSshkey :: Maybe Sshkey -> GaCmd Bool
    CmdRemoveSshkey :: Maybe Sshkey -> GaCmd Bool
    CmdLsSshkeys :: GaCmd [Sshkey]
    CmdLsUsers :: GaCmd [User]
    CmdLsRepos :: Domainname -> GaCmd [Repo]
    CmdCreateUser :: Username -> Usercomment -> GaCmd ()
    CmdRenameUser :: Username -> Username -> Usercomment -> GaCmd Bool
    CmdDeleteUser :: Username -> GaCmd ()
    CmdCreateDomain :: Domainname -> Domaincomment -> GaCmd ()
    CmdRenameDomain :: Domainname -> Domainname -> Domaincomment -> GaCmd Bool
    CmdDeleteDomain :: Domainname -> GaCmd ()
    CmdCreateRepo :: Domainname -> Reponame -> Repocomment -> GaCmd ()
    CmdRenameRepo :: Domainname -> Reponame -> Domainname -> Reponame -> Repocomment -> GaCmd Bool
    CmdDeleteRepo :: Domainname -> Reponame -> GaCmd ()
    CmdGrantAdmin :: Domainname -> Username -> GaCmd Bool
    CmdRevokeAdmin :: Domainname -> Username -> GaCmd Bool
    CmdSetPerm :: Domainname -> Reponame -> Username -> Perm -> GaCmd ()

instance Functor GaCmd where
    fmap f = (>>= return . f)

instance Monad GaCmd where
    (>>=) = CmdSeq
    return = CmdPure

compile :: GaCmd a -> GaAction a
compile (CmdSeq m f)                 = compile m >>= compile . f
compile (CmdPure a)                  = return a
compile (CmdWhoami)                  = do let ctx = RunTransaction GetCtx
                                          Username name <- logname <$> ctx
                                          PrintStdout name
                                          return (Username name)
compile (CmdAddSshkey mb)            = do key <- get_sshkey mb
                                          RunTransaction (AddSshkey key)
compile (CmdRemoveSshkey mb)         = do key <- get_sshkey mb
                                          RunTransaction (RemoveSshkey key)
compile (CmdLsSshkeys)               = RunTransaction (LsSshkeys)
compile (CmdLsUsers)                 = RunTransaction (LsUsers)
compile (CmdLsRepos d)               = RunTransaction (LsRepos d)
compile (CmdCreateDomain d c)        = RunTransaction (CreateDomain d c)
compile (CmdRenameDomain d d' c')    = RunTransaction (RenameDomain d d' c')
compile (CmdDeleteDomain d)          = RunTransaction (DeleteDomain d)
compile (CmdCreateUser u c)          = RunTransaction (CreateUser u c)
compile (CmdRenameUser u u' c')      = RunTransaction (RenameUser u u' c')
compile (CmdDeleteUser u)            = RunTransaction (DeleteUser u)
compile (CmdCreateRepo d r c)        = RunTransaction (CreateRepo d r c)
compile (CmdRenameRepo d r d' r' c') = RunTransaction (RenameRepo d r d' r' c')
compile (CmdDeleteRepo d r)          = RunTransaction (DeleteRepo d r)
compile (CmdGrantAdmin d u)          = RunTransaction (GrantAdmin d u)
compile (CmdRevokeAdmin d u)         = RunTransaction (RevokeAdmin d u)
compile (CmdSetPerm d r u p)         = RunTransaction (SetPerm d r u p)

get_sshkey :: Maybe Sshkey -> GaAction Sshkey
get_sshkey (Just s) = return s
get_sshkey Nothing = patiently fromstdin where
    patiently :: GaAction (Either String Sshkey) -> GaAction Sshkey
    patiently = fmap fromRight . runEitherT . msum . repeat . EitherT
    fromRight :: Either a b -> b
    fromRight (Left a)  = error "XXX"
    fromRight (Right b) = b
    fromstdin :: GaAction (Either String Sshkey)
    fromstdin = do ToStderrIfTTY "Please give SSH public key on one line"
                   (parse_sshkey . words) <$> ReadLineStdin

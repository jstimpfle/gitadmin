module FsAction where

import GaIO (GaIO)
import Fs
import Types

data FsAction
    = UpdateSshkeys [Sshkey]
    | CreateDomain Domainname Domaincomment
    | RenameDomain Domainname Domainname Domaincomment
    | DeleteDomain Domainname
    | CreateRepo Domainname Reponame Repocomment
    | RenameRepo Domainname Reponame Domainname Reponame Repocomment
    | DeleteRepo Domainname Reponame

compileFsAction :: FsAction -> GaIO ()
compileFsAction (UpdateSshkeys keys) = fs_update_authorizedkeysfile keys
compileFsAction (CreateDomain d c)   = fs_create_domain d c
compileFsAction (RenameDomain d d' c') = fs_rename_domain d d' c'
compileFsAction (DeleteDomain d)     = fs_delete_domain d
compileFsAction (CreateRepo d r c)   = fs_create_repo d r c
compileFsAction (RenameRepo d r d' r' c') = fs_rename_repo d r d' r' c'
compileFsAction (DeleteRepo d r)     = fs_delete_repo d r

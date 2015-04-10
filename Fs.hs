-- =====================================================================
-- elementary (IO) operations
-- =====================================================================

module Fs
  ( fs_update_authorizedkeysfile
  , fs_create_domain
  , fs_rename_domain
  , fs_delete_domain
  , fs_create_repo
  , fs_rename_repo
  , fs_delete_repo
  , module Types
  )
where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (join)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Either (EitherT(..), runEitherT)
import System.Directory (createDirectory, renameDirectory, removeDirectory, removeDirectoryRecursive, renameFile)
import System.Exit (ExitCode(..))
import System.FilePath (joinPath)
import System.IO.Error (catchIOError)
import System.Process (proc, createProcess, waitForProcess)

import GaCtx
import GaErr
import GaIO
import Types

liftIO1 :: MonadIO m => (a -> IO b) -> a -> m b
liftIO1 f a = liftIO (f a)

liftIO2 :: MonadIO m => (a -> b -> IO c) -> a -> b -> m c
liftIO2 f a b = liftIO (f a b)

catch_ioerror :: IO a -> (IOError -> GaErr) -> GaIO a
catch_ioerror m h = GaIO $ EitherT $ lift $
                catchIOError (Right <$> m) (return . Left . h)

basedir_path :: GaIO FilePath
basedir_path = get_ctx >>= return . baseDir

domain_path :: Domainname -> GaIO FilePath
domain_path (Domainname domain) =
 do bd <- basedir_path
    return $ joinPath [bd, domain]

repo_path :: Domainname -> Reponame -> GaIO FilePath
repo_path domain (Reponame repo) =
 do dp <- domain_path domain
    return $ joinPath [dp, repo]

authorizedkeys_path :: GaIO FilePath
authorizedkeys_path =
 do bd <- basedir_path
    return $ joinPath [bd, "authorized_keys"]

create_dir :: FilePath -> GaIO ()
create_dir fp = catch_ioerror m h where
  m   = createDirectory fp
  h e = FSErr "failed to create directory"

rename_dir :: FilePath -> FilePath -> GaIO ()
rename_dir fp1 fp2 = catch_ioerror m h where
  m   = renameDirectory fp1 fp2
  h e = FSErr "failed to rename directory"

delete_dir :: FilePath -> GaIO ()
delete_dir d = catch_ioerror m h where
  m   = removeDirectory d
  h e = FSErr "Failed"

run_process :: FilePath -> [String] -> GaIO ()
run_process fp args = GaIO $ EitherT $ lift $
 do (Nothing,Nothing,Nothing,ph) <- createProcess (proc fp args)
    e <- waitForProcess ph
    return $ case e of
      ExitSuccess   -> Left (UnexpectedErr "Process exited with nonzero code")
      ExitFailure x -> Right ()

create_git_repo :: Domainname -> Reponame -> GaIO ()
create_git_repo d r =
 do g <- repo_path d r
    run_process "git" ["init", "--bare", g]

rename_git_repo :: Domainname -> Reponame -> Domainname -> Reponame -> GaIO ()
rename_git_repo d1 r1 d2 r2 =
 do g1 <- repo_path d1 r1
    g2 <- repo_path d2 r2
    rename_dir g1 g2

delete_git_repo :: Domainname -> Reponame -> GaIO ()
delete_git_repo d r =
 do g <- repo_path d r
    run_process "rm" ["-f", "--", g]

create_domain_dir :: Domainname -> GaIO ()
create_domain_dir d =
 do dp <- domain_path d
    create_dir dp

rename_domain_dir :: Domainname -> Domainname -> GaIO()
rename_domain_dir d1 d2 =
 do dp1 <- domain_path d1
    dp2 <- domain_path d2
    rename_dir dp1 dp2

delete_domain_dir :: Domainname -> GaIO ()
delete_domain_dir d =
 do dp1 <- domain_path d
    delete_dir dp1

fs_update_authorizedkeysfile :: [Sshkey] -> GaIO ()
fs_update_authorizedkeysfile keys =
 do Username username <- logname <$> get_ctx
    filename <- authorizedkeys_path
    let tmpfilename = filename ++ ".tmp"
        contents = unlines $ map (authorized_keys_line username) keys
    liftIO $ writeFile tmpfilename contents
    liftIO $ renameFile tmpfilename filename
 where
    authorized_keys_line username(Sshkey (SshkeyAlgo algo) (SshkeyKey key) (SshkeyComment comment)) =
        "command=\"gitadmin -u " ++ username ++ "\" " ++ unwords [algo, key, comment]

fs_create_domain :: Domainname -> Domaincomment -> GaIO ()
fs_create_domain domainname _ = join $ liftIO1 createDirectory <$> domain_path domainname

fs_rename_domain :: Domainname -> Domainname -> Domaincomment -> GaIO ()
fs_rename_domain d d' c' = join $ liftIO2 renameDirectory
                                                    <$> domain_path d
                                                    <*> domain_path d'

fs_delete_domain :: Domainname -> GaIO ()
fs_delete_domain domainname = liftIO1 removeDirectory =<< domain_path domainname

fs_create_repo :: Domainname -> Reponame -> Repocomment -> GaIO ()
fs_create_repo = undefined

fs_rename_repo :: Domainname -> Reponame -> Domainname -> Reponame -> Repocomment -> GaIO ()
fs_rename_repo d r d' r' c' = join $ liftIO2 renameDirectory
                                                    <$> domain_path d
                                                    <*> domain_path d'

fs_delete_repo :: Domainname -> Reponame -> GaIO ()
fs_delete_repo d r = join $ liftIO1 removeDirectoryRecursive <$> repo_path d r

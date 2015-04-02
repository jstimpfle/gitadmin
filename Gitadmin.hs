{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

import Control.Applicative (Applicative(..), (<$>), (<*>))
import Control.Monad (when, guard, msum)
import Control.Monad.Trans.Either (EitherT(..))
import Control.Monad.Trans.Reader (ReaderT(..), ask)
import Control.Monad.Trans (lift)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Maybe (fromJust)
import System.Directory (createDirectory, renameDirectory, removeDirectory)
import System.Environment (getArgs)
import System.Exit (ExitCode(..))
import System.FilePath (joinPath)
import System.IO (Handle, stdin, stdout, stderr, hPutStrLn)
import System.IO.Error (catchIOError)
import System.Posix.Terminal (queryTerminal)
import System.Posix.IO (handleToFd)
import System.Process (proc, createProcess, waitForProcess)
import Text.Regex.Posix ((=~))
--import Database.HDBC
--import Database.HDBC.Sqlite3 (connectSqlite3)

data Userid = Userid Int deriving (Show, Eq)
data Username = Username String deriving (Show, Eq)
data Domainname = Domainname String deriving (Show, Eq)
data Reponame = Reponame String deriving (Show, Eq)
data Usercomment = Usercomment String deriving (Show, Eq)
data Domaincomment = Domaincomment String deriving (Show, Eq)
data Repocomment = Repocomment String deriving (Show, Eq)
data SshkeyAlgo = SshkeyAlgo String deriving (Show, Eq)
data SshkeyKey = SshkeyKey String deriving (Show, Eq)
data SshkeyComment = SshkeyComment String deriving (Show, Eq)
data Sshkey = Sshkey SshkeyAlgo SshkeyKey SshkeyComment deriving (Show, Eq)

--type Row = [SqlValue]

domainname_regex = "[abcdefghijklmnopqrstuvwxyz]{2,16}"
username_regex   = "[abcdefghijklmnopqrstuvwxyz]{2,16}"
reponame_regex   = "[abcdefghijklmnopqrstuvwxyz0123456789-]{2,32}"
domaincomment_regex = "[abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789 &'()\\+,\\./<>@-]{0,255}"
usercomment_regex   = "[abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789 &'()\\+,\\./<>@-]{0,255}"
repocomment_regex   = "[abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789 &'()\\+,\\./<>@-]{0,255}"
sshkey_algo_regex    = "(ssh-ed25519|ssh-rsa|ssh-dss|ecdsa-sha2-nistp256|ecdsa-sha2-nistp384|ecdsa-sha2-nistp521)"
sshkey_key_regex     = "[abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789\\+/]{0,8192}={0,2}"
sshkey_comment_regex = "[abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789\\+/@-]{0,63}"

parse_sshkey :: [String] -> Either String Sshkey
parse_sshkey [a,b,c] =
 do algo    <- check a sshkey_algo_regex    "algorithm field"
    key     <- check b sshkey_key_regex     "key field"
    comment <- check b sshkey_comment_regex "comment field"
    return $ Sshkey (SshkeyAlgo a) (SshkeyKey b) (SshkeyComment c)
  where
    check x re desc = when (not (x =~ re)) (Left $ "Failed to parse ssh key: \"" ++ desc ++ "\". SSH keys consist of three components, as for example in : ssh-rsa ABCFOO me@box")
parse_sshkey _ = Left "SSH keys consist of three components"

-- =====================================================================
-- command-line interface
-- =====================================================================

main :: IO ()
main = do
  r <- parse_args <$> getArgs
  case r of
    Left msg   -> putStrLn msg
    Right conf -> gitadmin conf

parse_args :: [String] -> Either String RunConf
parse_args = \case
  "--help" : rest    -> const ShowHelp         <$> guard (null rest)
  "--init" : rest    -> const Initialize       <$> guard (null rest)
  "-u" : user : rest -> Call (Username user)   <$> parse_call rest
  rest               -> Call (Username "root") <$> parse_call rest

parse_call :: [String] -> Either String GaCall
parse_call ("-c":scr:rest) = guard (length rest == 0) >> return (CallScript scr)
parse_call args = return $ if null args then CallInteractive else CallArray args

long_usage :: String
long_usage = "Usage: TODO\n"

init_ga_env :: IO ()
init_ga_env = undefined

-- =====================================================================
-- software interface
-- =====================================================================

data RunConf
  = ShowHelp
  | Initialize
  | Call Username GaCall

gitadmin :: RunConf -> IO ()
gitadmin ShowHelp = show_help
gitadmin Initialize = init_ga_env
gitadmin (Call user call) = run_call
 where
  run_call :: IO ()
  run_call =
   do ctx <- mk_ctx
      case gacmd of
        Left msg -> putStrLn msg
        Right cmd -> run_gaio ctx $ compileGaAction $ compile cmd

  gacmd :: Either String (GaCmd ())
  gacmd = case call of
    CallArray arr -> parse_cmd_array arr
    CallScript scr -> parse_script scr
    CallInteractive -> Left "interactive not yet supported"

parse_cmd_array :: [String] -> Either String (GaCmd ())
parse_cmd_array xs = go xs where
  go ["whoami"] = Right $ CmdWhoami >> return ()
  go ("addkey":rest) = parse_sshkey rest >>= \key ->
                  (Right $ CmdAddSshkey (Just key) >> return ())
  go ["addkey"] = Right $ CmdAddSshkey Nothing >> return ()
  go _          = Left  $ "Failed to parse command line"

parse_script :: String -> Either String (GaCmd ())
parse_script = undefined

show_help :: IO ()
show_help = putStr long_usage

mk_ctx :: IO GaCtx
mk_ctx = GaCtx "/home/jfs" undefined
               stdin stdout stderr
               (Userid 0) (Username "root")
               <$> (queryTerminal =<< handleToFd stdin)
               <*> (queryTerminal =<< handleToFd stdout)
               <*> (queryTerminal =<< handleToFd stderr)

-- =====================================================================
-- API
-- =====================================================================

data GaCall
  = CallArray [String]
  | CallScript String
  | CallInteractive

data GaCmd a where
  CmdSeq :: GaCmd a -> (a -> GaCmd b) -> GaCmd b
  CmdPure :: a -> GaCmd a
  CmdWhoami :: GaCmd Username
  CmdAddSshkey :: Maybe Sshkey -> GaCmd ()
  CmdRemoveSshkey :: Maybe Sshkey -> GaCmd ()
  CmdLsSshkeys :: GaCmd [Sshkey]
  CmdLsUsers :: GaCmd [Username]
  CmdLsRepos :: Domainname -> GaCmd [(Domainname, Reponame, Repocomment)]
  CmdCreateUser :: Username -> Usercomment -> GaCmd ()
  CmdRenameUser :: Username -> Username -> Usercomment -> GaCmd ()
  CmdDeleteUser :: Username -> GaCmd ()
  CmdCreateDomain :: Domainname -> Domaincomment -> GaCmd ()
  CmdRenameDomain :: Domainname -> Domainname -> Domaincomment -> GaCmd ()
  CmdDeleteDomain :: Domainname -> GaCmd ()
  CmdCreateRepo :: Domainname -> Reponame -> Repocomment -> GaCmd ()
  CmdRenameRepo :: Domainname -> Reponame -> Domainname -> Reponame -> Repocomment -> GaCmd ()
  CmdDeleteRepo :: Domainname -> Reponame -> GaCmd ()
  CmdGrantAdmin :: Domainname -> Username -> GaCmd ()
  CmdRevokeAdmin :: Domainname -> Username -> GaCmd ()
  CmdSetPerm :: Domainname -> Reponame -> Username -> Perm -> GaCmd ()

data Perm
  = PermNone
  | PermR
  | PermRW

instance Monad GaCmd where
  (>>=) = CmdSeq
  return = CmdPure

data GaErr
  = DbErr String
  | AuthErr String
  | FSErr String
  | UnexpectedErr String
 deriving (Show)

-- =====================================================================
-- internals
-- =====================================================================

data GaCtx
  = GaCtx
  { baseDir :: FilePath
  , dbHandle :: ({- ? -})
  , stdin_handle :: Handle
  , stdout_handle :: Handle
  , stderr_handle :: Handle
  , uid :: Userid
  , logname :: Username
  , is_stdin_tty :: Bool
  , is_stdout_tty :: Bool
  , is_stderr_tty :: Bool
  }

-- | The ingredients of which gitadmin commands are made

data GaAction a where
  GaSeq :: GaAction a -> (a -> GaAction b) -> GaAction b
  GaPure :: a -> GaAction a
  ReadLineStdin :: GaAction String
  ToStderrIfTTY :: String -> GaAction ()
  LogStderr :: String -> GaAction ()
  PrintStdout :: String -> GaAction ()
  RunTransaction :: DbAction a -> GaAction a

-- | Represent things that can be run inside a DB transaction
--
-- Blocking I/O inside transactions must be avoided. Therefore this closed set
-- is defined.

data DbAction a where
  DbSeq :: DbAction a -> (a -> DbAction b) -> DbAction b
  DbPure :: a -> DbAction a
  GetCtx :: DbAction GaCtx
  LogMesg :: String -> DbAction String
  AddSshkey :: Sshkey -> DbAction ()
  RemoveSshkey :: Sshkey -> DbAction ()
  LsSshkeys :: DbAction [Sshkey]
  LsUsers :: DbAction [Username]
  LsRepos :: Domainname -> DbAction [(Domainname, Reponame, Repocomment)]
  CreateDomain :: Domainname -> Domaincomment -> DbAction ()
  RenameDomain :: Domainname -> Domainname -> Domaincomment -> DbAction ()
  DeleteDomain :: Domainname -> DbAction ()
  CreateUser :: Username -> Usercomment -> DbAction ()
  RenameUser :: Username -> Username -> Usercomment -> DbAction ()
  DeleteUser :: Username -> DbAction ()
  CreateRepo :: Domainname -> Reponame -> Repocomment -> DbAction ()
  RenameRepo :: Domainname -> Reponame -> Domainname -> Reponame -> Repocomment -> DbAction ()
  DeleteRepo :: Domainname -> Reponame -> DbAction ()
  GrantAdmin :: Domainname -> Username -> DbAction ()
  RevokeAdmin :: Domainname -> Username -> DbAction ()
  SetPerm :: Domainname -> Reponame -> Username -> Perm -> DbAction ()

instance Functor GaAction where
  fmap f ga = GaSeq ga (return . f)

instance Applicative GaAction where
  mf <*> ma = do f <- mf; a <- ma; return (f a)

instance Monad GaAction where
  (>>=) = GaSeq
  return = GaPure

instance Functor DbAction where
  fmap f ga = DbSeq ga (return . f)

instance Applicative DbAction where
  mf <*> ma = do f <- mf; a <- ma; return (f a)

instance Monad DbAction where
  (>>=) = DbSeq
  return = DbPure

-- =====================================================================
-- compile GaCmd to GaAction
-- =====================================================================

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

-- =====================================================================
-- GaIO is the execution context for programs in the GaAction language
-- =====================================================================

newtype GaIO a = GaIO { unGaIO :: EitherT GaErr (ReaderT GaCtx IO) a }

get_ctx :: GaIO GaCtx
get_ctx = GaIO $ lift ask

run_gaio :: GaCtx -> GaIO a -> IO ()
run_gaio ctx gaio = do
  r <- runReaderT (runEitherT (unGaIO gaio)) ctx
  case r of
    Left a -> putStrLn $ "Error: " ++ show a
    Right _ -> return ()

instance Monad GaIO where
  GaIO m >>= f = GaIO $ m >>= (unGaIO . f)
  return = GaIO . return

instance MonadIO GaIO where
  liftIO = GaIO . EitherT . lift . fmap Right

-- =====================================================================
-- compile GaAction to GaIO
-- =====================================================================

compileGaAction :: GaAction a -> GaIO a
compileGaAction = go where
  go :: GaAction a -> GaIO a
  go (GaSeq m f)        = go m >>= (go . f)
  go (GaPure a)         = return a
  go (ReadLineStdin)    = liftIO $ getLine
  go (ToStderrIfTTY s)  = do x <- get_ctx
                             when (is_stderr_tty x)
                               (liftIO $ hPutStrLn (stderr_handle x) s)
  go (LogStderr s)      = do x <- get_ctx
                             liftIO $ hPutStrLn (stderr_handle x) s
  go (PrintStdout s)    = do x <- get_ctx
                             liftIO $ hPutStrLn (stdout_handle x) s
  go (RunTransaction a) = compileDbAction a

-- =====================================================================
-- compile DbAction to GaIO
-- =====================================================================

compileDbAction :: DbAction a -> GaIO a
compileDbAction = go where
  go (DbSeq m f)     = compileDbAction m >>= (compileDbAction . f)
  go (DbPure a)      = return a
  go (GetCtx)        = get_ctx
  go (AddSshkey key) = liftIO $ error "addkey not implemented"

-- =====================================================================
-- elementary (IO) operations
-- =====================================================================

catch_ioerror :: IO a -> (IOError -> GaErr) -> GaIO a
catch_ioerror m h = GaIO $ EitherT $ lift $
                catchIOError (Right <$> m) (return . Left . h)

domain_path :: Domainname -> GaIO FilePath
domain_path (Domainname domain) =
 do bd <- get_ctx >>= return . baseDir
    return $ joinPath [bd, domain]

repo_path :: Domainname -> Reponame -> GaIO FilePath
repo_path domain (Reponame repo) =
 do dp <- domain_path domain
    return $ joinPath [dp, repo]

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

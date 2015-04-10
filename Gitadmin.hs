module Gitadmin
  ( gitadmin
  , GaCall(..)
  )
where 

import Control.Applicative (Applicative(..), (<$>), (<*>))
import Control.Monad.Trans.Either (EitherT(..))
import Control.Monad.Trans (lift)
import Control.Monad.IO.Class (MonadIO(..))
import System.Exit (ExitCode(..))
import System.IO (hPutStrLn, stderr)
import Text.Regex.Posix ((=~))

import Types
import Sshkey
import GaIO
import GaAPI
import GaCmd
import GaAction

{-
domainname_regex, username_regex, reponame_regex, domaincomment_regex, usercomment_regex, repocomment_regex :: String
domainname_regex = "[abcdefghijklmnopqrstuvwxyz]{2,16}"
username_regex   = "[abcdefghijklmnopqrstuvwxyz]{2,16}"
reponame_regex   = "[abcdefghijklmnopqrstuvwxyz0123456789-]{2,32}"
domaincomment_regex = "[abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789 &'()\\+,\\./<>@-]{0,255}"
usercomment_regex   = "[abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789 &'()\\+,\\./<>@-]{0,255}"
repocomment_regex   = "[abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789 &'()\\+,\\./<>@-]{0,255}"
-}

data GaCall
  = CallArray [String]
  | CallScript String
  | CallInteractive

gitadmin :: Username -> GaCall -> IO ExitCode
gitadmin username call =
    case parse_cmd call of
        Left msg -> hPutStrLn stderr msg >> return (ExitFailure 2)
        Right cmd -> run_cmd username cmd >> return ExitSuccess

parse_cmd :: GaCall -> Either String (GaCmd ())
parse_cmd call = case call of
    CallArray arr -> parse_cmd_array arr
    CallScript scr -> parse_script scr
    CallInteractive -> Left "interactive not yet supported"

parse_cmd_array :: [String] -> Either String (GaCmd ())
parse_cmd_array xs = go xs where
    go ["whoami"]       = Right $ const () <$> CmdWhoami
    go ["addkey"]       = Right $ const () <$> (CmdAddSshkey Nothing)
    go ("addkey":rest)  = parse_sshkey rest >>= \key ->
                               (Right $ const () <$> CmdAddSshkey (Just key))
    go ["removekey"]    = Right $ const () <$> CmdRemoveSshkey Nothing
    go _                = Left "Failed to parse command line"

parse_script :: String -> Either String (GaCmd ())
parse_script = undefined

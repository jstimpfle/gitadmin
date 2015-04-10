module Gitadmin
  ( gitadmin
  , GaCall(..)
  )
where 

import Control.Applicative (Applicative(..), (<$>), (<*>))
import Control.Monad.Trans.Either (EitherT(..))
import Control.Monad.Trans (lift)
import Control.Monad.IO.Class (MonadIO(..))
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

gitadmin :: Username -> GaCall -> IO ()
gitadmin username call =
    case parse_cmd call of
        Left msg -> putStrLn msg
        Right cmd -> run_cmd username cmd

parse_cmd :: GaCall -> Either String (GaCmd ())
parse_cmd call = case call of
    CallArray arr -> parse_cmd_array arr
    CallScript scr -> parse_script scr
    CallInteractive -> Left "interactive not yet supported"

parse_cmd_array :: [String] -> Either String (GaCmd ())
parse_cmd_array xs = go xs where
    go ["whoami"] = Right $ CmdWhoami >> return ()
    go ["addkey"] = Right $ CmdAddSshkey Nothing >> return ()
    go ("addkey":rest) = parse_sshkey rest >>= \key ->
                            (Right $ CmdAddSshkey (Just key) >> return ())
    go _ = Left  $ "Failed to parse command line"

parse_script :: String -> Either String (GaCmd ())
parse_script = undefined

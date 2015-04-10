module Sshkey where

import Control.Monad (when)
import Text.Regex.Posix ((=~))

import Types

sshkey_algo_regex    = "(ssh-ed25519|ssh-rsa|ssh-dss|ecdsa-sha2-nistp256|ecdsa-sha2-nistp384|ecdsa-sha2-nistp521)"
sshkey_key_regex     = "[abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789\\+/]{0,8192}={0,2}"
sshkey_comment_regex = "[abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789\\+/@-]{0,63}"

parse_sshkey :: [String] -> Either String Sshkey
parse_sshkey [a,b,c] =
 do check a sshkey_algo_regex    "algorithm field"
    check b sshkey_key_regex     "key field"
    check b sshkey_comment_regex "comment field"
    return $ Sshkey (SshkeyAlgo a) (SshkeyKey b) (SshkeyComment c)
 where
    check x re desc = when (not (x =~ re)) $ Left $
                                "Failed to parse ssh key: \"" ++ desc ++ "\"."
                                ++ " SSH keys consist of three components,"
                                ++ " as for example in: ssh-rsa ABCFOO me@box"

parse_sshkey _ = Left "SSH key needs three components: algorithm, key, comment"

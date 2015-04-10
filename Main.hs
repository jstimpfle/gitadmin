{-# LANGUAGE LambdaCase #-}

import Control.Applicative ((<$>))
import Control.Monad (guard)
import System.Environment (getArgs)
import System.Exit (ExitCode(..), exitWith)
import System.IO (hPutStrLn, stderr)

import Types (Username(..))
import Help (show_help)
import Initialize (initialize)
import Gitadmin (GaCall(..), gitadmin)

data RunConf
    = ShowHelp
    | Initialize
    | Call Username GaCall

main :: IO ()
main =
 do runconf <- parse_args <$> getArgs
    r <- case runconf of
        Left msg -> hPutStrLn stderr msg >> return (ExitFailure 2)
        Right ShowHelp -> show_help >> return ExitSuccess
        Right Initialize -> initialize
        Right (Call username gacall) -> gitadmin username gacall
    exitWith r

parse_args :: [String] -> Either String RunConf
parse_args = \case
    "--help" : rest    -> const ShowHelp         <$> guard (null rest)
    "--init" : rest    -> const Initialize       <$> guard (null rest)
    "-u" : user : rest -> Call (Username user)   <$> parse_call rest
    rest               -> Call (Username "root") <$> parse_call rest

parse_call :: [String] -> Either String GaCall
parse_call ("-c":scr:rest) = guard (length rest == 0) >> return (CallScript scr)
parse_call args = return $ if null args then CallInteractive else CallArray args

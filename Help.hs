module Help where

short_usage :: String
short_usage = "\
\\ngitadmin invocation:\n\
\\n\
\    gitadmin --help\n\
\    gitadmin --init\n\
\    gitadmin [-u USER] ARGS...\n\
\    gitadmin [-u USER]\n\
\    gitadmin [-u USER] -c COMMAND\n\
\    SSH_ORIGINAL_CMD=GITCMD gitadmin [-u USER]\n"

long_usage :: String
long_usage = short_usage ++ "\nUsage:\n\
\\n\
\Valid gitadmin COMMANDs can be listed with \"gitadmin info\"\n\
\\n\
\In the first case, print this help message.\n\
\\n\
\In the second case, initialize a new gitadmin environment. gitadmin will abort\n\
\if an existing environment is found.\n\
\\n\
\In the third case, the argument list is interpreted as a single gitadmin\n\
\command. The arguments must form a pre-split valid gitadmin command. For\n\
\example, a valid gitadmin invocation from sh would be\n\
\\n\
\    $ gitadmin create_user john \"John Doe\"\n\
\\n\
\In the fourth case, run the gitadmin shell interpreter reading commands from\n\
\stdin. The shell is run in interactive mode if all standard streams are\n\
\connected to a tty, and otherwise in noninteractive mode. If -u USER is given,\n\
\run the shell as this gitadmin user. Commands are read line by line, and split\n\
\into words with a simple algorithm involving \" as quote character and \\ as\n\
\escape character, similar to the sh quoting algorithm. Example: This\\ is\" all\n\
\one \"word. Example gitadmin command line:\n\
\\n\
\    create_repo john testrepo \"John's testrepo in the \\\"john\\\" domain\"\n\
\\n\
\In the fifth case, run the gitadmin shell in noninteractive mode over COMMAND\n\
\which must be a single argument. Example gitadmin invocation from sh:\n\
\\n\
\    $ gitadmin -u john -c 'set_perm john testrepo jane RW'\n\
\\n\
\In the sixth case, validate the environment variable SSH_ORIGINAL_CMD as an\n\
\allowed git request (git-receive-pack 'DOMAIN/REPO' or git-uploadpack\n\
\'DOMAIN/REPO') and execute it.\n"

show_help :: IO ()
show_help = putStr long_usage

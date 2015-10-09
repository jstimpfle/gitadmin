# gitadmin
Lightweight git authorization layer / repository administration tool

- for small or mid-size groups
- only 1200 python3 lines
- maintains state in an sqlite3 database
- users manage their ssh keys and their git repos
  through an interactive admin shell
- repos are organized in domains, much like github
- each domain has a set of domain administrators
  who can modify the repos inside it or change their
  read/commit permissions.
- per repo/per user permissions (R/RW/none) for now
- the only bloat might be the guests feature: users
  can add other users with prefix guest-. (this tool
  was written with university system administrators
  in mind).

Test the shell by ssh'ing testgitadmin@jstimpfle.de: If you login with password
"testgitadmin", you'll get gitadmin root shell. If you create a gitadmin user,
register an ssh public key for that user ("su USER add\_sshkey") and login
again using that key, you'll get a gitadmin shell as that user. And you can
also use that ssh login for git interactions such as clone / pull / push.

This isn't a stable version yet. If you are interested in improvements, help
testing and get in touch. I'd love to use a plain text database instead and
make it more modular, but some experience is missing and size matters...

gitadmin was written in 2015 by Jens Stimpfle, with some support from Ahmet
Inan and Philipp Falk.

License: Public domain

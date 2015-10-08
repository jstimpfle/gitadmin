# gitadmin
Lightweight git authorization layer / repository administration tool

- for small or mid-size groups
- only 1200 python3 lines
- maintains state in an sqlite3 database
- users manage their ssh keys and their git repos
  through an interactive active admin shell
- repos are organized in domains, much like github
- each domain has a set of domain administrators
  who can modify the repos inside it or change their
  read/commit permissions.
- per repo/per user permissions (R/RW/none) for now
- the only bloat might be the guests feature: users
  can add other users with prefix guest-. (this tool
  was written with university system administrators
  in mind).

This isn't a stable version yet. If you are interested in improvements, help
testing and get in touch. I'd love to use a plain text database instead and
make it more modular, but some experience is missing and size matters...

gitadmin was written in 2015 by Jens Stimpfle, with some support from Ahmet
Inan and Philipp Falk.

License: Public domain

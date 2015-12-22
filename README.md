# gitadmin
Lightweight git authorization layer / repository administration tool

- for small or mid-size groups
- maintains state in an sqlite3 database
- users manage their ssh keys and their git repos through an admin shell
  (scriptable / interactive)
- repos are organized in domains, much like github
- each domain has a set of domain administrators who can modify the repos
  inside it or change their read/commit permissions.
- per repo/per user permissions (r/rw) for now

This isn't a stable version yet. If you are interested in improvements, get in
touch and help testing.

gitadmin was written in 2015 by Jens Stimpfle, with some support from
Ahmet Inan and Philipp Falk.

License: Just do what you want with it.

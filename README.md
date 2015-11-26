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
- the only bloat feature might be the following: users can add
  other users with prefix "guest-". (it was written also with
  university system administrators in mind).

You can the shell by ssh'ing testgitadmin@jstimpfle.de: If you login
with password "testgitadmin", you'll get a gitadmin root shell. If
you create a gitadmin user there (create-user foouser "Foo User"),
and register an ssh public key for that user (su foouser add-sshkey),
and login again using that key, you'll get a gitadmin shell as that
user. And you can also use that ssh login for git interactions such
as git clone / git pull / git push.

This isn't a stable version yet. If you are interested in improvements,
get in touch and help testing. I'd love to use a plain text database
and make it more modular, but size matters and some experience is
missing...

gitadmin was written in 2015 by Jens Stimpfle, with some support from
Ahmet Inan and Philipp Falk.

License: Just do what you want with it.

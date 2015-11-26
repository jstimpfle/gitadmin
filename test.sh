#!/bin/sh

set -e
set -u

red() {
    	printf '\033[1;31m%s\033[0m' "$1"
}

green() {
    	printf '\033[1;32m%s\033[0m' "$1"
}

expect_succeed() {
	if "$@" ; then
		green "[ OK ]"
		printf " (succeeded as expected) "
	else
		red "[ ERROR ]"
		printf " (should have succeeded) "
	fi
	echo "$@"
}

expect_fail() {
	if "$@" ; then
		red "[ ERROR ]"
		printf " (should have failed) "
	else
		green "[ OK ]"
		printf " (failed as expected) "
	fi
	echo "$@"
}

expect_gitadmin_errcode() {
	local errcode out r
	errcode=$1 ; shift

	r=0
	out=$({ "$@"; } 3>&2 2>&1 >&3 3>&-) 2>&1 || r=$?
	printf '%s\n' "$out"

	if [ "$r" -eq 0 ]; then
		red "[ ERROR ]"
		printf " (succeed while error $errcode was expected) "
	elif [ -z "${out##gitadmin: ERROR: "$errcode"*}" ] ; then
		green "[ OK ]"
		printf " ($errcode as expected) "
	else
		red "[ ERROR ]"
		printf " (unrecognized error while $errcode was expected) "
	fi
	echo "$@"
}

./gitadmin --init

mykey="ssh-rsa AAAAFOOBARFOOBARFOOBARFOOBARFOOBARFOOBAR test@test"

#gitadmin < testscript
expect_succeed ./gitadmin create-user john "John Doe"
expect_succeed ./gitadmin create-domain john "stupid description"
expect_succeed ./gitadmin set-domain-desc john "John's domain"
expect_succeed ./gitadmin set-admin john john true
expect_succeed ./gitadmin create-repo john john-test "stupid description"
expect_succeed ./gitadmin set-repo-desc john john-test "John's repo"
expect_succeed ./gitadmin set-perm john john-test john rw
expect_succeed ./gitadmin su john list-own-perms
expect_succeed ./gitadmin set-perm john john-test john none
expect_succeed ./gitadmin su john list-own-perms
expect_succeed ./gitadmin su john add-own-sshkey $mykey
expect_succeed ./gitadmin su john remove-own-sshkey $mykey

expect_gitadmin_errcode FAILED-DEPENDENCIES ./gitadmin delete-domain john
expect_succeed ./gitadmin delete-repo john john-test
expect_succeed ./gitadmin delete-domain john

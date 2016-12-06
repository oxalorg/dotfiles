#!/bin/sh

# This command outputs a list of all packages 
# 'intentionally installed' (not as dependecies)
# by 'apt' commands.

echo $1

( zcat $( ls -tr /var/log/apt/history.log*.gz ) ; \
cat /var/log/apt/history.log ) | egrep '^(Start-Date:|Commandline:)' | grep -v aptdaemon | egrep '^Commandline:'

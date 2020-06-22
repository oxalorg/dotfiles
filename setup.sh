#!/bin/bash

DOTDIR=`pwd`

function exit_if_failed{
    echo "# ERROR # Exiting.."
    echo 1>&2 "failed with $?"
    exit 1
}

for f in */setup.sh; do
    bash $f | exit_if_failed
done

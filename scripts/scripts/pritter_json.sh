#!/bin/sh
DATAFILE="/home/mitesh/Dropbox/Data/pritter.json";

# Takes input from the shell
read input;

# Escapes newlines for json data
input=`awk -v var="$input" 'BEGIN{print var }' ORS='\\n'`;

# Creates meta-data
datevar=`date +%s`;

# Check 


# Adds to json file
cat << EOFOE >> $DATAFILE
{
    "date": "$datevar",
    "body": "$input"
}$endchar
EOFOE


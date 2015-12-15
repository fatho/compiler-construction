#!/usr/bin/bash

if [ -z "$1" ]
  then
    echo "usage: $0 <tdiag-file>"
    exit 1
  else
    FILE=$1
fi

cat $FILE | ./dist/build/parse-hm-to-anf-to-cr/parse-hm-to-anf-to-cr

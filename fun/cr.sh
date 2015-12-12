#!/usr/bin/bash

if [ -z "$1" ]
  then
    echo "usage: $0 <tdiag-file>"
    exit 1
  else
    FILE=$1
fi

cat $FILE | ./dist/build/parse-hm/parse-hm | ./dist/build/hm-to-anf/hm-to-anf | 
	./dist/build/hm-to-cr/hm-to-cr | ./dist/build/pp-core/pp-core
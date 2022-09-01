#!/bin/sh

curl https://raw.githubusercontent.com/rdeits/cryptics/master/raw_data/UKACD.txt |
tail -n +26 | tr A-Z a-z |
sed 's/\s//g' | sed "s/'//g" | sed "s/-//g" | sed "s/\///g" | sed "s/\!//g" | sed "s/?//g" |sed "s/,//g" |sed "s/;//g" |sed "s/\.//g" |
grep -P '^[[:ascii:]]*$' | 
sed -nr '/^.{0,30}$/p' |
awk '{ print length, $0 }' | sort -n -s | cut -d" " -f2-

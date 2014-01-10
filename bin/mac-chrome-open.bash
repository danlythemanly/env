#!/bin/bash

URL=`cat /dev/stdin | xxd -plain | tr -d '\n' | sed 's/\(..\)/%\1/g'`
URL="https://www.google.com/search?q=$URL" 
open -a 'Google Chrome' $URL


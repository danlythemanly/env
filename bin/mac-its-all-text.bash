#!/bin/bash

if [ ! -f "$1" ]; then
  touch "$1"
fi

xattr -d com.apple.quarantine "$1" 

open -a 'Terminal' "`emacsclient -e \"(find-file \\\"$1\\\")\"`"

exit 0



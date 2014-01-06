#!/bin/bash
# ./xfce-term-font.bash < inc | dec >

CONFIGFILE="/home/djwillia/.config/xfce4/terminal/terminalrc"

S=`cat $CONFIGFILE | grep "FontName" | grep -o "[0-9]*$"`

if [[ $1 == "inc" ]]; then
    T=$((S+1))
elif [[ $1 == "dec" ]]; then
    T=$((S-1))
fi

EXPR="s/\\(FontName.*\\)$S/\1$T/"
sed -i "$EXPR" $CONFIGFILE


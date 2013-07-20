#!/bin/bash

source $(dirname $0)/config.sh

FREE=`df -h | grep "$1" | awk '{gsub(/%/,""); print $5}'`
USED=`df -h | grep "$1" | awk '{gsub(/%/,""); print $3}'`
TOTAL=`df -h | grep "$1" | awk '{gsub(/%/,""); print $2}'`

ICON="diskette.xbm"
if [[ $FREE -gt 75 ]]; then
    PERCBAR=`echo -e "$FREE"\
        | gdbar -bg $bar_bg -fg $warning -h 1 -w 130`
else
    PERCBAR=`echo -e "$FREE"\
        | gdbar -bg $bar_bg -fg $bar_fg -h 1 -w 130`
fi

ICON='^i(/home/sunn/.xmonad/dzen2/'"$ICON)"
echo "^fg($white0)$ICON^fg() $1 $PERCBAR ^fg($highlight)$USED^fg() / $TOTAL"

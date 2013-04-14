#!/bin/bash
source $(dirname $0)/config.sh
XPOS=1250
WIDTH="120"
LINES="12"

totaldays=$(date +"%j")
totalweeks=$(date +"%U")
time=$(TZ="America/New_York" date | awk -F " " '{print $4}')
calendar=$(cal -1)
date=$(date | cut -d' ' -f1,2,3,6)
timealivesecs=$(date -d 1990-09-26 +%s)
timealivedays=$(( $timealivesecs / 86400 ))

(echo " "; echo "   ^fg($highlight)$date"; echo " "; echo "$calendar"; echo " "; echo "^fg($highlight) ^ca(1,/home/sunn/.xmonad/scripts/dzen_date_prev.sh)PREV     ^ca()^ca(1,/home/sunn/.xmonad/scripts/dzen_date_next.sh)      NEXT^ca()"; sleep 15) | dzen2 -fg $foreground -bg $background -fn $FONT -x $XPOS -y $YPOS -w $WIDTH -l $LINES -e 'onstart=uncollapse,hide;button1=exit;button3=exit'

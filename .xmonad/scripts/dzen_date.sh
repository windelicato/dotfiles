#!/bin/bash
source $(dirname $0)/config.sh
XPOS=$((1250 + $XOFFSET))
WIDTH="110"
LINES="12"

#totaldays=$(date +"%j")
#totalweeks=$(date +"%U")
#timealivesecs=$(date -d 1990-09-26 +%s)
#timealivedays=$(( $timealivesecs / 86400 ))

time=$(TZ="America/New_York" date | awk -F " " '{print $4}')
calendar=$(cal -1)
datea=$(date +%a)
dateb=$(date +%b)
dated=$(date +%d)
datey=$(date +%Y)

(echo " "; echo "  ^fg($highlight)$datea $dateb $dated $datey"; echo " "; echo "$calendar"; echo " "; echo "^fg($highlight) ^ca(1,/home/sunn/.xmonad/scripts/dzen_date_prev.sh)PREV     ^ca()^ca(1,/home/sunn/.xmonad/scripts/dzen_date_next.sh)      NEXT^ca()"; sleep 15) | dzen2 -fg $foreground -bg $background -fn $FONT -x $XPOS -y $YPOS -w $WIDTH -l $LINES -e 'onstart=uncollapse,hide;button1=exit;button3=exit'

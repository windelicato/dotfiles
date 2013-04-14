#!/bin/bash
source $(dirname $0)/config.sh
XPOS=1250
WIDTH="110"
LINES="10"

date=$(date | cut -d' ' -f1,2,3,6)
calendar=$(cal $(( $(date +%m) + 1 )) $(date +%Y))

(echo " "; echo "   ^fg($highlight)$date"; echo ""; echo "$calendar"; sleep 15) | dzen2 -fg $foreground -bg $background -fn $FONT -x $XPOS -y $YPOS -w $WIDTH -l $LINES -e 'onstart=uncollapse,hide;button1=exit;button3=exit'

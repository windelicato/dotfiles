#!/bin/bash
source $(dirname $0)/config.sh
XPOS=$((866 + $XOFFSET))
WIDTH="500"
LINES="56"


date=$(date --rfc-3339=date)
dmesg=$(dmesg | tail -n25 | cut -b16-)
journal=$(journalctl --no-pager -q --since=$date | tail -n25)

(echo " ^fg($highlight)Logs"; echo "^fg($highlight)USER "; echo " "; echo "^fg()$dmesg"; echo " "; echo "^fg($highlight)SYSTEM "; echo " "; echo "$journal"; sleep 15) | dzen2 -fg $foreground -bg  $background -fn $FONT -x $XPOS -y $YPOS -w $WIDTH -l $LINES -e 'onstart=uncollapse,hide;button1=exit;button3=exit'

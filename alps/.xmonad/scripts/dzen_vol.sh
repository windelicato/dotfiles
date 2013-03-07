#!/bin/bash
source $(dirname $0)/config.sh
XPOS=886
WIDTH="15"
LINES="13"

vol=$(amixer get Master | egrep -o "[0-9]+%" | head -1 | egrep -o "[0-9]*")
val=()

for i in {0..100..10}; do
	if [ $vol -gt $i ]
	then
		val+=("^bg($white0)^ca(1,amixer set Master $i)      ^ca()")	
	else
		val+=("^bg(#000000)^ca(1,amixer set Master $i)      ^ca()")
	fi
done

(echo "Volume"; echo " +"; echo "${val[10]}"; echo "${val[9]}"; echo "${val[8]}"; echo "${val[7]}"; echo "${val[6]}"; echo "${val[5]}"; echo "${val[4]}"; echo "${val[3]}"; echo "${val[2]}"; echo "${val[1]}"; echo "${val[0]}"; echo " -";sleep 15) | dzen2 -fg $foreground -bg $background -fn $FONT -x $XPOS -y $YPOS -w $WIDTH -l $LINES -e 'onstart=uncollapse,hide;button1=exit;button3=exit'

#!/bin/bash
#
# (c) Ippytraxx 2012 v.szolnoky@tele2.se
#
#Colours
background="#000000"
foreground="#ffffff"
black="#b3b3b3"
red="#EA8484"
green="#C7F09F"
yellow="#FFCC9A"
blue="#A5CAEF"
magenta="#A6A6DE"
cyan="#95CDCD"
white="#d7d7d7"
grey="#848484"
pink="#FFA4E5"

FONT="-*-lime-*-*-*-*-*-*-*-*-*-*-*-*"
#XPOS=$(xdotool getmouselocation | awk -F " " '{print $1}' | cut -d ":" -f 2)
XPOS=886
YPOS="14"
HEIGHT="15"
WIDTH="15"
LINES="13"

vol=$(amixer get Master | egrep -o "[0-9]+%" | head -1 | egrep -o "[0-9]*")
val=()

for i in {0..100..10}; do
	if [ $vol -gt $i ]
	then
		val+=("^bg(#6e98b8)^ca(1,amixer set Master $i)      ^ca()")	
	else
		val+=("^bg(#000000)^ca(1,amixer set Master $i)      ^ca()")
	fi
done

(echo "Volume"; echo " +"; echo "${val[10]}"; echo "${val[9]}"; echo "${val[8]}"; echo "${val[7]}"; echo "${val[6]}"; echo "${val[5]}"; echo "${val[4]}"; echo "${val[3]}"; echo "${val[2]}"; echo "${val[1]}"; echo "${val[0]}"; echo " -";sleep 15) | dzen2 -bg $background -fn $FONT -x $XPOS -y $YPOS -w $WIDTH -l $LINES -e 'onstart=uncollapse,hide;button1=exit;button3=exit'

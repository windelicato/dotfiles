#!/bin/bash
#
# (c) Ippytraxx 2012 v.szolnoky@tele2.se
#
#Colours
foreground="#FFFFFF"
background="#000000"
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
XPOS=$(xdotool getmouselocation | awk -F " " '{print $1}' | cut -d ":" -f 2)
YPOS="14"
HEIGHT="44"
WIDTH="500"
LINES="56"


date=$(date --rfc-3339=date)
dmesg=$(dmesg | tail -n25 | cut -b16-)
journal=$(journalctl --no-pager --since=$date | tail -n25)

(echo " ^fg($foreground)Logs"; echo "^fg($foreground)USER "; echo " "; echo "^fg()$dmesg"; echo " "; echo "^fg($foreground)SYSTEM "; echo " "; echo "$journal"; sleep 15) | dzen2 -bg  $background -fn $FONT -x $XPOS -y $YPOS -w $WIDTH -l $LINES -e 'onstart=uncollapse,hide;button1=exit;button3=exit'

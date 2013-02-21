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
#XPOS=$(xdotool getmouselocation | awk -F " " '{print $1}' | cut -d ":" -f 2)
XPOS=1040
YPOS="14"
HEIGHT="15"
WIDTH="75"
LINES="2"


battime=$(acpi -b | sed -n "1p" | awk -F " " '{print $5}')
batperc=$(acpi -b | sed -n "1p" | awk -F " " '{print $4}' | head -c3)
batstatus=$(acpi -b | cut -d',' -f1 | awk -F " " '{print $3}')

(echo " ^fg($foreground)Battery"; echo " ^fg()$batstatus"; echo " ^fg($foreground)$battime ^fg()left"; sleep 15) | dzen2 -bg  $background -fn $FONT -x $XPOS -y $YPOS -w $WIDTH -l $LINES -e 'onstart=uncollapse,hide;button1=exit;button3=exit'

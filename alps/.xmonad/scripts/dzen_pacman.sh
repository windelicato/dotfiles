#!/bin/bash
#
# (c) Ippytraxx 2012 v.szolnoky@tele2.se
#
#Colours
background="#000000"
foreground="#FFFFFF"
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
XPOS="1200"
YPOS="14"
HEIGHT="15"
WIDTH="150"
pacmanlines=$(pacman -Qu | wc -l)
LINES=$(( $pacmanlines + 2 ))

updates=$(pacman -Qu)

(echo "^fg($white)Updates"; echo "$updates"; echo " "; echo "^fg($white)Right click to update";sleep 15) | dzen2 -bg $background -fn $FONT -x $XPOS -y $YPOS -w $WIDTH -l $LINES -e 'onstart=uncollapse,hide;button1=exit;button3=exit'

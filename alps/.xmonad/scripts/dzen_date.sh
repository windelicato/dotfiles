#!/bin/bash
#
# (c) Ippytraxx 2012 v.szolnoky@tele2.se
#
#Colours
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
HEIGHT="15"
WIDTH="325"
LINES="13"

totaldays=$(date +"%j")
totalweeks=$(date +"%U")
sydneytime=$(TZ="America/New_York" date | awk -F " " '{print $4}')
calendar=$(cal -3)
timealivesecs=$(date -d 1990-09-26 +%s)
timealivedays=$(( $timealivesecs / 86400 ))

(echo ""; echo "$calendar"; echo " "; echo "Time in New York: ^fg($white)$sydneytime"; echo "Day of the year: ^fg($white)$totaldays"; echo "Week of the year: ^fg($white)$totalweeks"; echo "You have been alive for: ^fg($white)$timealivesecs seconds or $timealivedays days"; sleep 3) | dzen2 -bg $background -fn $FONT -x $XPOS -y $YPOS -w $WIDTH -l $LINES -e 'onstart=uncollapse,hide;button1=exit;button3=exit'

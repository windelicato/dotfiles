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
XPOS=$(xdotool getmouselocation | awk -F " " '{print $1}' | cut -d ":" -f 2)
YPOS="14"
HEIGHT="0"
WIDTH="150"
LINES="5"

dailyrx=$(vnstat --oneline | awk -F ";" '{print $4}')
dailytx=$(vnstat --oneline | awk -F ";" '{print $5}')
alltimerx=$(vnstat --oneline | awk -F ";" '{print $13}')
alltimetx=$(vnstat --oneline | awk -F ";" '{print $14}')

externalip=$(curl -silent http://www.formyip.com/ | grep "Your IP is " | awk -F " " '{print $6}' | cut -d "<" -f 1)

(echo "^fg($white)Network"; echo "Daily up: ^fg($white)$dailytx"; echo "Daily down: ^fg($white)$dailyrx"; echo "Alltime up: ^fg($white)$alltimetx"; echo "Alltime down: ^fg($white)$alltimerx"; echo "External IP: ^fg($white)$externalip"; sleep 3) | dzen2 -bg $background -fn $FONT -x $XPOS -y $YPOS -w $WIDTH -l $LINES -e 'onstart=uncollapse,hide;button1=exit;button3=exit'

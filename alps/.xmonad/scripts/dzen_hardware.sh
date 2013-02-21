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
XPOS="1100"
YPOS="14"
HEIGHT="15"
WIDTH="260"
LINES="26"

cputemp=$(sensors | grep "temp1" | cut -d'+' -f2 | head -c2)F
cpuutiluser=$(iostat -c | sed -n "4p" | awk -F " " '{print $1}')
cpuutilsystem=$(iostat -c | sed -n "4p" | awk -F " " '{print $3}')
cpuutilidle=$(iostat -c | sed -n "4p" | awk -F " " '{print $6}')
ramtotal=$(free -m | sed -n "2p" | awk -F " " '{print $2}')
ramused=$(free -m | sed -n "2p" | awk -F " " '{print $3}')

uptime=$(uptime | sed -n "1p" | awk -F "," '{print $1}')
kernel=$(uname -r)
packages=$(pacman -Q | wc -l)

hdd1=$(df -h | grep "/dev/sd" | sed -n "1p")
hdd2=$(df -h | grep "/dev/sd" | sed -n "2p")
hdd3=$(df -h | grep "/dev/sd" | sed -n "3p")

hddtitle=$(df -h | head -1)
hddtotal=$(df -h --total | tail -1)
toptitle=$(top -bn1 | grep PID | cut -b1-5,42-) 
top=$(top -bn1 | tail -n+8 | sort -k9nr -k10nr | cut -b1-5,42- | grep -v "chromium" | head -n8)

(echo " ^fg($foreground)System"; echo " Temp: ^fg($foreground)$cputemp"; echo " CPU User : ^fg($foreground)$cpuutiluser %"; echo " CPU System : ^fg($foreground)$cpuutilsystem %"; echo " Idle: ^fg($foreground)$cpuutilidle %"; echo " RAM: ^fg($foreground)$ramused MB / $ramtotal MB"; echo " "; echo " Uptime: ^fg($foreground)$uptime"; echo " Kernel: ^fg($foreground)$kernel"; echo " Packages: ^fg($foreground)$packages"; echo " ";  echo "^fg($foreground)$toptitle"; echo "$top"; echo " ";echo " ^fg($foreground)$hddtitle"; echo " $hdd1"; echo " $hdd2"; echo " $hdd3"; echo " $hddtotal"; sleep 10) | dzen2 -bg  $background -fn $FONT -x $XPOS -y $YPOS -w $WIDTH -l $LINES -e 'onstart=uncollapse,hide;button1=exit;button3=exit'

#!/bin/bash
source $(dirname $0)/config.sh
XPOS="1100"
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

(echo " ^fg($highlight)System"; echo " Temp: ^fg($highlight)$cputemp"; echo " CPU User : ^fg($highlight)$cpuutiluser %"; echo " CPU System : ^fg($highlight)$cpuutilsystem %"; echo " Idle: ^fg($highlight)$cpuutilidle %"; echo " RAM: ^fg($highlight)$ramused MB / $ramtotal MB"; echo " "; echo " Uptime: ^fg($highlight)$uptime"; echo " Kernel: ^fg($highlight)$kernel"; echo " Packages: ^fg($highlight)$packages"; echo " ";  echo "^fg($highlight)$toptitle"; echo "$top"; echo " ";echo " ^fg($highlight)$hddtitle"; echo " $hdd1"; echo " $hdd2"; echo " $hdd3"; echo " $hddtotal"; sleep 10) | dzen2 -fg $foreground -bg  $background -fn $FONT -x $XPOS -y $YPOS -w $WIDTH -l $LINES -e 'onstart=uncollapse,hide;button1=exit;button3=exit'

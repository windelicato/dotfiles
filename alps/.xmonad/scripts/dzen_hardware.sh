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
HEIGHT="15"
WIDTH="250"
LINES="24"

cputemp=$(sensors | grep "temp1" | cut -d'+' -f2 | head -c2)F
cpuutiluser=$(iostat -c | sed -n "4p" | awk -F " " '{print $1}')
cpuutilsystem=$(iostat -c | sed -n "4p" | awk -F " " '{print $3}')
cpuutilidle=$(iostat -c | sed -n "4p" | awk -F " " '{print $6}')
ramtotal=$(free -m | sed -n "2p" | awk -F " " '{print $2}')
ramused=$(free -m | sed -n "2p" | awk -F " " '{print $3}')

battime=$(acpi -b | sed -n "1p" | awk -F " " '{print $5}')
batperc=$(acpi -b | sed -n "1p" | awk -F " " '{print $4}' | head -c3)
batstatus=$(acpi -b | cut -d',' -f1 | awk -F " " '{print $3}')

hdd1=$(df -h | grep "/dev/sd" | sed -n "1p")
hdd2=$(df -h | grep "/dev/sd" | sed -n "2p")
hdd3=$(df -h | grep "/dev/sd" | sed -n "3p")
hdd4=$(df -h | grep "/dev/sd" | sed -n "4p")
hdd5=$(df -h | grep "/dev/sd" | sed -n "5p")
hdd6=$(df -h | grep "/dev/sd" | sed -n "6p")

hddtitle=$(df -h | head -1)
hddtotal=$(df -h --total | tail -1)

(echo "^fg($white)System"; echo "[CPU]"; echo "Temp: ^fg($white)$cputemp"; echo "User utilisation: ^fg($white)$cpuutiluser %"; echo "System utilisation: ^fg($white)$cpuutilsystem %"; echo "Idle: ^fg($white)$cpuutilidle %"; echo " "; echo "[RAM]"; echo "Used: ^fg($white)$ramused MB / $ramtotal MB"; echo " "; echo " "; echo "[BATTERY]"; echo "Status: ^fg($white)$batstatus"; echo "Percent: ^fg($white)$batperc";  echo "Time: ^fg($white)$battime"; echo " "; echo "[HDD]"; echo "^fg($white)$hddtitle"; echo "$hdd1"; echo "$hdd2"; echo "$hdd3"; echo "$hdd4"; echo "$hdd5"; echo "$hdd6"; echo "$hddtotal"; sleep 3) | dzen2 -bg  $background -fn $FONT -x $XPOS -y $YPOS -w $WIDTH -l $LINES -e 'onstart=uncollapse,hide;button1=exit;button3=exit'

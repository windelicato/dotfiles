#!/bin/bash
source $(dirname $0)/config.sh
XPOS="938"
WIDTH="200"
LINES="13"

dailyrx=$(vnstat --oneline | awk -F ";" '{print $4}')
dailytx=$(vnstat --oneline | awk -F ";" '{print $5}')
alltimerx=$(vnstat --oneline | awk -F ";" '{print $13}')
alltimetx=$(vnstat --oneline | awk -F ";" '{print $14}')


essid=$(iwconfig wlan0 | sed -n "1p" | awk -F '"' '{print $2}')
mode=$(iwconfig wlan0 | sed -n "1p" | awk -F " " '{print $3}')
freq=$(iwconfig wlan0 | sed -n "2p" | awk -F " " '{print $2}' | cut -d":" -f2)
mac=$(iwconfig wlan0 | sed -n "2p" | awk -F " " '{print $6}')
qual=$(iwconfig wlan0 | sed -n "6p" | awk -F " " '{print $2}' | cut -d"=" -f2)
lvl=$(iwconfig wlan0 | sed -n "6p" | awk -F " " '{print $4}' | cut -d"=" -f2)
rate=$(iwconfig wlan0 | sed -n "3p" | awk -F "=" '{print $2}' | cut -d" " -f1)
inet=$(ifconfig wlan0 | sed -n "2p" | awk -F " " '{print $2}')
netmask=$(ifconfig wlan0 | sed -n "2p" | awk -F " " '{print $4}')
broadcast=$(ifconfig wlan0 | sed -n "2p" | awk -F " " '{print $6}')

#externalip=$(curl -silent http://www.formyip.com/ | grep "Your IP is " | awk -F " " '{print $6}' | cut -d "<" -f 1)

(echo " ^fg($white)Network"; echo " ^fg($highlight)$essid"; echo " Freq: ^fg($highlight)$freq GHz ^fg() Band: ^fg($highlight)$mode"; echo " Down: ^fg($highlight)$rate MB/s  ^fg() Quality: ^fg($highlight)$qual"; echo " IP: ^fg($white)$inet";  echo " Netmask: ^fg($white)$netmask";  echo " Broadcast: ^fg($white)$broadcast"; echo " MAC: ^fg($highlight)$mac";  echo " "; echo " Daily up: ^fg($white)$dailytx"; echo " Daily down: ^fg($white)$dailyrx"; echo " Alltime up: ^fg($white)$alltimetx"; echo " Alltime down: ^fg($white)$alltimerx"; sleep 10) | dzen2 -fg $foreground -bg $background -fn $FONT -x $XPOS -y $YPOS -w $WIDTH -l $LINES -e 'onstart=uncollapse,hide;button1=exit;button3=exit'

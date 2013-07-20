#!/bin/bash
source $(dirname $0)/config.sh
XPOS=$((1190 + $XOFFSET))
WIDTH="82"
LINES="2"


battime=$(acpi -b | sed -n "1p" | awk -F " " '{print $5}')
batperc=$(acpi -b | sed -n "1p" | awk -F " " '{print $4}' | head -c3)
batstatus=$(acpi -b | cut -d',' -f1 | awk -F " " '{print $3}')

(echo " ^fg($highlight)Battery"; echo "  ^fg()$batstatus"; echo "  ^fg($highlight)$battime ^fg()left"; sleep 15) | dzen2 -fg $foreground -bg  $background -fn $FONT -x $XPOS -y $YPOS -w $WIDTH -l $LINES -e 'onstart=uncollapse,hide;button1=exit;button3=exit'

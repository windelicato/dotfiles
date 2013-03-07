#!/bin/bash
source $(dirname $0)/config.sh

#XPOS=$(xdotool getmouselocation | awk -F " " '{print $1}' | cut -d ":" -f 2)
XPOS="750"
YPOS="11"
HEIGHT="12"
WIDTH="375"

url=http://www.last.fm/music/$(mpc current -f %artist% | sed 's/[ \/]/+/g')
urlalbum=$url/$(mpc current -f %album% | sed 's/[ \/]/+/g')
albumbio=$(wget -qO- $urlalbum | lynx -dump -nolist -stdin  | tail -n+60 | awk ' /About this album/ {flag=1;next} /Edit wiki/{flag=0} flag { print }')
albumbiol=$(wget -qO- $urlalbum | lynx -dump -nolist -stdin  | tail -n+60 | awk ' /About this album/ {flag=1;next} /Edit wiki/{flag=0} flag { print }' | wc -l)
LINES=$(( $albumbiol + 3 ))
if [[ $LINES -lt 4 ]]; then
	albumbio="No Bio Found"
fi

(echo "^fg($highlight)Music Info "; echo "^fg($highlight) ALBUM BIO"; echo "$albumbio"; echo " "; sleep 15) | dzen2 -fg $foreground -bg $background -fn $FONT -x $XPOS -y $YPOS -w $WIDTH -l $LINES -e 'onstart=uncollapse,hide;button1=exit;button3=exit'

#!/bin/bash
source $(dirname $0)/config.sh
XPOS=$((950 + $XOFFSET))
WIDTH="300"

url="http://makeitpersonal.co/lyrics?artist=$(mpc current -f %artist% | sed 's/ /%20/g')&title=$(mpc current -f %title% | sed 's/ /%20/g')"
lyrics=$(curl -s "$url" | fold -sw 60)
lyricslines=$(curl -s "$url" | fold -sw 60 | wc -l)
LINES=$(( $lyricslines + 3 ))

if [[ $LINES -gt 60 ]]; then
	LINES=60
fi

(echo "^fg($highlight)Music Info "; echo "^fg($highlight) Lyrics"; echo "$lyrics"; echo " "; sleep 15) | dzen2 -fg $foreground -bg $background -fn $FONT -x $XPOS -y $YPOS -w $WIDTH -l $LINES -e 'onstart=uncollapse,hide;button1=exit;button3=exit;button4=scrollup;button5=scrolldown;'


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
#XPOS=$(xdotool getmouselocation | awk -F " " '{print $1}' | cut -d ":" -f 2)
XPOS="750"
YPOS="14"
HEIGHT="15"
WIDTH="375"

url=http://www.last.fm/music/$(mpc current -f %artist% | sed 's/[^a-zA-Z0-9]/+/g')
urlalbum=$url/$(mpc current -f %album% | sed 's/[^a-zA-Z0-9]/+/g')
bio=$(wget -qO- $url | lynx -dump -nolist -stdin  | tail -n+100 | awk ' /Biography/ {flag=1;next} /Edit bio/{flag=0} flag { print }')
biol=$(wget -qO- $url | lynx -dump -nolist -stdin  | tail -n+100 | awk ' /Biography/ {flag=1;next} /Edit bio/{flag=0} flag { print }' | wc -l)
LINES=$(( $biol + 3 ))

(echo "^fg($foreground)Music Info "; echo "^fg($foreground)BIOGRAPHY"; echo " "; echo "$bio";  echo " "; sleep 15) | dzen2 -bg $background -fn $FONT -x $XPOS -y $YPOS -w $WIDTH -l $LINES -e 'onstart=uncollapse,hide;button1=exit;button3=exit'

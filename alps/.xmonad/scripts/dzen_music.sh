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
XPOS="856"
YPOS="14"
HEIGHT="15"
WIDTH="250"
LINES="15"

playing=$(mpc current)
stats=$(mpc stats)
playlistcurrent=$(mpc playlist | grep -n "$playing" | cut -d ":" -f 1 | head -n1)
nextnum=$(( $playlistcurrent + 1 ))
prevnum=$(( $playlistcurrent - 1 ))
next=$(mpc playlist | sed -n ""$nextnum"p")
prev=$(mpc playlist | sed -n ""$prevnum"p")
#art=$(ls ~/.config/ario/covers | grep SMALL | grep $album)


(echo "^fg($foreground)Music"; echo "Playing: ^fg($foreground)$playing"; echo " "; echo "             ^ca(1, ncmpcpp prev)  ^fg(#556c85)^i(/home/sunn/.xmonad/dzen2/prev.xbm) ^ca()  ^ca(1, ncmpcpp pause) ^i(/home/sunn/.xmonad/dzen2/pause.xbm) ^ca()  ^ca(1, ncmpcpp play) ^i(/home/sunn/.xmonad/dzen2/play.xbm) ^ca()   ^ca(1, ncmpcpp next) ^i(/home/sunn/.xmonad/dzen2/next.xbm) ^ca()"; echo " "; echo "Next: ^fg($foreground)$next"; echo "Prev: ^fg($foreground)$prev"; echo " ";  echo "$stats"; sleep 15) | dzen2 -bg $background -fn $FONT -x $XPOS -y $YPOS -w $WIDTH -l $LINES -e 'onstart=uncollapse,hide;button1=exit;button3=exit'

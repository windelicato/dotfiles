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
HEIGHT="15"
WIDTH="250"
LINES="12"

playing=$(mpc current)
stats=$(mpc stats)
#playlist=$(mpc playlist | sed "s/$playing/> $playing/")
playlistcurrent=$(mpc playlist | grep -n "$playing" | cut -d ":" -f 1)
nextnum=$(( $playlistcurrent + 1 ))
prevnum=$(( $playlistcurrent - 1 ))
next=$(mpc playlist | sed -n ""$nextnum"p")
prev=$(mpc playlist | sed -n ""$prevnum"p")

(echo "^fg($white)Music"; echo "Playing: ^fg($white)$playing"; echo "Next: ^fg($white)$next"; echo "Previous: ^fg($white)$prev"; echo " "; echo "$stats"; sleep 3) | dzen2 -bg $background -fn $FONT -x $XPOS -y $YPOS -w $WIDTH -l $LINES -e 'onstart=uncollapse,hide;button1=exit;button3=exit'

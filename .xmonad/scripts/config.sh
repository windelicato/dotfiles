#!/bin/bash

background="#1f1f1f"
foreground="#6d715e"
highlight="#c0b18b"

#XPOS=$(xdotool getmouselocation | awk -F " " '{print $1}' | cut -d ":" -f 2)
YPOS="11"
HEIGHT="12"
XOFFSET=554
if [[ -z `xrandr | grep " connected" | grep 'VGA'` ]]; then
	XOFFSET="0"
fi

#FONT="-artwiz-lime-medium-r-normal-*-10-110-75-75-m-50-iso8859-*"
FONT="-*-dweep-medium-r-semicondensed-*-11-*-*-*-*-*-*-*"
#FONT="-artwiz-limey-medium-r-normal-*-10-110-75-75-m-50-iso8859-*"
#FONT="-*-tamsyn-medium-r-normal-*-10-110-75-75-m-50-iso8859-*"
#FONT='-*-lemon-*-*-*-*-*-*-75-75-*-*-*-*'
#FONT="-*-tewi-medium-*-normal-*-*-*-*-*-*-*-*-*"
white0="#775759"

bar_bg="#1f1f1f"
bar_fg="#d17b49"
notify="#d17b49"
warning="#d17b49"

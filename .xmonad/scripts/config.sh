#!/bin/bash

background="#181512"
foreground="#9a875f"
highlight="#bea492"

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
white0="#646a6d"

bar_bg="#181512"
bar_fg="#746C48"
notify="#F39D21"
warning="#8c644c"

#!/bin/bash

background="#181512"
foreground="#8c644c"
highlight="#d6c3b6"

#XPOS=$(xdotool getmouselocation | awk -F " " '{print $1}' | cut -d ":" -f 2)
YPOS="11"
HEIGHT="12"
XOFFSET=554
if [[ -z `xrandr | grep " connected" | grep 'VGA'` ]]; then
	XOFFSET="0"
fi

#FONT="-artwiz-lime-medium-r-normal-*-10-110-75-75-m-50-iso8859-*"
FONT="-artwiz-limey-medium-r-normal-*-10-110-75-75-m-50-iso8859-*"
#FONT="-*-lemon-*-*-*-*-*-*-*-*-*-*-*-*"
white0="#646a6d"

bar_bg="#454545"
bar_fg="#746c48"
notify="#F39D21"
warning="#D23D3D"

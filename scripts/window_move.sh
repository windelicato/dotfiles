#!/bin/bash

# Example sxhkd binding:
#   alt + {a,f}
#       /home/sunn/scripts/window_move.sh {-p,-n} -x -s 20
#   
#   alt + {s,d}
#       /home/sunn/scripts/window_move.sh {-p,-n} -y -s 20


POSITIVE=false
HORIZONTAL=false
SIZE='20'

err() {
	echo "$1"
	exit 1
}

usage() {
	echo "usage: bspwm_resize [direction]"
	echo
	echo "Options:"
	echo "	 -p, --positive	      - resize in positively"
	echo "	 -n, --negative       - resize in negatively"
	echo "	 -x, --xdir           - resize in x direction"
	echo "	 -y, --ydir           - resize in y dir"
	echo "	 -s,                  - number of pixels to resize or move"
	echo "	 -h, --help           - display this"
	exit
}

if [[ $# -eq 0 ]] ; then
	usage
	exit
fi

for i in "$@"; do
	case $i in
		'-p'|'--positive')
			POSITIVE=true
			;;
		'-n'|'--negative')
			POSITIVE=false
			;;
		'-x'|'--xdir')
			HORIZONTAL=true
			;;
		'-y'|'--ydir')
			HORIZONTAL=false
			;;
		'-s')
			SIZE=$(echo $@ | sed 's/.*-s \([0-9]*\).*/\1/')
			[[ "$SIZE" == "$@" ]] && err "Must specify number of pixels"
			;;
		''|'-h'|'--help')
			usage
			exit
			;;
		*)
			;;
	esac
done

# Find current window mode
WINDOW_STATUS="$(bspc query -T -w | awk '/^.* [a-zA-Z\-]{8} \*$/{print $8}')" 
FLAGS="$(echo $WINDOW_STATUS | sed 's/-//g')";

if [[ "$FLAGS" =~ ^.*f.*$ ]]; then
# The window is floating, move it
	$HORIZONTAL && switch="-x" || switch="-y"
	$POSITIVE && sign="-" || sign="+"
	xdo move ${switch} ${sign}${SIZE}
elif [[ "$FLAGS" =~ ^.*d.*$ ]]; then
# The window is pseudotiled, resize it
	$HORIZONTAL && switch="-w" || switch="-h"
	$POSITIVE && sign="+" || sign="-"
	xdo resize ${switch} ${sign}${SIZE}
else
# The window is tiled modified split ratio
	$HORIZONTAL && switch=('left' 'right') || switch=('down' 'up')
	$POSITIVE && sign="+" || sign="-"
	bspc window -e ${switch[0]} ${sign}${SIZE} ||  bspc window -e ${switch[1]} ${sign}${SIZE}
fi

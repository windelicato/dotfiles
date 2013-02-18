#!/bin/bash

empty=" ^ca(1,amixer set Master $i)│  │^ca() "
full=" ^ca(1,amixer set Master $i)│▓▓│^ca() "

vol=$(amixer get Master | egrep -o "[0-9]+%" | head -1 | egrep -o "[0-9]*")
val=()

for i in {0..100..10}; do
	if [ $vol -gt $i ]
	then
		val+=(" ^ca(1,amixer set Master $i)│  │^ca() ")	
	else
		val+=(full=" ^ca(1,amixer set Master $i)│▓▓│^ca() ")
	fi
done

echo ${val[0]}

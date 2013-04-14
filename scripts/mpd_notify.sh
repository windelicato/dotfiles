#!/bin/bash

while : ; do
	mpc idle
	mpc current
	album=$(mpc current -f %album%)
	art=$(ls ~/.config/ario/covers | grep SMALL | grep "$album")
	feh --cycle-once /home/sunn/.config/ario/covers/"$art"
done

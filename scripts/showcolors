#!/bin/sh

xdef=`cat ~/.Xresources | grep "\.colors" | cut -d '"' -f2`

colors=( $( sed -re '/^!/d; /^$/d; /^#/d; s/(\*color)([0-9]):/\10\2:/g;' $xdef | grep 'color[01][0-9]:' | sort | sed  's/^.*: *//g' ) )

echo
for i in {0..7}; do echo -en "\e[$((30+$i))m\u2588\u2588 ${colors[i]} \e[0m"; done
echo
for i in {8..15}; do echo -en "\e[1;$((22+$i))m\u2588\u2588 ${colors[i]} \e[0m"; done
echo -e "\n"

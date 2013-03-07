#!/bin/bash
source $(dirname $0)/config.sh
XPOS="750"
WIDTH="250"
LINES="19"

playing=$(mpc current)
artist=$(mpc current -f  %artist%)
album=$(mpc current -f  %album%)
track=$(mpc current -f  %title%)
stats=$(mpc stats)
playlistcurrent=$(mpc playlist | grep -n "$playing" | cut -d ":" -f 1 | head -n1)
nextnum=$(( $playlistcurrent + 1 ))
prevnum=$(( $playlistcurrent - 1 ))
next=$(mpc playlist | sed -n ""$nextnum"p")
prev=$(mpc playlist | sed -n ""$prevnum"p")
#art=$(ls ~/.config/ario/covers | grep SMALL | grep $album)

(echo "^fg($highlight)Music"; echo "^ca(1,/home/sunn/.xmonad/scripts/dzen_lyrics.sh) Track   ^fg($highlight)$track^ca()"; echo " ^ca(1,/home/sunn/.xmonad/scripts/dzen_artistinfo.sh)^fg()Artist  ^fg($highlight)$artist^ca()";echo " ^ca(1,/home/sunn/.xmonad/scripts/dzen_albuminfo.sh)^fg()Album   ^fg($highlight)$album^ca()";echo " "; echo "             ^ca(1, ncmpcpp prev)  ^fg($white0)^i(/home/sunn/.xmonad/dzen2/prev.xbm) ^ca()  ^ca(1, ncmpcpp pause) ^i(/home/sunn/.xmonad/dzen2/pause.xbm) ^ca()  ^ca(1, ncmpcpp play) ^i(/home/sunn/.xmonad/dzen2/play.xbm) ^ca()   ^ca(1, ncmpcpp next) ^i(/home/sunn/.xmonad/dzen2/next.xbm) ^ca()"; echo " "; echo " Next: ^fg($highlight)$next"; echo " Prev: ^fg($highlight)$prev"; echo " "; echo "$stats"; echo " "; sleep 15) | dzen2 -fg $foreground -bg $background -fn $FONT -x $XPOS -y $YPOS -w $WIDTH -l $LINES -e 'onstart=uncollapse,hide;button1=exit;button3=exit'

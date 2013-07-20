#!/bin/bash
source $(dirname $0)/config.sh

n=$(($1 + 4))
PERC=`mpstat -P ALL 0 | awk '{gsub(//,""); print $4}' | sed -n "$n"p`

ICON="cpu.xbm"
if [[ $PERC > 75 ]]; then
    PERCBAR=`echo -e "$PERC"\
        | gdbar -bg $bar_bg -fg $warning -h 1 -w 180`
else
    PERCBAR=`echo -e "$PERC"\
        | gdbar -bg $bar_bg -fg $bar_fg -h 1 -w 180`
fi

ICON='^i(/home/sunn/.xmonad/dzen2/'"$ICON)"
echo "^fg($white0)$ICON^fg() CPU$1 $PERCBAR ^fg($highlight)$PERC%"

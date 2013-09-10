#!/bin/bash
source $(dirname $0)/config.sh
XPOS=$((1100 + $XOFFSET))
WIDTH="260"
LINES="15"

cputemp="^fg($white0)^i(/home/sunn/.xmonad/dzen2/temp.xbm)^fg() Temp ^fg($highlight)$(sensors | grep "temp1" | cut -d'+' -f2 | head -c2)F"
cpuutiluser=$(iostat -c | sed -n "4p" | awk -F " " '{print $1}')
cpuutilsystem=$(iostat -c | sed -n "4p" | awk -F " " '{print $3}')
cpuutilidle=$(iostat -c | sed -n "4p" | awk -F " " '{print $6}')
ramtotal=$(free -m | sed -n "3p" | awk -F " " '{print $4}')
ramused=$(free -m | sed -n "3p" | awk -F " " '{print $3}')

kernel="^fg($white0)^i(/home/sunn/.xmonad/dzen2/arch_10x10.xbm)^fg() Kernel ^fg($highlight)$(uname -r)"
packages="^fg($white0)^i(/home/sunn/.xmonad/dzen2/pacman.xbm)^fg() Packages ^fg($highlight)$(pacman -Q | wc -l)"
uptime="^fg($white0)^i(/home/sunn/.xmonad/dzen2/net_up_01.xbm)^fg() Uptime ^fg($highlight)$(uptime | awk '{gsub(/,/,""); print $3}')"

hddtitle=$(df -h | head -1)
hddtotal=$(df -h --total | tail -1)
#toptitle=$(top -bn1 | grep PID | cut -b1-5,42-) 
#top=$(top -bn1 | tail -n+8 | sort -k9nr -k10nr | cut -b1-5,42- | grep -v "chromium" | head -n8)

cpu_bar_total=`/home/sunn/.xmonad/scripts/bar_cpu.sh 0`
cpu_bar_0=`/home/sunn/.xmonad/scripts/bar_cpu.sh 1`
cpu_bar_1=`/home/sunn/.xmonad/scripts/bar_cpu.sh 2`
cpu_bar_2=`/home/sunn/.xmonad/scripts/bar_cpu.sh 3`
cpu_bar_3=`/home/sunn/.xmonad/scripts/bar_cpu.sh 4`

mem_bar=`/home/sunn/.xmonad/scripts/bar_ram_lg.sh`

sda_bar=`/home/sunn/.xmonad/scripts/bar_disk.sh /dev/sda`
sdb_bar=`/home/sunn/.xmonad/scripts/bar_disk.sh /dev/sdb`

(echo " ^fg($highlight)System"; echo "              $kernel"; echo "        $packages   $uptime"; echo " "; echo " $cpu_bar_0"; echo " $cpu_bar_1"; echo " $cpu_bar_2"; echo " $cpu_bar_3"; echo "                 $cputemp"; echo " ";  echo " $mem_bar"; echo "                 ^fg($highlight)$ramused MB / $ramtotal MB"; echo " "; echo " $sda_bar"; echo " $sdb_bar"; sleep 10) | dzen2 -fg $foreground -bg  $background -fn $FONT -x $XPOS -y $YPOS -w $WIDTH -l $LINES -e 'onstart=uncollapse,hide;button1=exit;button3=exit'

#!/bin/sh

c00=$'\e[0;30m'
c01=$'\e[0;31m'
c02=$'\e[0;32m'
c03=$'\e[0;33m'
c04=$'\e[0;34m'
c05=$'\e[0;35m'
c06=$'\e[0;36m'
c07=$'\e[0;37m'
c08=$'\e[1;30m'
c09=$'\e[1;31m'
c10=$'\e[1;32m'
c11=$'\e[1;33m'
c12=$'\e[1;34m'
c13=$'\e[1;35m'
c14=$'\e[1;36m'
c15=$'\e[1;37m'

f0=$'\e[1;30m'
f1=$'\e[1;37m'
f2=$'\e[0;37m'

kernel=$(uname -rmo)
cpuspe=$(grep 'model name' /proc/cpuinfo| sed 1q|sed 's/^.*:\ *//')

system=$(cat /etc/os-release | sed '2,$d;s/NAME="//;s/"//')

if [ -n "$DISPLAY" ]; then
    wmname=$(xprop -root _NET_WM_NAME|cut -d\" -f2)
    termfn=$(cat $HOME/.Xresources | grep -v ! | awk '/*font/ {print $2}' | sed 's/xft://;s/:pixelsize//;s/=/\ /')
    systfn=$(sed -n 's/^[gtk].*font.*"\(.*\)".*$/\1/p' ~/.gtkrc-2.0)
else
    wmname="none"
    termfn="none"
    systfn="none"
fi
wmname="bspwm"

pkgnum=$(pacman -Q|wc -l)
birthd=$(sed -n '1s/^\[\([0-9-]*\).*$/\1/p' /var/log/pacman.log | tr - .)

gitdir="github.com/windelicato"
myblog="www.windelicato.com"

cat << EOF

${c00}▉▉  | ${c15}OS ${f0}........... $f2$system
${c08}  ▉▉| ${c15}name ${f0}......... $f2$HOSTNAME
${c01}▉▉  | ${c15}birth day${f0}..... $f2$birthd
${c09}  ▉▉| ${c15}packages ${f0}..... $f2$pkgnum
${c02}▉▉  | 
${c10}  ▉▉| ${c15}wm ${f0}........... $f2$wmname
${c03}▉▉  | ${c15}shell ${f0}........ $f2$SHELL
${c11}  ▉▉| ${c15}terminal ${f0}..... $f2$TERM
${c04}▉▉  | ${c15}term font ${f0}.... $f2$termfn
${c12}  ▉▉| ${c15}system font ${f0}.. $f2$systfn
${c05}▉▉  | 
${c13}  ▉▉| ${c15}kernel ${f0}....... $f2$kernel
${c06}▉▉  | ${c15}processor ${f0}.... $f2$cpuspe
${c14}  ▉▉| 
${c07}▉▉  | ${c15}website ${f0}...... $f2$myblog
${c15}  ▉▉| ${c15}github ${f0}....... $f2$gitdir
EOF

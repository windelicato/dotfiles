xrdb ~/.Xresources 
xset +fp /usr/share/fonts/local 
xset +fp /usr/share/fonts/myfonts/limey 
xset +fp /usr/share/fonts/myfonts/lemon 
xset +fp /usr/share/fonts/myfonts/uushi 
xset +fp /usr/share/fonts/myfonts/suxus 
xset +fp /usr/share/fonts/myfonts/tewi 
xset +fp /usr/share/fonts/artwiz-fonts 
xset +fp /usr/share/fonts/yrmtmodded
xset fp rehash 
wmname LG3D
xcalib /home/sunn/x230.icm &
xsetroot -cursor_name left_ptr &
xmodmap -e 'clear Lock' -e 'keycode 0x42 = Escape' &
hsetroot -tile /home/sunn/Pictures/mountain-tall.jpg &
unclutter -grab -noevents -root &
#compton -cCb -fD 5 -r 5 -o 0.5 -l 2 -t 2 &
#compton -cCfFbG -I 0.05 -O 0.05 -D 4 -r4 -l-6 -t-5 -o.99 &
compton &
urxvtd &

if [[ -z `xrandr | grep " connected" | grep 'VGA'` ]]; then
	echo "No monitor found"
else
	xrandr --output VGA1 --auto --above LVDS1
	nitrogen --restore
fi

DEFAULT_SESSION=xmonad
case $1 in
xmonad)
exec dbus-launch xmonad
;;

openbox)
exec openbox-session
;;

dwm)
#conky -qc /home/sunn/.xmonad/.conky_dzen | dzen2 -x '666' -w '700' -h '12' -ta 'r' -bg '#222222' -fg '#bbbbbb' -y '0' -fn '-*-lime-*-*-*-*-*-*-*-*-*-*-*-*' &
#conky -qc /home/sunn/.xmonad/.conky_dzen | while read -r; do xsetroot -name "$REPLY"; done &
exec dbus-launch dwm-launch
;;

bspwm|*)
sxhkd &
[ -e "$PANEL_FIFO" ] && rm "$PANEL_FIFO"
mkfifo "$PANEL_FIFO"
#exec dbus-launch bspwm -s "$PANEL_FIFO" -p W
exec wm
;;
esac

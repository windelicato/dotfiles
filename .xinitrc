xrdb ~/.Xresources 
xset +fp /usr/share/fonts/local 
xset +fp /usr/share/fonts/myfonts/limey 
xset +fp /usr/share/fonts/myfonts/lemon 
xset +fp /usr/share/fonts/myfonts/uushi 
xset +fp /usr/share/fonts/myfonts/suxus 
xset +fp /usr/share/fonts/artwiz-fonts 
xset fp rehash 
xcalib /home/sunn/x230.icm &
xsetroot -cursor_name left_ptr &
xmodmap -e 'clear Lock' -e 'keycode 0x42 = Escape' &
hsetroot -fill /home/sunn/Pictures/southern-alps.jpg &
unclutter -grab -noevents -root &
#compton -cCb &
urxvtd &

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

bspwm)
sxhkd &
[ -e "$PANEL_FIFO" ] && rm "$PANEL_FIFO"
mkfifo "$PANEL_FIFO"
exec dbus-launch bspwm -c /home/sunn/.config/bspwm/autostart -s "$PANEL_FIFO" -p W
;;

*)
sxhkd &
[ -e "$PANEL_FIFO" ] && rm "$PANEL_FIFO"
mkfifo "$PANEL_FIFO"
exec dbus-launch bspwm -c /home/sunn/.config/bspwm/autostart -s "$PANEL_FIFO" -p W
;;
esac

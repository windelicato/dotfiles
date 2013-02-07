xrdb ~/.Xresources &
xset +fp /usr/share/fonts/local &
xset +fp /usr/share/fonts/artwiz-fonts &
xset fp rehash &
xcalib /home/sunn/x230.icm &
xsetroot -cursor_name left_ptr &
xmodmap -e 'clear Lock' -e 'keycode 0x42 = Escape' &
urxvtd &

DEFAULT_SESSION=xmonad
case $1 in
xmonad)
hsetroot -fill /home/sunn/Pictures/mountain-alps.jpg
exec dbus-launch xmonad
;;
dwm)
hsetroot -fill /home/sunn/Pictures/mountain-alps.jpg &
#conky -qc /home/sunn/.xmonad/.conky_dzen | dzen2 -x '666' -w '700' -h '12' -ta 'r' -bg '#222222' -fg '#bbbbbb' -y '0' -fn '-*-lime-*-*-*-*-*-*-*-*-*-*-*-*' &
conky -qc /home/sunn/.xmonad/.conky_dzen | while read -r; do xsetroot -name "$REPLY"; done &
exec dbus-launch /home/sunn/dwm/startdwm
;;
openbox)
exec dbus-launch openbox-session
;;
*)
exec dbus-launch  $DEFAULT_SESSION
;;
esac

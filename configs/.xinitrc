xrdb ~/.Xresources
xset +fp /usr/share/fonts/local
xset +fp /usr/share/fonts/artwiz-fonts
xset fp rehash
xcalib /home/sunn/x230.icm
xsetroot -cursor_name left_ptr
xmodmap -e 'clear Lock' -e 'keycode 0x42 = Escape'
#compton -cCfb -D 3.5
exec dbus-launch xmonad

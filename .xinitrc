hsetroot -fill /home/sunn/pics/thaw.png
xcalib -d :0 /home/sunn/mythlogic.icm
xrdb ~/.Xresources
xset +fp /usr/share/fonts/local
xset +fp /usr/share/fonts/artwiz-fonts
xset fp rehash
xsetroot -cursor_name left_ptr
exec ck-launch-session dbus-launch xmonad

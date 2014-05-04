xrdb ~/.Xresources 
xset +fp ~/.fonts
xset +fp /usr/share/fonts/local 
xset fp rehash 
wmname LG3D
xcalib /home/sunn/x230.icm &
xsetroot -cursor_name left_ptr &
xmodmap -e 'clear Lock' -e 'keycode 0x42 = Escape' &
hsetroot -tile /home/sunn/Pictures/tinytile2.gif &
unclutter -grab -noevents -root &
urxvtd &

if [[ -z `xrandr | grep " connected" | grep 'VGA'` ]]; then
	echo "No monitor found" &
else
	xrandr --output VGA1 --auto --above LVDS1 &
fi

if [[ "`acpi -b | awk '{gsub(/,/,""); print $3}'`" != "Discharging" ]]; then
	hsetroot -fill /home/sunn/Pictures/forest-birch.jpg &
	compton &
	/home/sunn/scripts/docky-launch &
fi

exec $1

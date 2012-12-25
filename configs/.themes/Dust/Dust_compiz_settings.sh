#!/bin/bash
if [ "$1" == "--help" ]; then
	echo Usage: $0 [OPTIONS]
	echo Adjusts Compiz settings.
	echo 
	echo "  (no args)         Sets the recommended settings."
	echo "  --fade            Changes titlebar to fade."
	echo 
	exit
fi
if [ "$1" == "" ]; then

	zenity --question --text="\<big\>\<b\>Compiz shadow settings for Dust\</b\>\</big\>\nThis script will do the following things:\n\n- Adjust compiz gtk-window-decorator titlebar opacity\n<small>   This will make the titlebar completely opaque. Compiz defaults to it being translucent.</small>\n\n- Adjust compiz 'decorator' plugin's drop shadow\n<small>   This will give windows a large drop shadow.</small>\n\nDo you want to proceed?"
	
	if [ "$?" != "0" ]; then
		zenity --info --text="<big><b>Nothing done</b></big>\nDon't worry."
		exit
	fi

	# Activate the blurring
	gconftool-2 --set --type=string /apps/gwd/blur_type titlebar
	gconftool-2 --set --type=float  /apps/gwd/metacity_theme_opacity 1
	gconftool-2 --set --type=bool   /apps/gwd/metacity_theme_shade_opacity true
	
	gconftool-2 --set --type=float  /apps/gwd/metacity_theme_active_opacity 1
	gconftool-2 --set --type=bool   /apps/gwd/metacity_theme_active_shade_opacity true
	
	# Set the compiz drop shadow
	gconftool-2 --set --type=float  /apps/compiz/plugins/decoration/allscreens/options/shadow_opacity 0.7
	gconftool-2 --set --type=float  /apps/compiz/plugins/decoration/allscreens/options/shadow_radius 25.0
	gconftool-2 --set --type=string /apps/compiz/plugins/decoration/allscreens/options/shadow_color /apps/compiz/plugins/decoration/allscreens/options/shadow_color #000000ff
	gconftool-2 --set --type=int    /apps/compiz/plugins/decoration/allscreens/options/shadow_x_offset 0
	gconftool-2 --set --type=int    /apps/compiz/plugins/decoration/allscreens/options/shadow_y_offset 5	
	gconftool-2 --set --type=string /apps/compiz/plugins/decoration/allscreens/options/command /usr/bin/compiz-decorator

	# Start the GTK window decorator
	if [ -z "`pidof compiz.real`" ]; then echo; else
		gtk-window-decorator --replace 2>/dev/null 1>/dev/null & disown
	fi
	
	zenity --info --text="\<big\>\<b\>Done!\</b\>\</big\>\nYour Compiz's decorator settings have now been adjusted. Try activating Compiz's window decoration plugin (via <i>ccsm</i>) if you did not see any changes."

elif [ "$1" == "--fade" ]; then
	# Running this with the "--fade" option will remove the titlebar fading.
	
	gconftool-2 --set --type=float  /apps/gwd/metacity_theme_opacity 0.85
	gconftool-2 --set --type=bool   /apps/gwd/metacity_theme_shade_opacity true
	
	gconftool-2 --set --type=float  /apps/gwd/metacity_theme_active_opacity 1
	gconftool-2 --set --type=bool   /apps/gwd/metacity_theme_active_shade_opacity true
fi
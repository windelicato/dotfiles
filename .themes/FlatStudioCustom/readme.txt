Author: Rafa Cobreros rafacobreros@gmail.com
License: GPL
Original theme in: http://gnome-look.org/content/show.php/FlatStudio?content=154296

1.- Change selection color
2.- Change style of window titlebar buttons (Metacity and unity)
3.- Select style for nautilus 

NOTE:
	- Changes may require open/close session to take effect


--------------------------------
* 1.- Change selection color   *
--------------------------------
To GTK3 apps
Edit the file ../FlatStudioDark/gtk-3.0/gtk.css
on line 24 assigns the value of desired color
@define-color theme_selected_bg_color 		@selection_acid_green;


for GTK2 apps
Edit the file .. /FlatStudioDark/gtk-2.0/gtkrc
on line 14 assigns the value of desired color
gtk-color-scheme = "selected_bg_color:#xxxxxx"


-------------------------------------------------
* 2.- Change style of window titlebar buttons   *
-------------------------------------------------
If you do not use unity, in most cases it can be enough to select with gnome-tweak-tools windows-theme of the buttons we want. If this is not sufficient, then:

In the folder "../FlatStudio/" there are 3 styles of buttons contained in the following files:
- window_normal_buttons_dark_frame.tar.gz  		(default in FlatStudio)
- window_normal_buttons_light_frame.tar.gz  	(default in FlatStudioLight)
- window_square_buttons.tar.gz  				(default in FlatStudioGray and FlatStudioDark)

eg
to set square buttons in "FlatStudioLight"
copy the contents of "window_square_buttons.tar.gz" in folders:

"../FlatStudioLight/metacity-1 /" and if you use unity, also in "../FlatStudioLight/unity/"


---------------------------------
* 3.- Select style for nautilus *
---------------------------------
Edit (gedit) the file ../FlatStudioDark/gtk-3.0/gtk.css

go to the last line of the file, there are six options for nautilus:
	1.- "gnome-applications-gray.css"  		(nautilus sidebar and toolbar dark gray)
	2.- "gnome-applications-light.css" 		(nautilus sidebar and toolbar light)
	3.- "gnome-applications-gray-light.css" (nautilus sidebar dark gray and toolbar light)


edit (please carefully) the corresponding line "@import" according to the style of nautilus you want,
to make it ONE of the three (not both)

@import url("gnome-applications-gray.css");
or
@import url("gnome-applications-light.css");
or
@import url("gnome-applications-gray-light.css");


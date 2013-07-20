import XMonad
-- LAYOUTS
import XMonad.Layout.Spacing
import XMonad.Layout.Fullscreen 
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.SimplestFloat
import XMonad.Layout.Tabbed 
import XMonad.Layout.ResizableTile
import XMonad.Layout.Circle
import XMonad.Layout.ThreeColumns

-- WINDOW RULES
import XMonad.ManageHook
-- KEYBOARD & MOUSE CONFIG
import XMonad.Util.EZConfig
import XMonad.Actions.FloatKeys
import Graphics.X11.ExtraTypes.XF86
-- STATUS BAR
import XMonad.Hooks.DynamicLog hiding (xmobar, xmobarPP, xmobarColor, sjanssenPP, byorgeyPP)
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Hooks.UrgencyHook
import XMonad.Util.Dmenu
--import XMonad.Hooks.FadeInactive
import XMonad.Hooks.EwmhDesktops hiding (fullscreenEventHook)
import System.IO (hPutStrLn)
--import XMonad.Operations
import qualified XMonad.StackSet as W
import qualified XMonad.Actions.FlexibleResize as FlexibleResize
import XMonad.Util.Run (spawnPipe)
import XMonad.Actions.CycleWS			-- nextWS, prevWS
import Data.List			-- clickable workspaces


--------------------------------------------------------------------------------------------------------------------
-- DECLARE WORKSPACES RULES
--------------------------------------------------------------------------------------------------------------------
myLayout = onWorkspace (myWorkspaces !! 0) (avoidStruts (bigMonitor ||| tiledSpace ||| tiled ||| Mirror tiled) ||| fullTile)
		$ onWorkspace (myWorkspaces !! 1) (avoidStruts (tiledSpace ||| fullTile) ||| fullScreen)
		$ onWorkspace (myWorkspaces !! 2) (avoidStruts (simplestFloat))
		$ avoidStruts ( tiledSpace  ||| tiled ||| fullTile ) 
	where
		tiled  		= spacing 5 $ ResizableTall nmaster delta ratio [] 
		tiledSpace  	= spacing 60 $ ResizableTall nmaster delta ratio [] 
		bigMonitor  	= spacing 5 $ ThreeColMid nmaster delta ratio 
		fullScreen 	= noBorders(fullscreenFull Full)
		fullTile 	= ResizableTall nmaster delta ratio [] 
		borderlessTile	= noBorders(fullTile)
		-- Default number of windows in master pane
		nmaster = 1
		-- Percent of the screen to increment when resizing
		delta 	= 5/100
		-- Default proportion of the screen taken up by main pane
		ratio 	= toRational (2/(1 + sqrt 5 :: Double)) 


--------------------------------------------------------------------------------------------------------------------
-- WORKSPACE DEFINITIONS
--------------------------------------------------------------------------------------------------------------------
myWorkspaces = clickable $ ["i"
		,"ii"	
		,"iii"	
		,"iv"	
		,"v"
		,"vi"]	
	where clickable l = [ "^ca(1,xdotool key alt+" ++ show (n) ++ ")" ++ ws ++ "^ca()" |
				(i,ws) <- zip [1..] l,
				let n = i ]

--------------------------------------------------------------------------------------------------------------------
-- APPLICATION SPECIFIC RULES
--------------------------------------------------------------------------------------------------------------------
myManageHook = composeAll 	[ resource =? "dmenu" --> doFloat
				, resource =? "skype" 	--> doFloat
--				, resource =? "mplayer"	--> doFloat
				, resource =? "steam"	--> doFloat
				, resource =? "hl2_linux" --> doFloat
				, resource =? "feh"	--> doIgnore
				, resource =? "dzen2"	--> doIgnore
				, resource =? "transmission"	--> doShift (myWorkspaces !! 2)
				, resource =? "thunar"	--> doShift (myWorkspaces !! 2)
				, resource =? "chromium"--> doShift (myWorkspaces !! 1)
				, resource =? "lowriter"--> doShift (myWorkspaces !! 3)
				, resource =? "localc"--> doShift (myWorkspaces !! 3)
				, resource =? "loimpress"--> doShift (myWorkspaces !! 3)
				, resource =? "zathura"--> doShift (myWorkspaces !! 3)
				, resource =? "ario"--> doShift (myWorkspaces !! 4)
				, resource =? "ncmpcpp"--> doShift (myWorkspaces !! 4)
				, resource =? "alsamixer"--> doShift (myWorkspaces !! 4)
				, resource =? "mutt"--> doShift (myWorkspaces !! 5)
				, resource =? "irssi"--> doShift (myWorkspaces !! 5)
				, resource =? "centerim"--> doShift (myWorkspaces !! 5)
				, manageDocks]
newManageHook = myManageHook <+> manageHook defaultConfig 

--------------------------------------------------------------------------------------------------------------------
-- DZEN LOG RULES for workspace names, layout image, current program title
--------------------------------------------------------------------------------------------------------------------
myLogHook h = dynamicLogWithPP ( defaultPP
	{
		  ppCurrent		= dzenColor color15 background .	pad
		, ppVisible		= dzenColor color14 background . 	pad
		, ppHidden		= dzenColor color14 background . 	pad
		, ppHiddenNoWindows	= dzenColor background background .	pad
		, ppWsSep		= ""
		, ppSep			= "    "
		, ppLayout		= wrap "^ca(1,xdotool key alt+space)" "^ca()" . dzenColor color2 background .
				(\x -> case x of
					"Full"				->	"^i(/home/sunn/.xmonad/dzen2/layout_full.xbm)"
					"Spacing 5 ResizableTall"	->	"^i(/home/sunn/.xmonad/dzen2/layout_tall.xbm)"
					"ResizableTall"			->	"^i(/home/sunn/.xmonad/dzen2/layout_tall.xbm)"
					"SimplestFloat"			->	"^i(/home/sunn/.xmonad/dzen2/mouse_01.xbm)"
					"Circle"			->	"^i(/home/sunn/.xmonad/dzen2/full.xbm)"
					_				->	"^i(/home/sunn/.xmonad/dzen2/grid.xbm)"
				) 
--		, ppTitle	=  wrap "^ca(1,xdotool key alt+shift+x)^fg(#D23D3D)^fn(fkp)x ^fn()" "^ca()" . dzenColor foreground background . shorten 40 . pad
		, ppTitle	=  wrap "^ca(1,xdotool key alt+shift+x)" "^ca()" . dzenColor color15 background . shorten 40 . pad
		, ppOrder	=  \(ws:l:t:_) -> [ws,l, t]
		, ppOutput	=   hPutStrLn h
	} )


--------------------------------------------------------------------------------------------------------------------
-- Spawn pipes and menus on boot, set default settings
--------------------------------------------------------------------------------------------------------------------
myXmonadBar = "dzen2 -x '0' -y '0' -h '14' -w '500' -ta 'l' -fg '"++foreground++"' -bg '"++background++"' -fn "++myFont
myStatusBar = "/home/sunn/.xmonad/status_bar '"++foreground++"' '"++background++"' "++myFont
--myConky = "conky -c /home/sunn/conkyrc"
--myStartMenu = "/home/sunn/.xmonad/start /home/sunn/.xmonad/start_apps"


main = do
	dzenLeftBar 	<- spawnPipe myXmonadBar
	dzenRightBar	<- spawnPipe myStatusBar
--	xmproc 		<- spawnPipe "GTK2_RC_FILES=/home/sunn/.gtkdocky /usr/bin/docky"
--	xmproc 		<- spawnPipe "tint2 -c /home/sunn/.config/tint2/xmonad.tint2rc"
--	conky 		<- spawn myConky
--	dzenStartMenu	<- spawnPipe myStartMenu
	xmonad $ ewmh defaultConfig
		{ terminal		= myTerminal
		, borderWidth		= 4
		, normalBorderColor 	= color0
		, focusedBorderColor  	= color8
		, modMask 		= mod1Mask
		, layoutHook 		= smartBorders $ myLayout
		, workspaces 		= myWorkspaces
		, manageHook		= newManageHook
		, handleEventHook 	= fullscreenEventHook <+> docksEventHook
		, startupHook		= setWMName "LG3D"
		, logHook		= myLogHook dzenLeftBar -- >> fadeInactiveLogHook 0xdddddddd
		}

--------------------------------------------------------------------------------------------------------------------
-- Keyboard options
--------------------------------------------------------------------------------------------------------------------
		`additionalKeys`
		[((mod1Mask .|. shiftMask	, xK_b), spawn "firefox")
		,((mod1Mask  			, xK_b), spawn "dwb")
		,((mod1Mask .|. shiftMask	, xK_n), spawn "xterm")
		,((mod1Mask .|. shiftMask  	, xK_t), spawn "urxvtc -e tmux")
		,((mod4Mask  			, xK_z), spawn "zathura")
		,((mod4Mask  			, xK_w), spawn "lowriter")
		,((mod4Mask  			, xK_c), spawn "localc")
		,((mod4Mask  			, xK_m), spawn "urxvtc -title mutt -name mutt -e muttb")
		,((mod4Mask  			, xK_i), spawn "urxvtc -title irssi -name irssi -e irssi")
		,((mod4Mask  			, xK_n), spawn "urxvtc -title ncmpcpp -name ncmpcpp -e ncmpcpp")
		,((mod4Mask  			, xK_a), spawn "urxvtc -title alsamixer -name alsamixer -e alsamixer")
		,((mod4Mask  			, xK_M), spawn "urxvtc -title centerim -name centerim -e centerim")
		,((mod1Mask 			, xK_r), spawn "/home/sunn/scripts/lens")
		,((mod1Mask .|. shiftMask	, xK_r), spawn "/home/sunn/scripts/dmenu/spotlight")
		,((mod1Mask			, xK_q), spawn "killall dzen2; killall conky; killall tint2; cd ~/.xmonad; ghc -threaded xmonad.hs; mv xmonad xmonad-x86_64-linux; xmonad --restart" )
		,((mod1Mask .|. shiftMask	, xK_i), spawn "xcalib -invert -alter")
		,((mod1Mask .|. shiftMask	, xK_x), kill)
		,((mod1Mask .|. shiftMask	, xK_c), return())
		,((mod1Mask  			, xK_p), moveTo Prev NonEmptyWS)
		,((mod1Mask  			, xK_n), moveTo Next NonEmptyWS)
		,((mod1Mask  			, xK_c), moveTo Next EmptyWS)
		,((mod1Mask .|. shiftMask	, xK_l), sendMessage MirrorShrink)
		,((mod1Mask .|. shiftMask	, xK_h), sendMessage MirrorExpand)
		,((mod1Mask  			, xK_a), withFocused (keysMoveWindow (-20,0)))
		,((mod1Mask  			, xK_d), withFocused (keysMoveWindow (0,-20)))
		,((mod1Mask  			, xK_s), withFocused (keysMoveWindow (0,20)))
		,((mod1Mask  			, xK_f), withFocused (keysMoveWindow (20,0)))
		,((mod1Mask .|. shiftMask  	, xK_a), withFocused (keysResizeWindow (-20,0) (0,0)))
		,((mod1Mask .|. shiftMask  	, xK_d), withFocused (keysResizeWindow (0,-20) (0,0)))
		,((mod1Mask .|. shiftMask  	, xK_s), withFocused (keysResizeWindow (0,20) (0,0)))
		,((mod1Mask .|. shiftMask  	, xK_f), withFocused (keysResizeWindow (20,0) (0,0)))
		,((mod1Mask			, xK_0), spawn "xdotool key alt+6")
		,((mod1Mask			, xK_9), spawn "xdotool key alt+5")
		,((mod1Mask			, xK_8), spawn "xdotool key alt+4")
		,((0				, xK_Super_L), spawn "menu ~/.xmonad/apps")
		,((mod1Mask			, xK_Super_L), spawn "menu ~/.xmonad/configs")
		,((mod1Mask  			, xK_F1), spawn "~/.xmonad/sc ~/.xmonad/scripts/dzen_date.sh")
		,((mod1Mask  			, xK_F2), spawn "~/.xmonad/sc ~/.xmonad/scripts/dzen_music.sh")
		,((mod1Mask  			, xK_F3), spawn "~/.xmonad/sc ~/.xmonad/scripts/dzen_network.sh")
		,((mod1Mask  			, xK_F4), spawn "~/.xmonad/sc ~/.xmonad/scripts/dzen_battery.sh")
		,((mod1Mask  			, xK_F5), spawn "~/.xmonad/sc ~/.xmonad/scripts/dzen_hardware.sh")
		,((mod1Mask  			, xK_F6), spawn "~/.xmonad/sc ~/.xmonad/scripts/dzen_log.sh")
		,((0 	 			, xK_Print), spawn "scrot & mplayer /usr/share/sounds/freedesktop/stereo/screen-capture.oga")
		,((mod1Mask 	 		, xK_Print), spawn "scrot -s & mplayer /usr/share/sounds/freedesktop/stereo/screen-capture.oga")
		,((0                     	, xF86XK_AudioLowerVolume), spawn "/home/sunn/scripts/dvol2 -d 2 & mplayer /usr/share/sounds/freedesktop/stereo/audio-volume-change.oga")
		,((0                     	, xF86XK_AudioRaiseVolume), spawn "/home/sunn/scripts/dvol2 -i 2 & mplayer /usr/share/sounds/freedesktop/stereo/audio-volume-change.oga")
		,((0                     	, xF86XK_AudioMute), spawn "/home/sunn/scripts/dvol2 -t")
		,((0                     	, xF86XK_Display), spawn "/home/sunn/scripts/project")
		,((0                     	, xF86XK_Sleep), spawn "pm-suspend")
		,((0                     	, xF86XK_AudioPlay), spawn "ncmpcpp toggle")
		,((0                     	, xF86XK_AudioNext), spawn "ncmpcpp next")
		,((0                     	, xF86XK_AudioPrev), spawn "ncmpcpp prev")
		]
		`additionalMouseBindings`
		[((mod1Mask			, 6), (\_ -> moveTo Next NonEmptyWS))
		,((mod1Mask			, 7), (\_ -> moveTo Prev NonEmptyWS))
		,((mod1Mask			, 5), (\_ -> moveTo Prev NonEmptyWS))
		,((mod1Mask			, 4), (\_ -> moveTo Next NonEmptyWS))
--		,((0				, 2), (\w -> focus w >> windows W.swapMaster))
--		,((0				, 3), (\w -> focus w >> FlexibleResize.mouseResizeWindow w))
		]



-- Define constants

myTerminal 	= "urxvtc"
myBitmapsDir	= "~/.xmonad/dzen2/"

--myFont 		= "-*-tamsyn-medium-r-normal-*-12-87-*-*-*-*-*-*"
--myFont		= "-*-terminus-medium-*-normal-*-9-*-*-*-*-*-*-*"
myFont		= "-*-nu-*-*-*-*-*-*-*-*-*-*-*-*"
--myFont			= "-artwiz-lime-medium-r-normal-*-10-110-75-75-m-50-iso8859-*"
--myFont			= "-artwiz-limey-medium-r-normal-*-10-110-75-75-m-50-iso8859-*"
--myFont		= "-benis-lemon-medium-r-normal-*-10-110-75-75-m-50-iso8859-*"
--myFont		= "'sans:italic:bold:underline'"
--myFont		= "xft:droid sans mono:size=9"
--myFont		= "xft:Droxd Sans:size=12"
--myFont		= "-*-cure-*-*-*-*-*-*-*-*-*-*-*-*"

--background=         "#140c0b"
--foreground=         "#877a70"
--color0=             "#403f3e"
--color8=             "#666362"
--color1=             "#91444d"
--color9=             "#c78186"
--color2=             "#6b853d"
--color10=            "#abbd80"
--color3=             "#916949"
--color11=            "#cca88b"
--color4=             "#557282"
--color12=            "#8eabbd"
--color5=             "#78516d"
--color13=            "#a8879f"
--color6=             "#58756c"
--color14=            "#8ca8a2"
--color7=             "#94928f"
--color15=            "#cdcdcd"

-- CRYPT
--background="#000000"
--foreground="#D3D3D3"
--color0=  "#181818"
--color8=  "#181818"
--color1=  "#D7D7D7"
--color9=  "#D7D7D7"
--color2=  "#AADB0F"
--color10= "#AADB0F"
--color3=  "#666666"
--color11= "#666666"
--color4=  "#FFFFFF"
--color12= "#FFFFFF"
--color5=  "#91BA0D"
--color13= "#91BA0D"
--color6=  "#D4D4D4"
--color14= "#D4D4D4"
--color7=  "#D3D3D3"
--color15= "#D3D3D3"

--CLOUDS
--background= "#0E0E0E"
--foreground= "#fefefe"
-- 
--color0= "#454545"
--color8= "#666666"
--color1=  "#CC4747"
--color9=  "#BF5858"
--color2=  "#A0CF5D"
--color10= "#B8D68C"
--color3=  "#FF9B52"
--color11= "#FFB680"
--color4=  "#5FA69B"
--color12= "#99C7BF"
--color5=  "#A966CC"
--color13= "#BD9FCC"
--color6=  "#6CAB79"
--color14= "#95BA9C"
--color7=  "#d3d3d3"
--color15= "#fefefe"

-- EROSION EDIT
background= "#181512"
foreground= "#D6C3B6"
color0=  "#332d29"
color8=  "#817267"
color1=  "#8c644c"
color9=  "#9f7155"
color2=  "#746C48"
color10= "#9f7155"
color3=  "#bfba92"
color11= "#E0DAAC"
color4=  "#646a6d"
color12= "#777E82"
color5=  "#766782"
color13= "#897796"
color6=  "#4B5C5E"
color14= "#556D70"
color7=  "#504339"
color15= "#9a875f"
-- EROSION
--background= "#181512"
--foreground= "#bea492"
--
--color0= "#332d29"
--color8= "#817267"
--
--color1= "#8c644c"
--color9= "#9f7155"
--
--color2= "#c4be90"
--color10= "#bec17e"
--
--color3= "#bfba92"
--color11= "#fafac0"
--
--color4= "#646a6d"
--color12= "#626e74"
--
--color5= "#6d6871"
--color13= "#756f7b"
--
--color6= "#3b484a"
--color14= "#444d4e"
--
--color7= "#504339"
--color15= "#9a875f"

-- PAPEY
--foreground= "#e5e5e5"
--background= "#1d1d1d"
--color0=  "#121212"
--color8=  "#5f5f5F" 
--color1=  "#a35b66"
--color9=  "#ab6b74"
--color2=  "#99ab6f"
--color10= "#acb792"
--color3=  "#ca9733"
--color11= "#ccaa69"
--color4=  "#495d6e"
--color12= "#687987"
--color5=  "#825969"
--color13= "#977381"
--color6=  "#839191"
--color14= "#98A4A4"
--color7=  "#E0E0E0"
--color15= "#e5e5e5"

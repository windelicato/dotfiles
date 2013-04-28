import XMonad
-- LAYOUTS
import XMonad.Layout.Spacing
import XMonad.Layout.Fullscreen 
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.SimplestFloat
import XMonad.Layout.ResizableTile
import XMonad.Layout.Circle
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
myLayout = onWorkspace (myWorkspaces !! 0) (avoidStruts (tiledSpace ||| tiled) ||| fullTile)
		$ onWorkspace (myWorkspaces !! 1) (avoidStruts (tiledSpace ||| fullTile) ||| fullScreen)
		$ onWorkspace (myWorkspaces !! 2) (avoidStruts simplestFloat)
		$ avoidStruts ( tiledSpace  ||| tiled ||| fullTile ) 
	where
		tiled  		= spacing 5 $ ResizableTall nmaster delta ratio [] 
		tiledSpace  	= spacing 60 $ ResizableTall nmaster delta ratio [] 
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
myWorkspaces = clickable $ ["term"
		,"web"	
		,"float"	
		,"docs"	
		,"tunes"
		,"mail"]	
	where clickable l = [ "^ca(1,xdotool key alt+" ++ show (n) ++ ")" ++ ws ++ "^ca()" |
				(i,ws) <- zip [1..] l,
				let n = i ]

--------------------------------------------------------------------------------------------------------------------
-- APPLICATION SPECIFIC RULES
--------------------------------------------------------------------------------------------------------------------
myManageHook = composeAll 	[ resource =? "dmenu" --> doFloat
				, resource =? "skype" 	--> doFloat
				, resource =? "mplayer"	--> doFloat
				, resource =? "feh"	--> doFloat
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
		  ppCurrent		= dzenColor color2 background .	pad
		, ppVisible		= dzenColor color14 background . 	pad
		, ppHidden		= dzenColor color14 background . 	pad
		, ppHiddenNoWindows	= dzenColor color6 background .	pad
		, ppWsSep		= ""
		, ppSep			= "    "
		, ppLayout		= wrap "^ca(1,xdotool key alt+space)" "^ca()" . dzenColor foreground background .
				(\x -> case x of
					"Full"				->	"^i(/home/sunn/.xmonad/dzen2/layout_full.xbm)"
					"Spacing 5 ResizableTall"	->	"^i(/home/sunn/.xmonad/dzen2/layout_tall.xbm)"
					"ResizableTall"			->	"^i(/home/sunn/.xmonad/dzen2/layout_tall.xbm)"
					"SimplestFloat"			->	"^i(/home/sunn/.xmonad/dzen2/mouse_01.xbm)"
					"Circle"			->	"^i(/home/sunn/.xmonad/dzen2/full.xbm)"
					_				->	"^i(/home/sunn/.xmonad/dzen2/grid.xbm)"
				) 
--		, ppTitle	=   wrap "^ca(1,xdotool key alt+shift+x)^fg(#222222)^i(/home/sunn/.xmonad/dzen2/corner_left.xbm)^bg(#222222)^fg(#AADB0F)^fn(fkp)x^fn()" "^fg(#222222)^i(/home/sunn/.xmonad/dzen2/corner_right.xbm)^ca()" .  dzenColor foreground "#222222" . shorten 40 . pad		
		, ppOrder	=  \(ws:l:t:_) -> [ws,l]
		, ppOutput	=   hPutStrLn h
	} )


--------------------------------------------------------------------------------------------------------------------
-- Spawn pipes and menus on boot, set default settings
--------------------------------------------------------------------------------------------------------------------
myXmonadBar = "dzen2 -x '0' -y '0' -h '14' -w '700' -ta 'l' -fg '"++foreground++"' -bg '"++background++"' -fn "++myFont
myStatusBar = "conky -qc /home/sunn/.xmonad/.conky_dzen | dzen2 -x '600' -w '766' -h '14' -ta 'r' -bg '"++background++"' -fg '"++foreground++"' -y '0' -fn "++myFont
--myConky = "conky -c /home/sunn/conkyrc"
--myStartMenu = "/home/sunn/.xmonad/start /home/sunn/.xmonad/start_apps"


main = do
	dzenLeftBar 	<- spawnPipe myXmonadBar
	dzenRightBar	<- spawnPipe myStatusBar
	xmproc 		<- spawnPipe "GTK2_RC_FILES=/home/sunn/.gtkdocky /usr/bin/docky"
--	xmproc 		<- spawnPipe "tint2 -c /home/sunn/.config/tint2/xmonad.tint2rc"
--	conky 		<- spawn myConky
--	dzenStartMenu	<- spawnPipe myStartMenu
	xmonad $ ewmh defaultConfig
		{ terminal		= myTerminal
		, borderWidth		= 2
		, normalBorderColor 	= background
		, focusedBorderColor  	= color6
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
		[((mod1Mask .|. shiftMask	, xK_b), spawn "chromium")
		,((mod1Mask  			, xK_b), spawn "dwb")
--		,((mod1Mask .|. shiftMask	, xK_n), spawn "urxvtc -fn '-*-gohufont-medium-r-normal-*-12-*-*-*-*-*-*-*' -fb '-*-gohufont-medium-r-normal-*-12-*-*-*-*-*-*-*' -fi '-*-gohufont-medium-r-normal-*-12-*-*-*-*-*-*-*'")
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
		,((0				, xK_Super_L), spawn "menu ~/.xmonad/apps")
		,((mod1Mask			, xK_Super_L), spawn "menu ~/.xmonad/configs")
		,((mod1Mask  			, xK_F1), spawn "~/.xmonad/sc ~/.xmonad/scripts/dzen_music.sh")
		,((mod1Mask  			, xK_F2), spawn "~/.xmonad/sc ~/.xmonad/scripts/dzen_vol.sh")
		,((mod1Mask  			, xK_F3), spawn "~/.xmonad/sc ~/.xmonad/scripts/dzen_network.sh")
		,((mod1Mask  			, xK_F4), spawn "~/.xmonad/sc ~/.xmonad/scripts/dzen_battery.sh")
		,((mod1Mask  			, xK_F5), spawn "~/.xmonad/sc ~/.xmonad/scripts/dzen_hardware.sh")
		,((mod1Mask  			, xK_F6), spawn "~/.xmonad/sc ~/.xmonad/scripts/dzen_pacman.sh")
		,((mod1Mask  			, xK_F7), spawn "~/.xmonad/sc ~/.xmonad/scripts/dzen_date.sh")
		,((mod1Mask  			, xK_F8), spawn "~/.xmonad/sc ~/.xmonad/scripts/dzen_log.sh")
		,((0 	 			, xK_Print), spawn "scrot & mplayer /usr/share/sounds/freedesktop/stereo/screen-capture.oga")
		,((mod1Mask 	 		, xK_Print), spawn "scrot -s & mplayer /usr/share/sounds/freedesktop/stereo/screen-capture.oga")
		,((0                     	, xF86XK_AudioLowerVolume), spawn "amixer set Master 2- & mplayer /usr/share/sounds/freedesktop/stereo/audio-volume-change.oga")
		,((0                     	, xF86XK_AudioRaiseVolume), spawn "amixer set Master 2+ & mplayer /usr/share/sounds/freedesktop/stereo/audio-volume-change.oga")
		,((0                     	, xF86XK_AudioMute), spawn "amixer set Master toggle")
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
--myFont		= "-*-tamsyn-medium-*-normal-*-10-*-*-*-*-*-*-*"
--myFont		= "-*-terminus-medium-*-normal-*-9-*-*-*-*-*-*-*"
--myFont		= "-*-nu-*-*-*-*-*-*-*-*-*-*-*-*"
--myFont			= "-artwiz-lime-medium-r-normal-*-10-110-75-75-m-50-iso8859-*"
myFont			= "-artwiz-limey-medium-r-normal-*-10-110-75-75-m-50-iso8859-*"
--myFont		= "'sans:italic:bold:underline'"
--myFont		= "xft:droid sans mono:size=9"
--myFont		= "xft:Droxd Sans:size=12"
--myFont		= "-*-cure-*-*-*-*-*-*-*-*-*-*-*-*"

--background= "#30303a"                                                
--foreground= "#d6d6d6"
--
--color0 = "#4a4a4a"
--color8 = "#777777"
--
----color1 = "#c29198"
----color9 = "#a0616a"
--color1 = "#df6f6f"
--color9 = "#eba8a8"
--
----color2 = "#abd8a5"
----color10 = "#80a27b"
--color2 = "#6fdfa4"
--color10 = "#a8ebc8"
--
----color3 = "#acacac"
----color11 = "#858585"
--color3 = "#dfdc6f"
--color11 = "#ebeaa8"
--
--color4 = "#6fa5df"
--color12 = "#a8c9eb"
--
--color5 = "#846fdf"
--color13 = "#b5a8eb"
--
--color6 = "#6fcadf"
--color14 = "#a6e6eb"
--
--color7 = "#aaaaaa"
--color15 = "#c3c3c3"


background=            "#262729"
foreground=            "#f8f8f2"
color0=                "#626262"
color8=                "#626262"
color1=                "#f92671"
color9=                "#ff669d"
color2=                "#a6e22e"
color10=               "#beed5f"
color3=                "#fd971f"
color11=               "#e6db74"
color4=                "#1692d0"
color12=               "#66d9ef"
color5=                "#9e6ffe"
color13=               "#df92f6"
color6=                "#5e7175"
color14=               "#a3babf"
color7=                "#ffffff"
color15=               "#ffffff"
cursorColor=           "#b5d2dd"


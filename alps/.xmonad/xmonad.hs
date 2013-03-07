import XMonad
-- LAYOUTS
import XMonad.Layout.Spacing
import XMonad.Layout.Fullscreen 
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.SimplestFloat
import XMonad.Layout.ThreeColumns
import XMonad.Layout.ResizableTile
import XMonad.Layout.Circle
import XMonad.Layout.Spiral

import XMonad.Actions.GridSelect
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
import XMonad.Util.Run (spawnPipe)
import XMonad.Actions.CycleWS			-- nextWS, prevWS
import Data.List			-- clickable workspaces



defaultLayouts =	  onWorkspace (myWorkspaces !! 0) (avoidStruts (Circle ||| tiled) ||| fullTile)
			$ onWorkspace (myWorkspaces !! 1) (avoidStruts (Circle ||| noBorders (fullTile)) ||| fullScreen)
			$ onWorkspace (myWorkspaces !! 2) (avoidStruts simplestFloat)
			$ avoidStruts ( tiledSpace  ||| tiled ||| fullTile ) ||| fullScreen
	where
		tiled  		= spacing 5 $ ResizableTall nmaster delta ratio [] 
		tiledSpace  	= spacing 60 $ ResizableTall nmaster delta ratio [] 
		tile3		= spacing 5 $ ThreeColMid nmaster delta ratio
		fullScreen 	= noBorders(fullscreenFull Full)
		fullTile 	= ResizableTall nmaster delta ratio [] 
		fullTile3	=  ThreeColMid nmaster delta ratio
		borderlessTile	= noBorders(fullTile)
		fullGoldenSpiral 	= spiral ratio
		goldenSpiral 	= spacing 5 $ spiral ratio
		-- Default number of windows in master pane
		nmaster = 1
		-- Percent of the screen to increment when resizing
		delta 	= 5/100
		-- Default proportion of the screen taken up by main pane
		ratio 	= toRational (2/(1 + sqrt 5 :: Double)) 

		

-- Give some workspaces no borders
nobordersLayout = noBorders $ Full
myLayout = defaultLayouts

-- Declare workspaces and rules for applications

myWorkspaces = clickable $ ["^i(/home/sunn/.dzen/term.xbm) shell"
		,"^i(/home/sunn/.dzen/fs_01.xbm) web"	
		,"^i(/home/sunn/.dzen/mouse_01.xbm) float"	
		,"^i(/home/sunn/.dzen/diskette.xbm) docs"	
		,"^i(/home/sunn/.dzen/note.xbm) tunes"
--		,"^i(/home/sunn/.dzen/info_03.xbm) irc",
		,"^i(/home/sunn/.dzen/mail.xbm) mail"]	

	where clickable l     = [ "^ca(1,xdotool key alt+" ++ show (n) ++ ")" ++ ws ++ "^ca()" |
                            (i,ws) <- zip [1..] l,
                            let n = i ]
			
myManageHook = composeAll 	[ resource =? "dmenu" --> doFloat
				, resource =? "skype" 	--> doFloat
				, resource =? "mplayer"	--> doFloat
				, resource =? "feh"	--> doFloat
				, resource =? "chromium"--> doShift (myWorkspaces !! 1)
				, resource =? "ario"--> doShift (myWorkspaces !! 4)
				, resource =? "lowriter"--> doShift (myWorkspaces !! 3)
				, resource =? "zathura"--> doShift (myWorkspaces !! 3)
				, resource =? "localc"--> doShift (myWorkspaces !! 3)
				, resource =? "loimpress"--> doShift (myWorkspaces !! 3)
				]
newManageHook = myManageHook <+> manageHook defaultConfig <+> manageDocks 

myLogHook h = dynamicLogWithPP ( defaultPP
	{
--		  ppCurrent		= dzenColor white0 background . wrap "^fg(#916949).: ^fg()" "^fg(#916949) :.^fg()" . pad
		  ppCurrent		= dzenColor foreground background . pad
		, ppVisible		= dzenColor white0 background . pad
		, ppHidden		= dzenColor white0 background . pad
		, ppHiddenNoWindows	= dzenColor black0 background . pad
		, ppWsSep		= ""
		, ppSep			= "    "
		, ppLayout		= wrap "^ca(1,xdotool key alt+space)" "^ca()" . dzenColor white1 background .
				(\x -> case x of
					"Full"				->	"^i(/home/sunn/.xmonad/dzen2/layout_full.xbm)"
					"Spacing 5 ResizableTall"	->	"^i(/home/sunn/.xmonad/dzen2/layout_tall.xbm)"
					"ResizableTall"			->	"^i(/home/sunn/.xmonad/dzen2/layout_tall.xbm)"
					"SimplestFloat"			->	"^i(/home/sunn/.xmonad/dzen2/mouse_01.xbm)"
					"Circle"			->	"^i(/home/sunn/.xmonad/dzen2/full.xbm)"
					_				->	"^i(/home/sunn/.xmonad/dzen2/grid.xbm)"
				) 
--		, ppTitle	=   wrap "^ca(1,xdotool key alt+shift+x)^fg(#424242)^i(/home/sunn/.xmonad/dzen2/corner_left.xbm)^bg(#424242)^fg(#74637d)X" "^fg(#424242)^i(/home/sunn/.xmonad/dzen2/corner_right.xbm)^ca()" .  dzenColor background green0 . shorten 40 . pad		
		, ppOrder	=  \(ws:l:t:_) -> [ws,l]
		, ppOutput	=   hPutStrLn h
	} )

myXmonadBar = "dzen2 -x '0' -y '0' -h '12' -w '700' -ta 'l' -fg '"++foreground++"' -bg '"++background++"' -fn "++myFont
myStatusBar = "conky -qc /home/sunn/.xmonad/.conky_dzen | dzen2 -x '700' -w '666' -h '12' -ta 'r' -bg '"++background++"' -fg '"++foreground++"' -y '0' -fn "++myFont
--myConky = "conky -c /home/sunn/conkyrc"
--myStartMenu = "/home/sunn/.xmonad/start /home/sunn/.xmonad/start_apps"

main = do
	dzenLeftBar 	<- spawnPipe myXmonadBar
	dzenRightBar	<- spawnPipe myStatusBar
--	xmproc 		<- spawnPipe "/usr/bin/docky"
	xmproc 		<- spawnPipe "GTK2_RC_FILES=/home/sunn/.gtkdocky /usr/bin/docky"
--	conky 		<- spawn myConky
--	dzenStartMenu	<- spawnPipe myStartMenu
	xmonad $ ewmh defaultConfig
		{ terminal		= myTerminal
		, borderWidth		= 1
		, normalBorderColor 	= black0
		, focusedBorderColor  	= magenta0
		, modMask 		= mod1Mask
		, layoutHook 		= myLayout
--		, layoutHook 		= avoidStruts  $  layoutHook defaultConfig
		, workspaces 		= myWorkspaces
		, manageHook		= newManageHook
--		, manageHook 		= manageDocks <+> manageHook defaultConfig
		, handleEventHook 	= fullscreenEventHook <+> docksEventHook
		, startupHook		= setWMName "LG3D"
		, logHook		= myLogHook dzenLeftBar -- >> fadeInactiveLogHook 0xdddddddd
		}
		`additionalKeys`
		[((mod1Mask .|. shiftMask	, xK_b), spawn "chromium")
		,((mod1Mask  			, xK_b), spawn "dwb")
		,((mod1Mask .|. shiftMask	, xK_n), spawn "urxvtc -fn '-*-terminus-medium-r-normal-*-12-*-*-*-*-*-*-*' -fb '-*-terminus-bold-r-normal-*-12-*-*-*-*-*-*-*' -fi '-*-terminus-medium-r-normal-*-12-*-*-*-*-*-*-*'")
		,((mod1Mask .|. shiftMask  	, xK_t), spawn "urxvtc -e tmux")
		,((mod1Mask  			, xK_z), spawn "zathura")
		,((mod1Mask 			, xK_r), spawn "/home/sunn/scripts/lens")
--		,((mod1Mask .|. shiftMask	, xK_r), spawn "dmenu_run -nb '#000000' -nf '#404040' -sb '#000000' -sf '#FFFFFF' -fn '-*-lime-*-*-*-*-*-*-*-*-*-*-*-*'")
		,((mod1Mask .|. shiftMask	, xK_r), spawn "/home/sunn/scripts/dmenu/spotlight")
		,((mod1Mask			, xK_q), spawn "killall dzen2; killall conky; cd ~/.xmonad; ghc -threaded xmonad.hs; mv xmonad xmonad-x86_64-linux; xmonad --restart" )
		,((mod1Mask .|. shiftMask	, xK_i), spawn "xcalib -invert -alter")
		,((mod1Mask .|. shiftMask	, xK_x), kill)
		,((mod1Mask .|. shiftMask	, xK_c), return())
		,((mod1Mask  			, xK_p), moveTo Prev NonEmptyWS)
		,((mod1Mask  			, xK_n), moveTo Next NonEmptyWS)
		,((mod1Mask  			, xK_c), moveTo Next EmptyWS)
		,((mod1Mask .|. shiftMask	, xK_l), sendMessage MirrorShrink)
		,((mod1Mask .|. shiftMask	, xK_h), sendMessage MirrorExpand)
--		,((mod1Mask .|. shiftMask	, xK_q), sendMessage MirrorExpand)
		,((mod1Mask  			, xK_a), withFocused (keysMoveWindow (-20,0)))
		,((mod1Mask  			, xK_s), withFocused (keysMoveWindow (0,-20)))
		,((mod1Mask  			, xK_d), withFocused (keysMoveWindow (0,20)))
		,((mod1Mask  			, xK_f), withFocused (keysMoveWindow (20,0)))
		,((mod1Mask .|. shiftMask  	, xK_a), withFocused (keysResizeWindow (-20,0) (0,0)))
		,((mod1Mask .|. shiftMask  	, xK_s), withFocused (keysResizeWindow (0,-20) (0,0)))
		,((mod1Mask .|. shiftMask  	, xK_d), withFocused (keysResizeWindow (0,20) (0,0)))
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
		,((mod1Mask 	 			, xK_Print), spawn "scrot -s & mplayer /usr/share/sounds/freedesktop/stereo/screen-capture.oga")
		,((0                     	, xF86XK_AudioLowerVolume), spawn "amixer set Master 2- & mplayer /usr/share/sounds/freedesktop/stereo/audio-volume-change.oga")
		,((0                     	, xF86XK_AudioRaiseVolume), spawn "amixer set Master 2+ & mplayer /usr/share/sounds/freedesktop/stereo/audio-volume-change.oga")
		,((0                     	, xF86XK_AudioMute), spawn "amixer set Master toggle")
		,((0                     	, xF86XK_Display), spawn "xrandr --output VGA1 --mode 1366x768")
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
		]





myTerminal 	= "urxvtc"
myBitmapsDir	= "~/.xmonad/dzen2/"
--myFont		= "-*-tamsyn-medium-*-normal-*-10-*-*-*-*-*-*-*"
--myFont		= "-*-terminus-medium-*-normal-*-9-*-*-*-*-*-*-*"
myFont		= "-*-lime-*-*-*-*-*-*-*-*-*-*-*-*"
--myFont		= "-*-drift-*-*-*-*-*-*-*-*-*-*-*-*"
--myFont		= "xft:inconsolata:size=10"
--myFont		= "xft:droid sans mono:size=9"
--myFont		= "-*-cure-*-*-*-*-*-*-*-*-*-*-*-*"

background= "#000000"
foreground= "#ffffff"

black0= "#343638"
black1= "#404040"

red0=  "#2f468e"
red1=  "#7791e0"

green0= "#424242"
green1= "#828a8c"

yellow0=  "#6b8ba3"
yellow1= "#8ebdde"

blue0=  "#1c4582"
blue1= "#5365a6"

magenta0=  "#74636d"
magenta1= "#927d9e"

cyan0=  "#556c85"
cyan1= "#6e98b8"

white0=  "#b2b2b2"
white1= "#bdbdbd"

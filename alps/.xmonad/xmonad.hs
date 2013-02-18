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
--import XMonad.Hooks.EwmhDesktops
import System.IO (hPutStrLn)
--import XMonad.Operations
import XMonad.Util.Run (spawnPipe)
import XMonad.Actions.CycleWS			-- nextWS, prevWS
import Data.List			-- clickable workspaces

gsconfig colorizer = (buildDefaultGSConfig myColorizer) 
	{ gs_cellheight = 30
	, gs_cellwidth = 100
	, gs_cellpadding = 10
	, gs_font = myFont
	}

-- | A green monochrome colorizer based on window class
myColorizer = colorRangeFromClassName
	(0x00,0x00,0x00) -- lowest inactive bg
	(0x60,0xA0,0xC0) -- highest inactive bg
	(0x34,0x75,0xAA) -- active bg
	(0xBB,0xBB,0xBB) -- inactive fg
	(0x00,0x00,0x00) -- active fg
 
defaultLayouts =	  onWorkspace (myWorkspaces !! 0) (avoidStruts (Circle ||| tiled) ||| fullTile ||| fullTile3)
			$ onWorkspace (myWorkspaces !! 1) (fullScreen ||| avoidStruts (tiled ||| Circle))
			$ onWorkspace (myWorkspaces !! 2) (avoidStruts simplestFloat)
			$ avoidStruts ( tiled   ||| fullTile ||| fullTile3 ||| Circle ||| simplestFloat) ||| fullScreen 
	where
		tiled  		= spacing 30 $ ResizableTall nmaster delta (1/3) [] 
		tile3		= spacing 30 $ ThreeColMid nmaster delta (1/3)
		fullScreen 	= noBorders(fullscreenFull Full)
		fullTile 	= ResizableTall nmaster delta (1/3) [] 
		fullTile3	=  ThreeColMid nmaster delta (1/3)
		borderlessTile	= noBorders(fullTile)

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

myWorkspaces = clickable $ ["^i(/home/sunn/.dzen/full.xbm) shell"
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
				, resource =? "java" 	--> doFloat
				, resource =? "./a.out" --> doFloat
				, resource =? "mplayer"	--> doFloat
				, resource =? "feh"	--> doFloat
				, className =? "tint2"	--> doIgnore
				, resource =? "chromium"--> doShift (myWorkspaces !! 1)
				, resource =? "lowriter"--> doShift (myWorkspaces !! 3)
				, resource =? "localc"--> doShift (myWorkspaces !! 3)
				, resource =? "loimpress"--> doShift (myWorkspaces !! 3)
				]
newManageHook = myManageHook <+> manageHook defaultConfig <+> manageDocks

myLogHook h = dynamicLogWithPP ( defaultPP
	{
		  ppCurrent		= dzenColor foreground background . pad
		, ppVisible		= dzenColor black1 background . pad
		, ppHidden		= dzenColor green1 background . pad
		, ppHiddenNoWindows	= dzenColor black1 background . pad
		, ppWsSep		= ""
		, ppSep			= "  "
		, ppLayout		= dzenColor white1 background .
				(\x -> case x of
					"Full"				->	"^i(/home/sunn/.xmonad/dzen2/layout_full.xbm)"
					"Spacing 30 ResizableTall"	->	"^i(/home/sunn/.xmonad/dzen2/layout_tall.xbm)"
					"ResizableTall"			->	"^i(/home/sunn/.xmonad/dzen2/layout_tall.xbm)"
					"SimplestFloat"			->	"^i(/home/sunn/.xmonad/dzen2/mouse_01.xbm)"
					"Circle"			->	"^i(/home/sunn/.xmonad/dzen2/full.xbm)"
					_				->	"^i(/home/sunn/.xmonad/dzen2/grid.xbm)"
				)
	--	, ppTitle	=   ("" ++) . dzenColor green1 background . dzenEscape
		, ppOrder	=  \(ws:t:_:_) -> [ws,t]
		, ppOutput	=   hPutStrLn h
	} )

myXmonadBar = "dzen2 -x '70' -y '0' -h '15' -w '330' -ta 'l' -fg '"++foreground++"' -bg '"++background++"' -fn "++myFont
myStatusBar = "conky -qc /home/sunn/.xmonad/.conky_dzen | dzen2 -x '400' -w '966' -h '15' -ta 'r' -bg '"++background++"' -fg '"++foreground++"' -y '0' -fn "++myFont
myStartMenu = "/home/sunn/.xmonad/start /home/sunn/.xmonad/start_apps"

main = do
	dzenLeftBar 	<- spawnPipe myXmonadBar
	dzenRightBar	<- spawnPipe myStatusBar
	dzenStartMenu	<- spawnPipe myStartMenu
    	xmproc <- spawnPipe "GTK2_RC_FILES=/home/sunn/.gtkdocky /usr/bin/docky"
	xmonad $ defaultConfig
		{ terminal		= myTerminal
		, borderWidth		= 2
		, normalBorderColor 	= black0
		, focusedBorderColor  	= purple0
		, modMask 		= mod1Mask
		, layoutHook 		= myLayout
--		, layoutHook 		= avoidStruts  $  layoutHook defaultConfig
		, workspaces 		= myWorkspaces
		, manageHook		= newManageHook
--		, manageHook 		= manageDocks <+> manageHook defaultConfig
		, handleEventHook 	= fullscreenEventHook
		, startupHook		= setWMName "LG3D"
		, logHook		= myLogHook dzenLeftBar -- >> fadeInactiveLogHook 0xdddddddd
		}
		`additionalKeys`
		[((mod1Mask .|. shiftMask	, xK_b), spawn "chromium")
		,((mod1Mask  			, xK_b), spawn "dwb")
		,((mod1Mask  			, xK_n), spawn "urxvtd")
		,((mod1Mask .|. shiftMask  	, xK_t), spawn "urxvtc -e tmux")
		,((mod1Mask  			, xK_z), spawn "zathura")
		,((mod1Mask 			, xK_r), spawn "GTK_RC_FILES=/home/sunn/.gtkdocky /home/sunn/scripts/lens")
		,((mod1Mask .|. shiftMask	, xK_r), spawn "dmenu_run -nb '#000000' -nf '#404040' -sb '#000000' -sf '#FFFFFF' -fn '-*-lime-*-*-*-*-*-*-*-*-*-*-*-*'")
		,((mod1Mask			, xK_q), spawn "killall dzen2; killall conky; cd ~/.xmonad; ghc -threaded xmonad.hs; mv xmonad xmonad-x86_64-linux; xmonad --restart" )
		,((mod1Mask  			, xK_g), goToSelected $ gsconfig myColorizer)
		,((mod1Mask .|. shiftMask	, xK_x), kill)
		,((mod1Mask .|. shiftMask	, xK_c), return())
		,((mod1Mask  			, xK_p), prevWS)
		,((mod1Mask  			, xK_n), nextWS)
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
		,((0                     	, xF86XK_AudioLowerVolume), spawn "amixer set Master 2-")
		,((0                     	, xF86XK_AudioRaiseVolume), spawn "amixer set Master 2+")
		,((0                     	, xF86XK_AudioMute), spawn "amixer set Master toggle")
		,((0                     	, xF86XK_Display), spawn "xrandr --output VGA1 --mode 1366x768")
		,((0                     	, xF86XK_Sleep), spawn "pm-suspend")
		,((0                     	, xF86XK_AudioPause), spawn "ncmpcpp toggle")
		,((0                     	, xF86XK_AudioNext), spawn "ncmpcpp next")
		,((0                     	, xF86XK_AudioPrev), spawn "ncmpcpp prev")
		]
		`additionalMouseBindings`
		[((mod1Mask			, 6), (\_ -> moveTo Prev NonEmptyWS))
		,((mod1Mask			, 7), (\_ -> moveTo Next NonEmptyWS))
		,((mod1Mask			, 5), (\_ -> moveTo Next NonEmptyWS))
		,((mod1Mask			, 4), (\_ -> moveTo Prev NonEmptyWS))
		]





myTerminal 	= "urxvtc"
myBitmapsDir	= "~/.xmonad/dzen2/"
--myFont		= "-*-tamsyn-medium-*-normal-*-12-*-*-*-*-*-*-*"
myFont		= "-*-lime-*-*-*-*-*-*-*-*-*-*-*-*"
--myFont		= "xft:inconsolata:size=11"
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

purple0=  "#74636d"
purple1= "#927d9e"

cyan0=  "#556c85"
cyan1= "#6e98b8"

white0=  "#b2b2b2"
white1= "#bdbdbd"

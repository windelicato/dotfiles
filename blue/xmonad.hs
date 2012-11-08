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

-- WINDOW RULES
import XMonad.ManageHook
-- KEYBOARD & MOUSE CONFIG
import XMonad.Util.EZConfig
import XMonad.Actions.FloatKeys
-- STATUS BAR
import XMonad.Hooks.DynamicLog hiding (xmobar, xmobarPP, xmobarColor, sjanssenPP, byorgeyPP)
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.UrgencyHook
import XMonad.Util.Dmenu
--import XMonad.Hooks.FadeInactive
--import XMonad.Hooks.EwmhDesktops
import System.IO (hPutStrLn)
--import XMonad.Operations
import XMonad.Util.Run (spawnPipe)
import XMonad.Actions.CycleWS			-- nextWS, prevWS





defaultLayouts =	  onWorkspace (myWorkspaces !! 0) (avoidStruts (Circle ||| tiled ||| fullTile ||| fullTile3))
			$ onWorkspace (myWorkspaces !! 1) (fullScreen ||| avoidStruts (tiled))
			$ onWorkspace (myWorkspaces !! 2) (avoidStruts simplestFloat)
			$ avoidStruts ( tiled   ||| fullTile ||| fullTile3 ||| Circle ||| simplestFloat) ||| fullScreen 
	where
		tiled  		= spacing 30 $ ResizableTall nmaster delta (1/3) [] 
		tile3		= spacing 30 $ ThreeColMid nmaster delta (1/3)
		fullScreen 	= noBorders(fullscreenFull Full)
		fullTile 	= ResizableTall nmaster delta (1/3) [] 
		fullTile3	=  ThreeColMid nmaster delta (1/3)

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
myWorkspaces = 	[" ä "
		," ê "
		," í "
		," å "
		," é "]


myManageHook = composeAll 	[ resource =? "dmenu" --> doFloat
				, resource =? "skype" 	--> doFloat
				, resource =? "mplayer"	--> doFloat
				, resource =? "chromium"--> doShift (myWorkspaces !! 1)
				]
newManageHook = myManageHook <+> manageHook defaultConfig

myLogHook h = dynamicLogWithPP ( defaultPP
	{
		  ppCurrent	= dzenColor background cyan1 . pad
		, ppVisible	= dzenColor cyan1 background . pad
		, ppHidden	= dzenColor cyan1 background . pad
		, ppSep		= "  "
		, ppLayout	= dzenColor foreground background .
				(\x -> case x of
					"Full"				->	"^i(/home/sunn/.xmonad/dzen2/layout_full.xbm)"
					"Spacing 30 ResizableTall"	->	"^i(/home/sunn/.xmonad/dzen2/layout_tall.xbm)"
					"ResizableTall"			->	"^i(/home/sunn/.xmonad/dzen2/layout_tall.xbm)"
					"SimplestFloat"			->	"^i(/home/sunn/.xmonad/dzen2/arch.xbm)"
					"Circle"			->	"^i(/home/sunn/.xmonad/dzen2/full.xbm)"
					_				->	"^i(/home/sunn/.xmonad/dzen2/grid.xbm)"
				)
--		, ppTitle	=   (" " ++) . dzenColor "#d0d0d0" "#170F0D" . dzenEscape
		, ppOrder	=  \(ws:t:_:_) -> [ws,t]
		, ppOutput	=   hPutStrLn h
	} )

myXmonadBar = "dzen2 -x '0' -y '0' -h '15' -w '500' -ta 'l' -fg '"++foreground++"' -bg '"++background++"' -fn "++myFont
myStatusBar = "conky -c /home/sunn/.xmonad/.conky_dzen | dzen2 -x '500' -w '866' -h '15' -ta 'r' -bg '"++background++"' -fg '"++foreground++"' -y '0' -fn "++myFont


main = do
	dzenLeftBar 	<- spawnPipe myXmonadBar
	dzenRightBar	<- spawnPipe myStatusBar
	xmonad $ defaultConfig
		{ terminal		= myTerminal
		, borderWidth		= 2
		, normalBorderColor 	= black0
		, focusedBorderColor  	= cyan1
		, modMask 		= mod1Mask
		, layoutHook 		= myLayout
		, workspaces 		= myWorkspaces
		, manageHook		= newManageHook
		, logHook		= myLogHook dzenLeftBar -- >> fadeInactiveLogHook 0xdddddddd
		}
		`additionalKeys`
		[((mod1Mask .|. shiftMask	, xK_b), spawn "chromium")
		,((mod1Mask  			, xK_b), spawn "dwb")
		,((mod1Mask  			, xK_n), spawn "urxvt")
		,((mod1Mask  			, xK_z), spawn "zathura")
		,((mod1Mask 			, xK_r), spawn "gmrun")
		,((mod1Mask 			, xK_p), spawn "dmenu_run -b -fn '-*-lime-*-*-*-*-*-*-*-*-*-*-*-*'")
		,((mod1Mask .|. shiftMask	, xK_x), kill)
		,((mod1Mask .|. shiftMask	, xK_c), return())
		,((mod1Mask  			, xK_c), moveTo Next EmptyWS)
		,((mod1Mask .|. shiftMask	, xK_l), sendMessage MirrorShrink)
		,((mod1Mask .|. shiftMask	, xK_h), sendMessage MirrorExpand)
		,((mod1Mask .|. shiftMask	, xK_h), sendMessage MirrorExpand)
		,((mod1Mask .|. shiftMask	, xK_q), sendMessage MirrorExpand)
		,((mod1Mask  			, xK_a), withFocused (keysMoveWindow (-20,0)))
		,((mod1Mask  			, xK_s), withFocused (keysMoveWindow (0,-20)))
		,((mod1Mask  			, xK_d), withFocused (keysMoveWindow (0,20)))
		,((mod1Mask  			, xK_f), withFocused (keysMoveWindow (20,0)))
		,((mod1Mask .|. shiftMask  	, xK_a), withFocused (keysResizeWindow (-20,0) (0,0)))
		,((mod1Mask .|. shiftMask  	, xK_s), withFocused (keysResizeWindow (0,-20) (0,0)))
		,((mod1Mask .|. shiftMask  	, xK_d), withFocused (keysResizeWindow (0,20) (0,0)))
		,((mod1Mask .|. shiftMask  	, xK_f), withFocused (keysResizeWindow (20,0) (0,0)))
		,((0                     	, 0x1008FF11), spawn "amixer set Master 2-")
		,((0                     	, 0x1008FF13), spawn "amixer set Master 2+")
		,((0                     	, 0x1008FF12), spawn "amixer set Master toggle")
		,((0                     	, 0x1008FF59), spawn "xrandr --output VGA1 --mode 1366x768")
		,((0                     	, 0x1008FF59), spawn "pm-suspend")
		,((0                     	, 0x1008FF14), spawn "ncmpcpp toggle")
		,((0                     	, 0x1008FF17), spawn "ncmpcpp next")
		,((0                     	, 0x1008FF16), spawn "ncmpcpp prev")
		]
		`additionalMouseBindings`
		[((mod1Mask			, 6), (\_ -> moveTo Prev NonEmptyWS))
		,((mod1Mask			, 7), (\_ -> moveTo Next NonEmptyWS))
		,((mod1Mask			, 5), (\_ -> moveTo Next NonEmptyWS))
		,((mod1Mask			, 4), (\_ -> moveTo Prev NonEmptyWS))
		]




myTerminal 	= "urxvt"
myBitmapsDir	= "~/.xmonad/dzen2/"
myFont		= "-*-lime-*-*-*-*-*-*-*-*-*-*-*-*"


-- background= "#202020"
-- background= "#291D21"
-- foreground= "#ddccbb"
-- black0= "#222222"
-- black1= "#666666"
-- red0=  "#cc4747"
-- red1=  "#bf5858"
-- green0=  "#a0cf5d"
-- green1= "#b8d68c"
-- yellow0=  "#e0a524"
-- yellow1= "#edB85c"
-- blue0=  "#4194d9"
-- blue1= "#60aae6"
-- purple0=  "#cc2f6e"
-- purple1= "#db588c"
-- cyan0=  "#6d878d"
-- cyan1=  "#42717b"
-- white0=  "#dedede"
-- white1= "#f2f2f2"

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
cyan1=  "#6e98b8"

white0=  "#b2b2b2"
white1= "#bdbdbd"

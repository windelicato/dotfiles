import XMonad
-- LAYOUTS
import XMonad.Layout.Spacing
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.SimpleFloat
import XMonad.Layout.ThreeColumns
import XMonad.Layout.ResizableTile
-- WINDOW RULES
import XMonad.ManageHook
-- KEYBOARD
import XMonad.Util.EZConfig
-- STATUS BAR
import XMonad.Hooks.DynamicLog hiding (xmobar, xmobarPP, xmobarColor, sjanssenPP, byorgeyPP)
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.UrgencyHook
--import XMonad.Hooks.FadeInactive
--import XMonad.Hooks.EwmhDesktops
import System.IO (hPutStrLn)
--import XMonad.Operations
import XMonad.Util.Run (spawnPipe)
--import XMonad.Actions.CycleWS




defaultLayouts = avoidStruts ( tile3 ||| tiled ||| simpleFloat ||| Full)
	where
		tiled  = spacing 35 $ ResizableTall nmaster delta (1/3) [] 
		tile3	= spacing 35 $ ThreeColMid nmaster delta (1/3)
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
myWorkspaces = ["   dev   ", "   www   ","   float   ","   doc   ","   rand   "]

myManageHook = composeAll 	[ resource =? "dmenu" --> doFloat
				, resource =? "skype" 	--> doFloat
				, resource =? "mplayer"	--> doFloat
				]
newManageHook = myManageHook <+> manageHook defaultConfig

-- myLogHook :: Handle -> X ()
myLogHook h = dynamicLogWithPP ( defaultPP
	{
		  ppCurrent	= dzenColor "#170F0D" "#746C48" . pad
		, ppVisible	= dzenColor "#746C48" "#170F0D" . pad
		, ppHidden	= dzenColor "#746C48" "#170F0D" . pad
		, ppSep		= "   "
		, ppLayout	= dzenColor "#a0a57e" "#170F0D" .
				(\x -> case x of
					"Full"		->	"^i(/home/sunn/.xmonad/dzen2/full.xbm)"
					"ThreeColMid"	->	"^i(/home/sunn/.xmonad/dzen2/grid.xbm)"
					_		->	"^i(/home/sunn/.xmonad/dzen2/shroom.xbm)"
				)
--		, ppTitle	=   (" " ++) . dzenColor "#d0d0d0" "#170F0D" . dzenEscape
		, ppOrder	=  \(ws:t:_:_) -> [ws,t]
		, ppOutput	=   hPutStrLn h
	} )

myXmonadBar = "dzen2 -x '0' -y '0' -h '19' -w '960' -ta 'l' -fg '#d0d0d0' -bg '#170F0D' -fn "++myFont
myStatusBar = "conky -c /home/sunn/.xmonad/.conky_dzen | dzen2 -x '960' -w '960' -h '19' -ta 'r' -bg '#170F0D' -fg '#d0d0d0' -y '0' -fn "++myFont


main = do
	dzenLeftBar 	<- spawnPipe myXmonadBar
	dzenRightBar	<- spawnPipe myStatusBar
	xmonad $ defaultConfig
		{ terminal		= myTerminal
		, borderWidth		= 1
		, normalBorderColor 	= background
		, focusedBorderColor  	= "#7F4546"
		, modMask 		= mod1Mask
		, layoutHook 		= myLayout
		, workspaces 		= myWorkspaces
		, manageHook		= newManageHook
		, logHook		= myLogHook dzenLeftBar -- >> fadeInactiveLogHook 0xdddddddd
		}`additionalKeys`
		[((mod1Mask .|. shiftMask, xK_b), spawn "chromium")
		,((mod1Mask  , xK_b), spawn "luakit")
		,((mod1Mask  , xK_z), spawn "zathura")
		,((mod1Mask .|. shiftMask, xK_x), kill)
		,((mod1Mask , xK_p), spawn "gmrun")
		,((mod1Mask .|. shiftMask, xK_c), return())
		,((mod1Mask .|. shiftMask, xK_l), sendMessage MirrorShrink)
		,((mod1Mask .|. shiftMask, xK_h), sendMessage MirrorExpand)
		]




myTerminal 	= "urxvt"
myBitmapsDir	= "~/.xmonad/dzen2/"
-- myFont		= "-*-termsyn.icons-medium-*-normal-*-11-*-*-*-*-*-*-*"
-- myFont		= "-*-smoothansi-*-*-*-*-*-*-*-*-*-*-*-*"
myFont		= "-*-lime-*-*-*-*-*-*-*-*-*-*-*-*"

background	="#170F0D"
foreground	="#746C48"

black0=           "#392925"
black1=           "#544B2E"

red0=		"#98724C"
red1=		"#AF652F"

green0= "#908F32"
green1=	"#C3C13D"

yellow0= "#AA964C"
yellow1= "#C8B55B"

blue0= "#7B854E"
blue1= "#70A16C"

purple0= "#6B5644"
purple1= "#98724C"

cyan0= "#5C5142"
cyan1= "#778725"

white0= "#C8B55B"
white1= "#E4DC8C"


{-
background	="#2c2c2c"
foreground	="#ffffff"
-- Black
black0		="#363636"
black1		="#424242"
-- Red 
red0		="#c37561"
red1		="#d19485"
-- Green
green0		="#a0a57e"
green1		="#b6d99d"
-- Yellow
yellow0		="#d1a375"
yellow1		="#debc9c"
-- Blue
blue0		="#7985a3"
blue1		="#98a1b9"
-- Purple
purple0		="#ab716d"
purple1		="#be918e"
-- Cyan
cyan0		="#98b9b1"
cyan1		="#cbe6cb"
-- White
white0		="#d0d0d0"
white1		="#e6e6e6"
-}

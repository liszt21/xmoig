import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Layout.Gaps
import XMonad.Layout.Magnifier
import XMonad.Layout.Spacing
import XMonad.Layout.ThreeColumns
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.Loggers
import XMonad.Util.SpawnOnce
import XMonad.Util.Ungrab

main :: IO ()
main =
  xmonad
    . ewmhFullscreen
    . ewmh
    . withEasySB (statusBarProp "xmobar" (pure myXmobarPP)) defToggleStrutsKey
    $ myConfig

myWorkspaces :: [String]
myWorkspaces =
  [
    "☯",
    "☰",
    "☱",
    "☲",
    "☳",
    "☴",
    "☵",
    "☶",
    "☷"
  ]

myXmobarPP :: PP
myXmobarPP =
  def
    { ppSep = magenta " • ",
      ppTitleSanitize = xmobarStrip,
      ppCurrent = wrap " " "" . xmobarBorder "Top" "#8be9fd" 2,
      ppHidden = white . wrap " " "",
      ppHiddenNoWindows = lowWhite . wrap " " "",
      ppUrgent = red . wrap (yellow "!") (yellow "!"),
      ppOrder = \[ws, l, _, wins] -> [ws, l, wins],
      ppExtras = [logTitles formatFocused formatUnfocused]
    }
  where
    formatFocused = wrap (white "[") (white "]") . magenta . ppWindow
    formatUnfocused = wrap (lowWhite "[") (lowWhite "]") . blue . ppWindow

    ppWindow :: String -> String
    ppWindow = xmobarRaw . (\w -> if null w then "untitled" else w) . shorten 30

    blue, lowWhite, magenta, red, white, yellow :: String -> String
    magenta = xmobarColor "#ff79c6" ""
    blue = xmobarColor "#bd93f9" ""
    white = xmobarColor "#f8f8f2" ""
    yellow = xmobarColor "#f1fa8c" ""
    red = xmobarColor "#ff5555" ""
    lowWhite = xmobarColor "#bbbbbb" ""

-- myLayout :: Layout
myLayout =
  gaps [(L, 0), (R, 0), (U, 0), (D, 0)] $
    spacingRaw False (Border 10 0 10 0) True (Border 0 10 0 10) True $
      (tiled ||| Mirror tiled ||| Full ||| threeCol)
  where
    threeCol = magnifiercz' 1.5 $ ThreeColMid nmaster delta ratio
    tiled = Tall nmaster delta ratio
    nmaster = 1
    ratio = 1 / 2
    delta = 3 / 100

myStartupHook = do
  -- spawnOnce "tmux new -d -s xmonad-startup -n main sh ~/Dotfiles/config/startup.sh"
  -- spawnOnce "xautolock -time 6 -locker \"betterlockscreen -l\""
  spawnOnce "picom --config ~/Dotfiles/module/grask/config/picom/picom.conf"
  -- spawnOnce "polybar --config=~/Dotfiles/config/polybar/polybar"

myManageHook :: ManageHook
myManageHook =
  composeAll
    [ className =? "Gimp" --> doFloat,
      isDialog --> doFloat
    ]

-- myConfig :: Config
myConfig =
  def
    { modMask = mod1Mask,
      layoutHook = myLayout,
      manageHook = myManageHook,
      workspaces = myWorkspaces,
      startupHook = myStartupHook,
      terminal = "alacritty",
      borderWidth = 1
    }
    `additionalKeysP` [
                        ("M-S-z", spawn "xscreensaver-command -lock"),
                        ("M-S-=", unGrab *> spawn "scrot -s"),
                        ("M-]", spawn "firefox"),
                        ("M-s", spawn "flameshot gui"),
                        ("M-d", spawn "rofi -show combi"),
                        ("M4-l", spawn "betterlockscreen -l")
                        -- , ("M4-l", spawn "slock")
                        -- , ("M4-l", spawn "xlock")
                        -- , ("M4-l", spawn "i3lock")
                      ]

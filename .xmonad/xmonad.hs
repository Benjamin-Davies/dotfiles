import XMonad
import XMonad.Hooks.ManageDocks
import XMonad.Layout.NoBorders
import XMonad.Layout.Spacing
import XMonad.Layout.Grid
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig
import XMonad.Util.Run(runInTerm)
import System.IO

main = do
  startup
  xmonad conf

startup = do
  spawn "xmodmap ~/.speedswapper"
  spawn $ "feh --bg-fill " ++ background
  spawn "compton"
  spawn "conky"
  spawn "google-chrome --no-startup-window"

conf = docks $ defaultConfig
    { manageHook = manageDocks <+> manageHook defaultConfig
    , layoutHook = avoidStruts . noBorders . smartSpacingWithEdge 10
      $ layoutHook defaultConfig ||| Grid
    , modMask = mod4Mask
    , terminal = term
    } `additionalKeysP` keyBindings

keyBindings =
  -- Control and power
  [ ("M-q", kill)
  , ("M-r", restart "xmonad" True)
  , ("M-<F4>", spawn "poweroff")
  , ("M-z", lockScreen)
  -- Layout
  , ("M-w", sendMessage NextLayout)
  , ("M-b", sendMessage ToggleStruts)
  , ("M-S-<Return>", windows W.swapMaster)
  -- Applications
  , ("M-e", spawn "pcmanfm")
  , ("M-<Return>", spawn term)
  , ("M-c", spawn "google-chrome")
  , ("M-S-c", spawn "lxterminal -e cmatrix")
  , ("M-v", runInTerm "" "nvim")
  , ("M-m", spawn "minecraft-launcher")
  , ("M-S-m", spawn "monodevelop")
  , ("M-<Space>", spawn "dmenu_run")
  -- Sound
  , ("<XF86AudioMute>", spawn "pactl set-sink-mute 0 toggle")
  , ("<XF86AudioLowerVolume>", spawn "pactl set-sink-volume 0 -2%")
  , ("<XF86AudioRaiseVolume>", spawn "pactl set-sink-volume 0 +2%")
  -- Screenshot
  , ("<Print>", spawn "scrot -z")
  , ("C-<Print>", spawn "sleep 0.2; scrot -sz")
  ]

lockScreen = spawn $ "i3lock -i " ++ background

background = "/usr/share/backgrounds/default"
term = "konsole"

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
  spawn "xmodmap ~/.speedswapper"
  spawn "feh --bg-fill /usr/share/backgrounds/default"
  spawn "compton"
  spawn "xmobar"
  spawn "google-chrome --no-startup-window"
  spawn "konsole"
  xmonad $ docks $ defaultConfig
    { manageHook = manageDocks <+> manageHook defaultConfig
    , layoutHook = avoidStruts . noBorders . smartSpacingWithEdge 10
      $ layoutHook defaultConfig ||| Grid
    , modMask = mod4Mask
    , terminal = "konsole"
    } `additionalKeysP`
    [ ("M-q", kill)
    , ("M-w", sendMessage NextLayout)
    , ("M-e", spawn "pcmanfm-qt")
    , ("M-r", restart "xmonad" True)
    , ("M-<Return>", runInTerm "" "zsh")
    , ("M-S-<Return>", windows W.swapMaster)
    , ("M-z", lockScreen)
    , ("M-c", spawn "google-chrome")
    , ("M-v", runInTerm "" "nvim")
    , ("M-b", sendMessage ToggleStruts)
    , ("M-m", spawn "minecraft-launcher")
    , ("M-<Space>", spawn "dmenu_run")
    , ("M-F4", spawn "poweroff")
    , ("<XF86AudioMute>", spawn "pactl set-sink-mute 0 toggle")
    , ("<XF86AudioLowerVolume>", spawn "pactl set-sink-volume 0 -2%")
    , ("<XF86AudioRaiseVolume>", spawn "pactl set-sink-volume 0 +2%")
    , ("<Print>", spawn "scrot")
    , ("C-<Print>", spawn "sleep 0.2; scrot -s")
    ]

lockScreen = spawn "i3lock -i /usr/share/backgrounds/default"

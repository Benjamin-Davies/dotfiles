import XMonad
import XMonad.Hooks.ManageDocks
import XMonad.Layout.NoBorders
import XMonad.Layout.Spacing
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig
import XMonad.Util.Run(spawnPipe)
import System.IO

main = do
  spawnPipe "compton"
  spawnPipe "xmobar"
  xmonad $ docks $ defaultConfig
    { manageHook = manageDocks <+> manageHook defaultConfig
    , layoutHook = avoidStruts . noBorders . smartSpacingWithEdge 10
      $ layoutHook defaultConfig
    , modMask = mod4Mask
    } `additionalKeysP`
    [ ("M-q", kill)
    , ("M-w", sendMessage NextLayout)
    , ("M-e", spawn "pcmanfm-qt")
    , ("M-r", restart "xmonad" True)
    , ("M-<Return>", spawn term)
    , ("M-S-<Return>", windows W.swapMaster)
    , ("M-z", spawn "xscreensaver-command -lock")
    , ("M-x", spawn "poweroff")
    , ("M-c", spawn "google-chrome")
    , ("M-v", spawn (term <+> " nvim"))
    , ("M-b", sendMessage ToggleStruts)
    , ("M-m", spawn "minecraft-launcher")
    , ("M-<Space>", spawn "dmenu_run")
    , ("<Print>", spawn "scrot")
    , ("C-<Print>", spawn "sleep 0.2; scrot -s")
    ]

term = "x-terminal-emulator" 

import XMonad
import XMonad.Layout.NoBorders
import XMonad.Layout.Spacing
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig
import System.IO

main = do
    xmonad $ defaultConfig
        { layoutHook = noBorders . smartSpacingWithEdge 10 $ layoutHook defaultConfig
        , modMask = mod4Mask
        } `additionalKeysP`
        [ ("M-q", kill)
        , ("M-w", sendMessage NextLayout)
        , ("M-e", spawn "pcmanfm-qt")
        , ("M-r", restart "xmonad" True)
        , ("M-<Return>", spawn term)
        , ("M-S-<Return>", windows W.swapMaster)
        , ("M-z", spawn "xscreensaver-command -lock")
        , ("M-c", spawn "google-chrome")
        , ("M-v", spawn (term <+> " nvim"))
        , ("M-m", spawn "minecraft-launcher")
        , ("M-<Space>", spawn "dmenu_run")
        , ("<Print>", spawn "scrot")
        , ("C-<Print>", spawn "sleep 0.2; scrot -s")
        ]

term = "x-terminal-emulator" 

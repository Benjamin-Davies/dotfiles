import Control.Exception
import qualified Data.Map as M
import Graphics.X11.ExtraTypes.XF86
import System.IO
import XMonad
import qualified XMonad.StackSet as W

main = do
  startup
  xmonad conf

ignore _ = return ()

startup = do
  spawn "xmodmap ~/.speedswapper"
--  spawn $ "feh --bg-fill " ++ background
--  spawn "compton"
--  spawn "conky"
--  spawn "google-chrome --no-startup-window"

conf = defaultConfig
    { modMask = mod4Mask
    , terminal = term
    } `additionalKeys` keyBindings

keyBindings =
  -- Control and power
  [ ((cmdMask, xK_q),   kill)
  , ((cmdMask, xK_r),   restart "xmonad" True)
  , ((cmdMask, xK_F4),  spawn "poweroff")
  , ((cmdMask, xK_z),   lockScreen)
  -- Layout
  , ((cmdMask,                xK_w),      sendMessage NextLayout)
  , ((cmdMask .|. shiftMask,  xK_Return), windows W.swapMaster)
  -- Applications
  , ((cmdMask, xK_e),               spawn "pcmanfm")
  , ((cmdMask, xK_Return),          spawn term)
  , ((cmdMask, xK_c),               spawn "google-chrome")
  , ((cmdMask .|. shiftMask, xK_c), spawn "lxterminal -e cmatrix")
  , ((cmdMask, xK_v),               runInTerm "" "nvim")
  , ((cmdMask, xK_m),               spawn "minecraft-launcher")
  , ((cmdMask .|. shiftMask, xK_m), spawn "monodevelop")
  , ((cmdMask, xK_space),           spawn "dmenu_run")
  -- Sound
  , ((0, xF86XK_AudioMute),         spawn "pactl set-sink-mute 0 toggle")
  , ((0, xF86XK_AudioLowerVolume),  spawn "pactl set-sink-volume 0 -2%")
  , ((0, xF86XK_AudioRaiseVolume),  spawn "pactl set-sink-volume 0 +2%")
  -- Screenshot
  , ((0,            xK_Print), spawn "scrot -z")
  , ((controlMask,  xK_Print), spawn "sleep 0.2; scrot -sz")
  ]

cmdMask = mod4Mask

lockScreen = spawn $ "i3lock -i " ++ background

background = "/usr/share/backgrounds/default"
term = "konsole"
runInTerm options command = spawn $ term ++ " " ++ options ++ " -e " ++ command

additionalKeys conf keyList =
  conf { keys = \cnf -> M.union (M.fromList keyList) (keys conf cnf) }

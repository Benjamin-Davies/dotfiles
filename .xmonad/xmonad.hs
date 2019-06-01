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
  spawn $ "hsetroot -solid \"#1B2B34\""
  spawn "compton"
  spawn "google-chrome --no-startup-window"

conf = docks $ defaultConfig
    { layoutHook = noBorders $ Tall 1 0.05 0.55 ||| Full
    , modMask = mod4Mask
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
  , ("M-e", spawn "nemo")
  , ("M-y", openInBrowser 0 "https://www.youtube.com/")
  , ("M-\\", runInTerm "" "zsh -c 'tmux attach || tmux'")
  , ("M-<Return>", spawn term)
  , ("M-c", openInBrowser 0 "https://benjamin-davies.github.io/newtab/")
  , ("M-S-c", runInTerm "" "cmatrix")
  , ("M-s", openInBrowser 1 "https://moodle.mmc.school.nz/login/index.php")
  , ("M-S-s", spawn "spotify")
  , ("M-v", runInTerm "" "nvim")
  , ("M-S-v", spawn "code")
  , ("M-m", spawn "minecraft-launcher")
  , ("M-S-m", spawn "monodevelop")
  , ("M-<Space>", spawn "dmenu_run")
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
openInBrowser profile site = spawn $ "google-chrome --profile-directory='" ++ profileDir profile ++ "' --new-window " ++ site

profileDir :: Integer -> [Char]
profileDir 0        = "Default"
profileDir profile  = "Profile " ++ show profile

background = "/usr/share/backgrounds/default"
term = "konsole"
runInTerm options command = spawn $ term ++ " " ++ options ++ " -e " ++ command

additionalKeys conf keyList =
  conf { keys = \cnf -> M.union (M.fromList keyList) (keys conf cnf) }

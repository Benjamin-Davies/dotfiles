import XMonad
import XMonad.Hooks.ManageDocks
import XMonad.Layout.NoBorders
import XMonad.Layout.Spacing
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig
import XMonad.Util.Run(runInTerm)
import System.IO

main = do
  startup
  xmonad conf

startup = do
  spawn "xmodmap ~/.speedswapper"
  spawn $ "hsetroot -solid \"#1B2B34\""
  spawn "compton"
  spawn "google-chrome --no-startup-window"

conf = docks $ defaultConfig
    { layoutHook = noBorders $ Tall 1 0.05 0.55 ||| Full
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
  , ("M-e", spawn "nemo")
  , ("M-y", openInBrowser 0 "https://www.youtube.com/")
  , ("M-\\", runInTerm "" "zsh -c 'tmux attach || tmux'")
  , ("M-<Return>", spawn term)
  , ("M-c", openInBrowser 0 "https://benjamin-davies.github.io/newtab/")
  , ("M-S-c", spawn "cool-retro-term -e zsh -c cmatrix")
  , ("M-s", openInBrowser 1 "https://moodle.mmc.school.nz/login/index.php")
  , ("M-S-s", spawn "spotify")
  , ("M-v", runInTerm "" "nvim")
  , ("M-S-v", spawn "code")
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
openInBrowser profile site = spawn $ "google-chrome --profile-directory='" ++ profileDir profile ++ "' --new-window " ++ site

profileDir :: Integer -> [Char]
profileDir 0        = "Default"
profileDir profile  = "Profile " ++ show profile

background = "/usr/share/backgrounds/default"
term = "st"

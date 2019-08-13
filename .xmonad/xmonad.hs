{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

import Control.Exception
import qualified Data.Map as M
import Graphics.X11 (Rectangle(..))
import Graphics.X11.ExtraTypes.XF86
import System.IO
import XMonad
import XMonad.Layout
import qualified XMonad.StackSet as W

main = do
  startup
  xmonad conf

startup = do
  spawn "xmodmap ~/.speedswapper"
  -- Solid background
  --spawn $ "hsetroot -solid \"#1B2B34\""
  -- Picture background
  spawn $ "feh --bg-fill " ++ background
  spawn "compton"
  spawn "TMUX=TMUX albert"
  spawn "google-chrome --no-startup-window"
  spawn "albert"

conf = defaultConfig
  { borderWidth = 0
  , layoutHook = (Gaps 20 10 $ Tall 1 0.05 0.55) ||| Full
  , modMask = mod4Mask
  , terminal = term
  } `additionalKeys` keyBindings

keyBindings =
  -- Control and power
  [ ((cmdMask,                xK_q  ), kill)
  , ((cmdMask,                xK_r  ), restart "xmonad" True)
  , ((cmdMask .|. shiftMask,  xK_r  ), restart "xmonad" False)
  , ((cmdMask,                xK_F4 ), spawn "poweroff")
  , ((cmdMask .|. shiftMask,  xK_l  ), lockScreen)
  -- Layout
  , ((cmdMask,                xK_w),      sendMessage NextLayout)
  , ((cmdMask .|. shiftMask,  xK_Return), windows W.swapMaster)
  -- Applications
  , ((cmdMask,                xK_e), spawn "nemo")
  , ((cmdMask,                xK_y), openInBrowser 0 "https://www.youtube.com/")
  , ((cmdMask,        xK_backslash), runInTerm "" "zsh -c 'tmux attach || tmux'")
  , ((cmdMask,        xK_Return   ), spawn term)
  , ((cmdMask,                xK_c), openInBrowser 0 "chrome://newtab")
  , ((cmdMask .|. shiftMask,  xK_c), runInTerm "" "cmatrix")
  , ((cmdMask,                xK_s), openInBrowser 1 "https://moodle.mmc.school.nz/login/index.php")
  , ((cmdMask .|. shiftMask,  xK_s), spawn "spotify")
  , ((cmdMask,                xK_v), runInTerm "" "nvim")
  , ((cmdMask .|. shiftMask,  xK_v), spawn "code")
  , ((cmdMask,                xK_m), spawn "minecraft-launcher")
  , ((cmdMask .|. shiftMask,  xK_m), spawn "monodevelop")
  , ((cmdMask,        xK_space    ), spawn "albert show")
  -- Sound
  , ((0, xF86XK_AudioMute),         spawn "pactl set-sink-mute 0 toggle")
  , ((0, xF86XK_AudioLowerVolume),  spawn "pactl set-sink-volume 0 -2%")
  , ((0, xF86XK_AudioRaiseVolume),  spawn "pactl set-sink-volume 0 +2%")
  -- Screenshot
  , ((0,            xK_Print), spawn "scrot -z")
  , ((controlMask,  xK_Print), spawn "sleep 0.2; scrot -sz")
  ]

cmdMask = mod4Mask

lockScreen = spawn "dm-tool switch-to-greeter"
openInBrowser profile site = spawn $ "google-chrome --profile-directory='" ++ profileDir profile ++ "' --new-window " ++ site

profileDir :: Integer -> [Char]
profileDir 0        = "Default"
profileDir profile  = "Profile " ++ show profile

background = "/usr/share/backgrounds/default"
term = "st"
runInTerm options command = spawn $ term ++ " " ++ options ++ " -e " ++ command

-- Minimal implementation of additionalKeys
additionalKeys :: XConfig a -> [((KeyMask, KeySym), X ())] -> XConfig a
additionalKeys conf keyList =
  conf { keys = \cnf -> M.union (M.fromList keyList) (keys conf cnf) }

-- Minimal implementation of Gaps layout
data Gaps l a = Gaps Int Int (l a) deriving (Read, Show)
instance LayoutClass l a => LayoutClass (Gaps l) a where
  runLayout (W.Workspace i (Gaps ig og l) ms) r = do
    (rs, l') <- runLayout (W.Workspace i l ms) (withGap og r)
    return ([(a, withGap ig r) | (a, r) <- rs],
            maybe Nothing (Just . (Gaps ig og)) l')

  handleMessage (Gaps ig og l) = fmap (fmap (Gaps ig og)) . handleMessage l
  description (Gaps _ _ l) = "Gaps " ++ description l

withGap :: Int -> Rectangle -> Rectangle
withGap g (Rectangle x y w h) =
  Rectangle (x + fromIntegral g) (y + fromIntegral g)
    (w - 2 * fromIntegral g) (h - 2 * fromIntegral g)

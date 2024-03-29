{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

import Control.Monad (forever, when)
import Data.List (isInfixOf)
import GHC.IO.Handle (hFlush, hGetLine)
import Hooks
import Keybindings
import LaptopMode (tabletModeHook)
import Layouts
import Mousebindings
import Options
import System.Environment (lookupEnv, setEnv)
import Thumbnail
import XMonad
import XMonad.Actions.Navigation2D
import XMonad.Actions.UpdateFocus (adjustEventInput, focusOnMouseMove)
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.Rescreen
import XMonad.Hooks.SetWMName (setWMName)
import XMonad.Util.NamedActions (addDescrKeys, addName, subtitle, xMessage, (^++^))
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.SpawnOnce

-- Startup hook
myStartupHook = do
  -- spawnOnce "polybar top"
  adjustEventInput
  -- enable tap-to-click
  spawnOnce "xinput set-prop 'ELAN1201:00 04F3:3098 Touchpad' 'libinput Tapping Enabled' 1"
  liftIO $ setEnv "QT_QPA_PLATFORMTHEME" "qt5ct"
  -- tabletModeHook
  -- only call the function, when the environment variable "XMONAD_TEST_MODE" is set
  test_mode <- liftIO $ lookupEnv "XMONAD_TEST_MODE"
  if test_mode == Just "1"
    then do
      liftIO $ putStrLn "XMONAD has been started in test mode"
    else do
      spawnOnce "light-locker --lock-on-lid"
      spawnOnce "/etc/nixos/scripts/xidlehook.sh"
      -- spawnOnce "xfce4-power-manager --daemon"
      spawnOnce "touchegg &"
      spawnOnce "sudo bluetooth off"
      spawnOnce "autoscreenrotation.sh &"
      spawnOnce "$(echo $(nix eval --raw nixos.polkit_gnome.outPath)/libexec/polkit-gnome-authentication-agent-1)"
      spawnOnce "eval $(gnome-keyring-daemon --start -components=secrets)"
      spawnOnce "export $(dbus-launch)"
      spawnOnce "dbus-update-activation-environment --all"
      spawnOnce "xinput disable \"ThinkPad Extra Buttons\""
      spawnOnce "nitrogen --restore &"
      spawnOnce "export SSH_AUTH_SOCK"
      spawnOnce "batsignal -b -n BAT0"
      spawnOnce "pactl load-module module-bluetooth-policy auto_switch=2"
      spawnOnce "pactl load-module module-bluetooth-discover"
      spawnOnce "pactl load-module module-bluetooth-policy"
      spawnOnce "pactl load-module module-switch-on-connect"
      spawnOnce "pactl load-module module-switch-on-port-available"
  -- liftIO removeOldThumbnails
  spawnOnce "~/.autostart.sh"
  spawnOnce "launch-notification-manager"
  spawnOnce "xfce4-panel --disable-wm-check"
  spawnOnce "xhost +si:localuser:$USER"
  setWMName "LG3D"

-- Main
main = do
  putStrLn "Starting XMonad"
  -- set myTerminal to the value of the environment variable "TERMINAL"
  -- if the environment variable is not set, use "alacritty"
  myTerminal <- lookupEnv "TERMINAL" >>= return . maybe "alacritty" id
  dirs <- getDirectories
  let myConfig =
        ( docks
            $ ewmh
            $ myAdditionalKeys
            $ addDescrKeys ((myModMask, xK_F1), xMessage) myKeys
            $ withNavigation2DConfig myNavigation2DConfig
            $ rescreenHook
              def
                { randrChangeHook = myRandrChangeHook
                , afterRescreenHook = myAfterRescreenHook
                }
            $ def
              { terminal = myTerminal
              , focusFollowsMouse = myFocusFollowsMouse
              , clickJustFocuses = myClickJustFocuses
              , borderWidth = myBorderWidth
              , modMask = myModMask
              , workspaces = myWorkspaces
              , normalBorderColor = myNormalBorderColor
              , focusedBorderColor = myFocusedBorderColor
              , mouseBindings = myMouseBindings
              , layoutHook = myLayout
              , manageHook = myManageHook
              , handleEventHook = myEventHook
              , startupHook = myStartupHook
              , clientMask = myClientMask
              , logHook = myLogHook
              }
        )
   in launch myConfig dirs

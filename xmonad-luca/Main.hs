{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent (forkIO)
import Control.Monad (forever, when)
import DBusServer qualified
import Data.List (isInfixOf)
import GHC.IO.Handle (hFlush, hGetLine)
import Hooks (
  myAfterRescreenHook,
  myClientMask,
  myEventHook,
  myLogHook,
  myManageHook,
  myRandrChangeHook,
 )
import Keybindings (myAdditionalKeys, myKeys)
import LaptopMode (tabletModeHook)
import Layouts (myLayout, myNavigation2DConfig)
import Mousebindings (myMouseBindings)
import Options (
  myBorderWidth,
  myClickJustFocuses,
  myFocusFollowsMouse,
  myFocusedBorderColor,
  myModMask,
  myNormalBorderColor,
  myWorkspaces,
 )
import State qualified
import System.Environment (lookupEnv, setEnv)
import Thumbnail ()
import XMonad (
  Default (def),
  MonadIO (liftIO),
  X,
  XConfig (
    borderWidth,
    clickJustFocuses,
    clientMask,
    extensibleConf,
    focusFollowsMouse,
    focusedBorderColor,
    handleEventHook,
    layoutHook,
    logHook,
    manageHook,
    modMask,
    mouseBindings,
    normalBorderColor,
    startupHook,
    terminal,
    workspaces
  ),
  getDirectories,
  launch,
  xK_F1,
 )
import XMonad.Actions.Navigation2D (withNavigation2DConfig)
import XMonad.Actions.UpdateFocus (adjustEventInput, focusOnMouseMove)
import XMonad.Hooks.EwmhDesktops (ewmh)
import XMonad.Hooks.ManageDocks (docks)
import XMonad.Hooks.Rescreen (
  RescreenConfig (afterRescreenHook, randrChangeHook),
  rescreenHook,
 )
import XMonad.Hooks.SetWMName (setWMName)
import XMonad.Util.ExtensibleState qualified as XS
import XMonad.Util.NamedActions (addDescrKeys, addName, subtitle, xMessage, (^++^))
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.SpawnOnce (spawnOnOnce, spawnOnce)

-- Startup hook
myStartupHook :: X ()
myStartupHook = do
  -- DBUS
  appstate <- State.initialize
  XS.put appstate
  liftIO $ forkIO $ DBusServer.start appstate
  -- Other startup hooks
  adjustEventInput
  liftIO $ do
    setEnv "QT_QPA_PLATFORMTHEME" "qt5ct"
    setEnv "QT_STYLE_OVERRIDE" "kvantum"
  -- enable tap-to-click
  spawnOnce "xinput set-prop 'ELAN1201:00 04F3:3098 Touchpad' 'libinput Tapping Enabled' 1"
  -- tabletModeHook
  spawnOnce "light-locker --lock-on-lid"
  spawnOnce "/etc/nixos/scripts/xidlehook.sh"
  spawnOnce "touchegg &"
  spawnOnce "bluetooth off"
  spawnOnce "autoscreenrotation.sh &"
  spawnOnce "$(echo $(nix eval --raw nixos.polkit_gnome.outPath)/libexec/polkit-gnome-authentication-agent-1)"
  spawnOnce "eval $(gnome-keyring-daemon --start -components=secrets)"
  spawnOnce "export $(dbus-launch)"
  spawnOnce "dbus-update-activation-environment --all"
  spawnOnce "xinput disable \"ThinkPad Extra Buttons\""
  spawnOnce "nitrogen --restore &"
  spawnOnce "batsignal -b -n BAT0"
  -- spawnOnce "com.slack.Slack"
  spawnOnce "/etc/nixos/scripts/autohibernate.sh"
  spawnOnOnce "3" "thunderbird"
  spawnOnce "pactl load-module module-bluetooth-policy auto_switch=2"
  spawnOnce "pactl load-module module-bluetooth-discover"
  spawnOnce "pactl load-module module-bluetooth-policy"
  spawnOnce "pactl load-module module-switch-on-connect"
  spawnOnce "pactl load-module module-switch-on-port-available"
  spawnOnce "picom"
  -- liftIO removeOldThumbnails
  spawnOnce "~/.autostart.sh"
  spawnOnce "~/.local/bin/lucapanel"
  spawnOnce "launch-notification-manager"
  spawnOnce "xhost +si:localuser:$USER"
  setWMName "LG3D"

-- Main
main = do
  putStrLn "Starting XMonad"
  -- DBusServer.start
  -- set myTerminal to the value of the environment variable "TERMINAL"
  -- if the environment variable is not set, use "alacritty"
  myTerminal <- lookupEnv "TERMINAL" >>= return . maybe "alacritty" id
  dirs <- getDirectories
  let myConfig =
        docks
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
  launch myConfig dirs

-- vim: fdm=marker tabstop=2 shiftwidth=2 expandtab
--
-- TODO: use `libinput debug-events` (maybe some other more performant program?) to detect touchscreen gestures
-- Language overrides
-- {{{
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ViewPatterns #-}

-- }}}

-- Imports
-- {{{
-- for some fullscreen events, also for xcomposite in obs.

import System.Environment (lookupEnv)
import XMonad
import XMonad.Actions.Navigation2D
import XMonad.Actions.UpdateFocus (adjustEventInput, focusOnMouseMove)
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.Rescreen
import XMonad.Hooks.SetWMName (setWMName)
import XMonad.Util.NamedActions (addDescrKeys, addName, subtitle, xMessage, (^++^))
import XMonad.Util.SpawnOnce

import Keybindings
import Layouts
import Options
import Mousebindings
import Hooks
import Thumbnail
-- }}}

-- Startup hook
-- {{{
myStartupHook = do
  -- spawnOnce "polybar top"
  adjustEventInput
  -- only call the function, when the environment variable "XMONAD_TEST_MODE" is set
  test_mode <- liftIO $ lookupEnv "XMONAD_TEST_MODE"
  if test_mode == Just "1"
    then do
      liftIO $ putStrLn "XMONAD has been started in test mode"
    else do
      spawnOnce "light-locker --lock-on-lid"
      spawnOnce "/etc/nixos/scripts/xidlehook.sh"
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
      liftIO removeOldThumbnails
  spawnOnce "~/.autostart.sh"
  spawnOnce "launch-notification-manager"
  spawnOnce "xfce4-panel --disable-wm-check"
  spawnOnce "xhost +si:localuser:$USER"
  setWMName "LG3D"
-- }}}

-- Main
-- {{{
main = do
  putStrLn "Starting XMonad"
  -- set myTerminal to the value of the environment variable "TERMINAL"
  -- if the environment variable is not set, use "alacritty"
  myTerminal <- lookupEnv "TERMINAL" >>= return . maybe "alacritty" id
  getDirectories
    >>= launch
      ( docks
      $ ewmh
      $ myAdditionalKeys 
      $ addDescrKeys ((myModMask, xK_F1), xMessage) myKeys
      $ withNavigation2DConfig myNavigation2DConfig
      $ rescreenHook def
        { randrChangeHook = myRandrChangeHook
        , afterRescreenHook = myAfterRescreenHook }
      $ def
        { -- simple stuff
          -- set the terminal variable 
          terminal = myTerminal,
          focusFollowsMouse = myFocusFollowsMouse,
          clickJustFocuses = myClickJustFocuses,
          borderWidth = myBorderWidth,
          modMask = myModMask,
          workspaces = myWorkspaces,
          normalBorderColor = myNormalBorderColor,
          focusedBorderColor = myFocusedBorderColor,
          -- mouse bindings
          mouseBindings = myMouseBindings,
          -- hooks, layouts
          layoutHook = myLayout,
          manageHook = myManageHook,
          handleEventHook = myEventHook,
          startupHook = myStartupHook,
          clientMask = myClientMask,
          logHook = myLogHook
        }
      )

-- }}}

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
  -- only call the function, when the environment variable "XMONAD_TEST_MODE" is set
  test_mode <- liftIO $ lookupEnv "XMONAD_TEST_MODE"
  if test_mode == Just "1"
    then do
      liftIO $ putStrLn "XMONAD has been started in test mode"
    else do
      spawnOnce "light-locker --lock-on-lid"
      spawnOnce "/etc/nixos/scripts/xidlehook.sh"
      spawnOnce "sudo bluetooth off"
      spawnOnce "$(echo $(nix eval --raw nixos.polkit_gnome.outPath)/libexec/polkit-gnome-authentication-agent-1)"
      spawnOnce "xinput disable \"ThinkPad Extra Buttons\""
      spawnOnce "birdtray"
      spawnOnce "nitrogen --restore &"
      spawnOnce "autoscreenrotation.sh &"
      spawnOnce "nm-applet"
      spawnOnce "blueman-applet"
      spawnOnce "export $(dbus-launch)"
      spawnOnce "eval $(gnome-keyring-daemon --daemonize)"
      spawnOnce "export SSH_AUTH_SOCK"
      spawnOnce "batsignal -b -n BAT0"
      spawnOnce "touchegg &"
      spawnOnce "rclone --vfs-cache-mode writes mount \"OnedriveHHU\": ~/OneDrive"
      liftIO removeOldThumbnails
  spawnOnce "polybar top"
  setWMName "LG3D"
  adjustEventInput
-- }}}

-- Main
-- {{{
main = do
  putStrLn "Starting XMonad"
  getDirectories
    >>= launch
      ( docks $
          ewmh $
            myAdditionalKeys $
              addDescrKeys ((myModMask, xK_F1), xMessage) myKeys $
                withNavigation2DConfig myNavigation2DConfig $
                  rescreenHook
                    def
                      { randrChangeHook = myRandrChangeHook,
                        afterRescreenHook = myAfterRescreenHook
                      }
                    $ def
                      { -- simple stuff
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

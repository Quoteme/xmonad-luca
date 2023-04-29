-- vim: fdm=marker tabstop=2 shiftwidth=2 expandtab
--
-- All the "automatic actions" are defined here. A hook is a function that
-- automatically runs when a certain event occurs. For example, when a new
-- window is created, the hook "manageHook" is run.

module Hooks where

import qualified XMonad.Layout.BoringWindows as BW
import qualified XMonad.StackSet as S
import qualified Data.Map        as M
import qualified XMonad.Util.ExtensibleState as XS

import XMonad.Prelude
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ServerMode (serverModeEventHookF)
import XMonad.Actions.Navigation2D
import XMonad.Actions.UpdateFocus ( adjustEventInput, focusOnMouseMove )
import XMonad.Actions.WindowMenu (windowMenu)
import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.LayoutHints
import XMonad.Util.Hacks (windowedFullscreenFixEventHook)
import XMonad.Actions.UpdatePointer (updatePointer)
import XMonad.Hooks.ManageHelpers (doRectFloat, isDialog, doCenterFloat)
import XMonad.Actions.CycleWindows (rotUnfocusedUp, rotUnfocusedDown)

import Utilities
import XMonad
import Thumbnail
import Text.Format
import Keybindings
import LaptopMode

-- {{{ The client events that xmonad is interested in
myClientMask = focusChangeMask .|. clientMask def
-- }}}

-- {{{ Manage hooks
myManageHook = composeAll
  [ appName =? "control_center" --> doRectFloat (S.RationalRect 0.65 0.05 0.325 0.45)
  , className =? "Xfce4-popup-whiskermenu" --> doCenterFloat -- Center the WhiskerMenu
  , className =? "wrapper-2.0" --> doCenterFloat -- Center the WhiskerMenu
  , className =? "Wrapper-2.0" --> doCenterFloat -- Center the WhiskerMenu
  , appName =? "Whisker Menu" --> doRectFloat (S.RationalRect 0.1 0.05 0.325 0.45)
  , className =? "Onboard" --> doFloat
  , className =? ".blueman-manager-wrapped" --> doRectFloat (S.RationalRect 0.65 0.05 0.325 0.45)
  , isDialog --> doCenterFloat
  ]
-- }}}

-- {{{ Event hook
myEventHook = focusOnMouseMove
            <+> hintsEventHook
            <+> windowedFullscreenFixEventHook
            <+> notificationsOnTop
            <+> serverModeEventHookF "XMONAD_COMMAND" defaultServerCommands
            <+> serverModeEventHookF "LAYOUT" layoutServerCommands
            <+> serverModeEventHookF "WINDOW" windowServerCommands
            <+> serverModeEventHookF "WORKSPACE" workspaceServerCommands
              where
                defaultServerCommands :: String -> X ()
                defaultServerCommands "menu"               = windowMenu
                defaultServerCommands "swap-up"            = windowSwap U False
                defaultServerCommands "swap-down"          = windowSwap D False
                defaultServerCommands "swap-left"          = windowSwap L False
                defaultServerCommands "swap-right"         = windowSwap R False
                defaultServerCommands "rotate"             = sendMessage Rotate
                defaultServerCommands "layout-next"        = sendMessage NextLayout
                defaultServerCommands "layout-tablet"      = setLaptopMode TabletMode 
                defaultServerCommands "layout-normal"      = setLaptopMode LaptopMode 
                defaultServerCommands "layout-get"         = sendMessage ToggleStruts
                defaultServerCommands "toggle-struts"      = sendMessage ToggleStruts
                defaultServerCommands "select-to-maximize" = selectMaximizeWindow
                defaultServerCommands "workspace-add"      = Keybindings.addLastWorkspace
                defaultServerCommands "workspace-remove"   = Keybindings.removeLastWorkspace
                layoutServerCommands :: String -> X ()
                layoutServerCommands layout = sendMessage $ JumpToLayout layout
                windowServerCommands :: String -> X ()
                windowServerCommands "prev" = windows S.focusDown >> updatePointer (0.5, 0.5) (0.0, 0.0)
                windowServerCommands "next" = windows S.focusUp >> updatePointer (0.5, 0.5) (0.0, 0.0)
                windowServerCommands "swapPrev" = windows S.swapDown >> updatePointer (0.5, 0.5) (0.0, 0.0)
                windowServerCommands "swapNext" = windows S.swapUp >> updatePointer (0.5, 0.5) (0.0, 0.0)
                windowServerCommands "centerMouse" = windows S.focusDown
                                                  >> updatePointer (0.5, 0.5) (0.0, 0.0)
                                                  >> windows S.focusUp
                                                  >> updatePointer (0.5, 0.5) (0.0, 0.0)
                windowServerCommands "rotate-unfocused-up" = rotUnfocusedUp
                windowServerCommands "rotate-unfocused-down" = rotUnfocusedDown
                -- | switch to workspace `workspacename`
                workspaceServerCommands :: String -> X ()
                workspaceServerCommands workspacename = windows $ S.greedyView workspacename
                notificationsOnTop :: Event -> X All
                notificationsOnTop (AnyEvent {ev_event_type = et}) = do
                  when (et == focusOut) $ do
                    spawn "xdotool windowraise `xdotool search --all --name Dunst`"
                    spawn "xdotool windowfocus `xdotool search --all --name xfce4-notifyd`"
                  return $ All True
                notificationsOnTop (FocusChangeEvent {}) = do
                  spawn "xdotool windowraise `xdotool search --all --name Dunst`"
                  spawn "xdotool windowraise `xdotool search --all --name xfce4-notifyd`"
                  return $ All True
                notificationsOnTop ev = return $ All True
-- }}}

-- {{{ Log hook
myLogHook :: X ()
myLogHook = do
  -- thumbnailhook disabled, because I do not use it that much (maybe this saves battery?)
  -- thumbnailHook
  return ()
  where
    thumbnailHook :: X ()
    thumbnailHook = do
      workspacename <- gets (S.tag . S.workspace . S.current . windowset)
      createThumbnailIfNeccessary workspacename 20
      where
        createThumbnailIfNeccessary :: String -> Int -> X ()
        createThumbnailIfNeccessary workspaceName quality = do
          shouldSave <- shouldSaveNewThumbnail 5
          when shouldSave $ createThumbnail workspaceName quality
        createThumbnail :: String -> Int -> X ()
        createThumbnail workspaceName quality = do
          -- TODO: Write this to ram
          spawn $ format "import -window root -resize {0}% $XDG_CACHE_HOME/xmonad_workspace_thumbnail-{1}.png" [(show quality), workspaceName]
-- }}}

-- {{{ Screen / rander change hooks
myAfterRescreenHook :: X ()
myAfterRescreenHook = do
  spawn "xinput --map-to-output 'ELAN9008:00 04F3:2C82' eDP"
  spawn "xinput --map-to-output 'ELAN9008:00 04F3:2C82 Stylus Pen (0)' eDP"
  spawn "xinput --map-to-output 'ELAN9008:00 04F3:2C82 Stylus Eraser (0)' eDP"

myRandrChangeHook :: X ()
myRandrChangeHook = do
  spawn "nitrogen --restore"
  spawn "xinput --map-to-output 'ELAN9008:00 04F3:2C82' eDP"
  spawn "xinput --map-to-output 'ELAN9008:00 04F3:2C82 Stylus Pen (0)' eDP"
  spawn "xinput --map-to-output 'ELAN9008:00 04F3:2C82 Stylus Eraser (0)' eDP"

-- }}}

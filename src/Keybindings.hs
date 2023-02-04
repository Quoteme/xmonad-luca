-- vim: fdm=marker tabstop=2 shiftwidth=2 expandtab
--
-- This file is responsible for the keybindings in xmonad-luca
--

module Keybindings where

import XMonad
import XMonad.Prelude
import XMonad.Util.SpawnOnce
import XMonad.Util.Run(spawnPipe, runProcessWithInput)
import XMonad.Util.EZConfig
import XMonad.Hooks.DynamicLog (xmobarAction)
import XMonad.Hooks.EwmhDesktops  -- for some fullscreen events, also for xcomposite in obs.
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.Place (placeHook, withGaps, smart, underMouse, fixed)
import XMonad.Hooks.SetWMName (setWMName)
import XMonad.Hooks.ServerMode (serverModeEventHookF)
import XMonad.Actions.Navigation2D
import XMonad.Actions.UpdateFocus ( adjustEventInput, focusOnMouseMove )
import XMonad.Actions.WindowMenu (windowMenu)
import XMonad.Actions.EasyMotion (selectWindow)
import XMonad.Layout.Renamed
import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.NoBorders (smartBorders, noBorders)
import XMonad.Layout.BorderResize (borderResize)
import XMonad.Layout.DecorationMadness ( mirrorTallSimpleDecoResizable, shrinkText )
import XMonad.Layout.Spiral (spiral)
import XMonad.Layout.LayoutHints
import XMonad.Layout.WindowSwitcherDecoration
import Data.Maybe (isJust, fromJust)
import Data.List (elemIndex)
import Data.List.NonEmpty (NonEmpty(..), nonEmpty)
import System.Exit
import System.IO (hPutStrLn)
import Graphics.X11.ExtraTypes.XF86
import qualified XMonad.StackSet as S
import qualified Data.Map        as M
import qualified XMonad.Util.ExtensibleState as XS
import XMonad.Layout.DraggingVisualizer
import XMonad.Layout.ImageButtonDecoration
import XMonad.Util.NamedActions (addDescrKeys, xMessage, addName, (^++^), subtitle)
import XMonad.Util.Hacks (windowedFullscreenFixEventHook)
import XMonad.Layout.Hidden
import XMonad.Actions.UpdatePointer (updatePointer)
import XMonad.Layout.Decoration
import XMonad.Actions.OnScreen (viewOnScreen)
import XMonad.Actions.DynamicWorkspaces (appendWorkspacePrompt, removeEmptyWorkspace, selectWorkspace, withNthWorkspace, addWorkspace, removeWorkspace, removeWorkspaceByTag)
import XMonad.Prompt (amberXPConfig)
import XMonad.Hooks.Rescreen
import XMonad.Layout.Magnifier (magnifier)
import XMonad.Layout.DecorationAddons (handleScreenCrossing)
import Control.Monad (unless)
import Text.Format (format)
import XMonad.Util.Image (Placement(..))
import XMonad.Layout.Minimize (minimize)
import qualified XMonad.Layout.BoringWindows as BW
import XMonad.Actions.Minimize (withMinimized, maximizeWindow, minimizeWindow)
import XMonad.Actions.GridSelect (gridselect)
import XMonad.Layout.Maximize (maximize, maximizeRestore)
import Control.Concurrent (threadDelay)
import System.Process (readProcess)
import XMonad.Actions.CopyWindow (copyToAll, killAllOtherCopies, kill1)
import XMonad.Hooks.ManageHelpers (doRectFloat, isDialog, doCenterFloat)
import XMonad.Layout.Reflect (reflectVert, reflectHoriz)
import XMonad.Actions.CycleWindows (rotUnfocusedUp, rotUnfocusedDown)
import Data.Time.Clock.POSIX (getPOSIXTime, POSIXTime)
import System.Directory (getHomeDirectory, listDirectory, removeFile)
import System.Environment (lookupEnv)
import Utilities (selectMaximizeWindow)
import Options
import LaptopMode


-- {{{ My own keybindings
myKeys config = (subtitle "Custom Keys":) $ mkNamedKeymap config $
  -- {{{ Legend on how to use modifiers
  -- Code | Key
  -- M    | super key
  -- C    | control
  -- S    | shift
  -- M1   | alt
  -- M2   | num lock
  -- M3   | 
  -- M4   | super
  -- }}}
  -- {{{ 🚀 Launch Programs
  [ ("M-<Return>"              , addName "Spawn Terminal" $ spawn $ terminal config)
  , ("M-d"                     , addName "Open program launcher" $ spawn "rofi -show combi -show-icons")
  , ("M-w"                     , addName "Search open window" $ spawn "rofi -show window")
  , ("M-e"                     , addName "Open emoji selector" $ spawn "rofimoji")
  , ("M-S-w"                   , addName "Open network settings" $ spawn "networkmanager_dmenu")
  , ("M-S-s"                   , addName "Screenshot" $ spawn "flameshot gui")
  , ("M-S-C-s"                 , addName "Simple screenshot" $ spawn "maim -su | xclip -selection clipboard -t image/png")
  , ("M-S-q"                   , addName "Kill window" $ kill)
  , ("M-<Space>"               , addName "Layout: next" $ sendMessage NextLayout)
  , ("M-S-<Space>"             , addName "Layout: default" $ setLayout $ layoutHook config)
  -- }}}
  -- {{{ 🔄 Rotational Focus Movement
  , ("M-<Tab>"                 , addName "WindowStack: rotate next" $ windows S.focusDown   >> myUpdateFocus)
  , ("M-S-<Tab>"               , addName "WindowStack: rotate previous" $ windows S.focusUp >> myUpdateFocus)
  , ("M-C-<Tab>"               , addName "WindowStack: swap next" $ windows S.swapDown      >> myUpdateFocus)
  , ("M-C-S-<Tab>"             , addName "WindowStack: swap previous" $ windows S.swapUp    >> myUpdateFocus)
  -- }}}
  -- {{{ 🔎 Easymotion
  , ("M-f"                     , addName "Easymotion: focus" $ selectWindow def >>= (`whenJust` windows . S.focusWindow) >> myUpdateFocus)
  , ("M-C-f"                   , addName "Easymotion: kill" $ selectWindow def >>= (`whenJust` killWindow))
  -- }}}
  -- {{{ 🏃 Directional Focus Movement
  , ("M-h"                     , addName "Focus: left"   $ windowGo L False      >> myUpdateFocus)
  , ("M-j"                     , addName "Focus: down"   $ windowGo D False      >> myUpdateFocus)
  , ("M-k"                     , addName "Focus: up"     $ windowGo U False      >> myUpdateFocus)
  , ("M-l"                     , addName "Focus: right"  $ windowGo R False      >> myUpdateFocus)
  , ("M-<Left>"                , addName "Focus: left"   $ windowGo L False      >> myUpdateFocus)
  , ("M-<Down>"                , addName "Focus: down"   $ windowGo D False      >> myUpdateFocus)
  , ("M-<Up>"                  , addName "Focus: up"     $ windowGo U False      >> myUpdateFocus)
  , ("M-<Right>"               , addName "Focus: right"  $ windowGo R False      >> myUpdateFocus)
  , ("M-m"                     , addName "Focus: master" $ windows S.focusMaster >> myUpdateFocus)
  -- }}}
  -- {{{ 🔀 Directional Window Movement
  , ("M-S-h"                   , addName "Swap: left"   $ windowSwap L False   >> myUpdateFocus)
  , ("M-S-j"                   , addName "Swap: down"   $ windowSwap D False   >> myUpdateFocus)
  , ("M-S-k"                   , addName "Swap: up"     $ windowSwap U False   >> myUpdateFocus)
  , ("M-S-l"                   , addName "Swap: right"  $ windowSwap R False   >> myUpdateFocus)
  , ("M-S-<Left>"              , addName "Swap: left"   $ windowSwap L False   >> myUpdateFocus)
  , ("M-S-<Down>"              , addName "Swap: down"   $ windowSwap D False   >> myUpdateFocus)
  , ("M-S-<Up>"                , addName "Swap: up"     $ windowSwap U False   >> myUpdateFocus)
  , ("M-S-<Right>"             , addName "Swap: right"  $ windowSwap R False   >> myUpdateFocus)
  , ("M-S-m"                   , addName "Swap: master" $ windows S.swapMaster >> myUpdateFocus)
  -- }}}
  -- {{{ Window resizing
  , ("M-C-h"                   , addName "Expand: left" $ sendMessage $ ExpandTowards L)
  , ("M-C-j"                   , addName "Expand: down" $ sendMessage $ ExpandTowards D)
  , ("M-C-k"                   , addName "Expand: up" $ sendMessage $ ExpandTowards U)
  , ("M-C-l"                   , addName "Expand: right" $ sendMessage $ ExpandTowards R)
  , ("M-C-<Left>"              , addName "Expand: left" $ sendMessage $ ExpandTowards L)
  , ("M-C-<Down>"              , addName "Expand: down" $ sendMessage $ ExpandTowards D)
  , ("M-C-<Up>"                , addName "Expand: up" $ sendMessage $ ExpandTowards U)
  , ("M-C-<Right>"             , addName "Expand: right" $ sendMessage $ ExpandTowards R)
  , ("M-M1-h"                  , addName "Expand: left" $ sendMessage $ ShrinkFrom L)
  , ("M-M1-j"                  , addName "Expand: down" $ sendMessage $ ShrinkFrom D)
  , ("M-M1-k"                  , addName "Expand: up" $ sendMessage $ ShrinkFrom U)
  , ("M-M1-l"                  , addName "Expand: right" $ sendMessage $ ShrinkFrom R)
  , ("M-M1-<Left>"             , addName "Expand: left" $ sendMessage $ ShrinkFrom L)
  , ("M-M1-<Down>"             , addName "Expand: down" $ sendMessage $ ShrinkFrom D)
  , ("M-M1-<Up>"               , addName "Expand: up" $ sendMessage $ ShrinkFrom U)
  , ("M-M1-<Right>"            , addName "Expand: right" $ sendMessage $ ShrinkFrom R)
  -- }}}
  -- {{{ Splitting and moving
  , ("M-S-C-k"                 , addName "Split: next" $ sendMessage $ SplitShift Next )
  , ("M-S-C-j"                 , addName "Split: previous" $ sendMessage $ SplitShift Prev)
  -- }}}
  -- {{{ Rotations/Swappings
  , ("M-r"                     , addName "BSP: rotate" $ myUpdateFocus <> sendMessage Rotate)
  , ("M-S-r"                   , addName "BSP: rotate left around parent" $ myUpdateFocus <> sendMessage RotateL)
  , ("M-C-r"                   , addName "BSP: rotate right around parent" $ myUpdateFocus <> sendMessage RotateR)
  , ("M-s"                     , addName "BSP: swap" $ myUpdateFocus <> sendMessage Swap)
  , ("M-n"                     , addName "BSP: focus parent" $ myUpdateFocus <> sendMessage FocusParent)
  , ("M-C-n"                   , addName "BSP: select node" $ sendMessage SelectNode)
  , ("M-S-n"                   , addName "BSP: move node" $ sendMessage MoveNode)
  , ("M-a"                     , addName "BSP: balance" $ sendMessage Balance)
  , ("M-S-a"                   , addName "BSP: equalize" $ sendMessage Equalize)
  -- }}}
  -- {{{ (Un-)Hiding
  , ("M-<Backspace>"           , addName "Window: hide"                         $ withFocused hideWindow *> spawn "notify-send \"hidden a window\"")
  , ("M-S-<Backspace>"         , addName "Window: unhide"                       $ popOldestHiddenWindow >> myUpdateFocus)
  , ("M-S-o"                   , addName "Window: unminimize menu"              $ selectMaximizeWindow)
  , ("M-C-m"                   , addName "Window: maximize"                     $ withFocused (sendMessage . maximizeRestore))
  , ("M-C-m"                   , addName "Window: maximize"                     $ withFocused (sendMessage . maximizeRestore))
  , ("M-c"                     , addName "Window: copy to all other workspaces" $ do
                                                                                  spawn $  "notify-send "
                                                                                        ++ "'window copied to all workspaces' "
                                                                                        ++ "'undo by pressing M-S-c\n"
                                                                                        ++ "do not show on current workspace by pressing M-C-c'"
                                                                                  windows copyToAll)
  , ("M-S-c"                   , addName "Window: delete all other copies"      $ do
                                                                                  killAllOtherCopies
                                                                                  spawn "notify-send 'deleted all other copies'")
  , ("M-C-c"                   , addName "Window: kill current copy of window"  $ do
                                                                                  kill1
                                                                                  spawn "notify-send 'deleted current copy'")
  -- }}}
  -- {{{ Other stuff
  , ("M-S-t"                   , addName "LaptopMode: toggle tablet mode" $ toggleTabletMode)
  , ("M-b"                     , addName "Statusbar: toggle" $ sendMessage ToggleStruts)
  , ("M-t"                     , addName "Window: unfloat" $ withFocused $ windows . S.sink)
  , ("M-,"                     , addName "Master: increase" $ sendMessage (IncMasterN 1))
  , ("M-."                     , addName "Master: decrease" $ sendMessage (IncMasterN (-1)))
  , ("M-o"                     , addName "Window: menu" $ windowMenu)
  -- }}}
  -- {{{ Quitting
  , ("M-<Delete>"              , addName "Xmonad: exit" $ io exitSuccess)
  , ("M-S-<Delete>"            , addName "Xmonad: restart" $ restart "xmonad" True *> spawn "notify-send \"Xmonad: restarted\"")
  -- }}}
  -- {{{ Function Keys
  , ("<XF86MonBrightnessUp>"   , addName "Brightness: Monitor: raise" $ raiseMonBrigthness)
  , ("<XF86MonBrightnessDown>" , addName "Brightness: Monitor: lower" $ lowerMonBrigthness)
  , ("<XF86KbdBrightnessUp>"   , addName "Brightness: Keyboard: raise"$ raiseKbdBrigthness)
  , ("<XF86KbdBrightnessDown>" , addName "Brightness: Keyboard: lower" $ lowerKbdBrigthness)
  , ("<XF86AudioLowerVolume>"  , addName "Volume: raise" $ raiseAudio)
  , ("<XF86AudioRaiseVolume>"  , addName "Volume: lower" $ lowerAudio)
  , ("<XF86AudioMicMute>"      , addName "Microphone: toggle" $ spawn "amixer set Capture toggle")
  , ("<XF86AudioMute>"         , addName "Volume: toggle" $ spawn "pamixer --toggle-mute")
  , ("<XF86AudioNext>"         , addName "Media: next" $ spawn "playerctl next")
  , ("<XF86AudioPrev>"         , addName "Media: previous" $ spawn "playerctl previous")
  , ("<XF86AudioPlay>"         , addName "Media: pause" $ spawn "playerctl play-pause")
  , ("<XF86Launch1>"           , addName "Workspace: preview" $ spawn "xmonad-workspace-preview")
  , ("<XF86Launch3>"           , addName "Select color" $ spawn "xcolor | perl -pe 'chomp if eof' | xclip -selection clipboard")
  , ("<XF86Launch4>"           , addName "Power profile: cycle" $ spawn "powerprofilesctl-cycle")
  -- }}}
  -- {{{ Workspace keys
  , ("M-p"                     , addName "Workspace: preview" $ spawn "xmonad-workspace-preview")
  ] ^++^
  (  [ ("M-"   ++ show n, withNthWorkspace S.greedyView (n-1)) | n <- [0..9] ]
  ++ [ ("M-S-" ++ show n, withNthWorkspace S.shift (n-1)) | n <- [0..9] ]
  ++
  [ (modifier ++ nth_key, windows $ function nth_workspace)
      | (nth_key,  nth_workspace) <- zip (map show [1..9]) (workspaces config)
      , (modifier, function)      <- [  ("M-C-", viewOnScreen 0)
                                     , ("M-M1-", viewOnScreen 1)
                                     --, ("M-", S.greedyView)
                                     -- , ("M-S-", S.shift)
                                     ]
  ]
  )
  -- }}}
  where
    -- Helper functions
    -- {{{
    lowerMonBrigthness :: MonadIO m => m ()
    lowerMonBrigthness =  spawn "brightnessctl set 5%-"
                       *> spawn "notify-send 'Brightness lowered'"
    raiseMonBrigthness :: MonadIO m => m ()
    raiseMonBrigthness =  spawn "brightnessctl set 5%+"
                       *> spawn "notify-send 'Brightness raised'"
    lowerKbdBrigthness :: MonadIO m => m ()
    lowerKbdBrigthness =  spawn "brightnessctl --device=\"asus::kbd_backlight\" set 1-"
                       *> spawn "notify-send 'Brightness lowered'"
    raiseKbdBrigthness :: MonadIO m => m ()
    raiseKbdBrigthness =  spawn "brightnessctl --device=\"asus::kbd_backlight\" set 1+"
                       *> spawn "notify-send 'Brightness raised'"
    lowerAudio :: MonadIO m => m ()
    lowerAudio =  spawn "pamixer --increase 5"
                       *> spawn "notify-send -a \"changeVolume\" -u low -i /etc/nixos/xmonad/icon/high-volume.png \"volume up\""
    raiseAudio :: MonadIO m => m ()
    raiseAudio =  spawn "pamixer --decrease 5"
                       *> spawn "notify-send -a \"changeVolume\" -u low -i /etc/nixos/xmonad/icon/volume-down.png \"volume down\""
    myUpdateFocus = updatePointer (0.5, 0.5) (0.1, 0.1)
  -- }}}
-- }}}

-- {{{ My additional keybindings
-- {{{ 
-- Additional state needed
newtype KeyboardToggleState = KeyboardToggleState Bool deriving (Typeable, Read, Show)
instance ExtensionClass KeyboardToggleState where
  initialValue = KeyboardToggleState False
-- }}}
myAdditionalKeys config = additionalKeys config
  [ ((0                 , xF86XK_TouchpadToggle ), toggleTouchpad)
  , ((0                 , xF86XK_TouchpadOn     ), enableTouchpad)
  , ((0                 , xF86XK_PowerOff       ), spawn "notify-send 'Poweroff' 'button was pressed'")
  -- Thinkpad X201T keys
  , ((0                 , xF86XK_RotateWindows  ), spawn "screenrotation.sh cycle_left")
  , ((0                 , xF86XK_TaskPane       ), spawn "screenrotation.sh swap")
  -- , ((0                 , xF86XK_ScreenSaver    ), spawn "xdotool key super+s")
  -- , ((0                 , xF86XK_Launch1        ), spawn "xdotool key super+r")
  -- Workspaces
-- selectWindow def >>= (`whenJust` windows . S.focusWindow) >> myUpdateFocus
  , ((myModMask                 , xK_numbersign ), selectWorkspace amberXPConfig)
  , ((myModMask .|. shiftMask   , xK_plus       ), appendWorkspacePrompt amberXPConfig)
  , ((myModMask                 , xK_plus       ), addLastWorkspace)
  , ((myModMask                 , xK_minus      ), removeLastWorkspace)
  ]
  where
    toggleTouchpad :: X ()
    toggleTouchpad = do
      KeyboardToggleState state <- XS.get
      if state
        then enableTouchpad
        else disableTouchpad
      XS.put $ KeyboardToggleState $ not state
    enableTouchpad :: MonadIO m => m ()
    enableTouchpad =  spawn "xinput --enable \"ELAN1201:00 04F3:3098 Touchpad\""
                   *> spawn "xinput --enable \"AT Translated Set 2 keyboard\""
                   *> spawn "notify-send 'touchpad enabled'"
    disableTouchpad :: MonadIO m => m ()
    disableTouchpad =  spawn "xinput --disable \"ELAN1201:00 04F3:3098 Touchpad\""
                    *> spawn "xinput --disable \"AT Translated Set 2 keyboard\""
                    *> spawn "notify-send 'touchpad disabled'"

-- Needed for adding workspaces with automatic names
-- {{{
addLastWorkspace :: X ()
addLastWorkspace = do
    -- maybe use xdotool instead of extensible state?
    -- workspaceLen <- liftIO $ (\t -> read t :: Int) <$> readProcess "xdotool" ["get_num_desktops"] []
    workspaceLenString <- runProcessWithInput "xdotool" (["get_num_desktops"]) ""
    let workspaceLen = read workspaceLenString :: Int
    -- spawn $ format "notify-send \"Workspace length increased\" \"now at {0}\"" [show workspaceLen]
    addWorkspace $ show $ workspaceLen + 1
    return ()

removeLastWorkspace :: X ()
removeLastWorkspace = do
    workspaceLenString <- runProcessWithInput "xdotool" (["get_num_desktops"]) ""
    let workspaceLen = read workspaceLenString :: Int
    -- remove old thumbnails
    home <- liftIO getHomeDirectory
    let thumbDir = home ++ "/.cache/"
    files <- liftIO $ listDirectory thumbDir
    let oldFiles = [ f | f <- sort files, "xmonad_workspace_thumbnail" `isPrefixOf` f ]
    liftIO $ removeFile $ thumbDir ++ last oldFiles
    removeWorkspaceByTag $ show $ workspaceLen
    return ()
-- }}}
-- }}}
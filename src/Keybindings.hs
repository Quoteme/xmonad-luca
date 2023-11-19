-- vim: fdm=marker tabstop=2 shiftwidth=2 expandtab
--
-- This file is responsible for the keybindings in xmonad-luca
--
{-# LANGUAGE OverloadedStrings #-}

module Keybindings where

-- for some fullscreen events, also for xcomposite in obs.

import Control.Concurrent (threadDelay)
import Control.Monad (unless)
import Data.List (elemIndex)
import Data.List.NonEmpty (NonEmpty (..), nonEmpty)
import Data.Map qualified as M
import Data.Maybe (fromJust, isJust)
import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import Graphics.X11.ExtraTypes.XF86
import LaptopMode
import Options
import System.Directory (getHomeDirectory, listDirectory, removeFile)
import System.Environment (lookupEnv)
import System.Exit
import System.IO (hPutStrLn)
import System.Process (readProcess)
import Text.Format (format)
import Utilities (selectMaximizeWindow, toggleFloat)
import XMonad
import XMonad.Actions.CopyWindow (copyToAll, kill1, killAllOtherCopies)
import XMonad.Actions.CycleWindows (rotUnfocusedDown, rotUnfocusedUp)
import XMonad.Actions.DynamicWorkspaces (addWorkspace, appendWorkspacePrompt, removeEmptyWorkspace, removeWorkspace, removeWorkspaceByTag, renameWorkspace, selectWorkspace, withNthWorkspace)
import XMonad.Actions.EasyMotion (selectWindow)
import XMonad.Actions.GridSelect (gridselect)
import XMonad.Actions.Minimize (maximizeWindow, maximizeWindowAndFocus, minimizeWindow, withLastMinimized, withMinimized)
import XMonad.Actions.Navigation2D
import XMonad.Actions.OnScreen (viewOnScreen)
import XMonad.Actions.UpdateFocus (adjustEventInput, focusOnMouseMove)
import XMonad.Actions.UpdatePointer (updatePointer)
import XMonad.Actions.WindowMenu (windowMenu)
import XMonad.Hooks.DebugStack (debugStackFullString)
import XMonad.Hooks.DynamicLog (xmobarAction)
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers (doCenterFloat, doRectFloat, isDialog)
import XMonad.Hooks.Place (fixed, placeHook, smart, underMouse, withGaps)
import XMonad.Hooks.Rescreen
import XMonad.Hooks.ServerMode (serverModeEventHookF)
import XMonad.Hooks.SetWMName (setWMName)
import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.BorderResize (borderResize)
import XMonad.Layout.BoringWindows qualified as BW
import XMonad.Layout.Decoration
import XMonad.Layout.DecorationAddons (handleScreenCrossing)
import XMonad.Layout.DecorationMadness (mirrorTallSimpleDecoResizable, shrinkText)
import XMonad.Layout.DraggingVisualizer
import XMonad.Layout.Hidden
import XMonad.Layout.ImageButtonDecoration
import XMonad.Layout.LayoutHints
import XMonad.Layout.Magnifier (magnifier)
import XMonad.Layout.Maximize (maximize, maximizeRestore)
import XMonad.Layout.Minimize (minimize)
import XMonad.Layout.NoBorders (noBorders, smartBorders)
import XMonad.Layout.Reflect (reflectHoriz, reflectVert)
import XMonad.Layout.Renamed
import XMonad.Layout.Spiral (spiral)
import XMonad.Layout.SubLayouts (GroupMsg (MergeAll, UnMerge), onGroup, pullGroup, pullWindow, pushGroup, pushWindow)
import XMonad.Layout.WindowSwitcherDecoration
import XMonad.Prelude
import XMonad.Prompt (amberXPConfig)
import XMonad.StackSet qualified as S
import XMonad.Util.EZConfig
import XMonad.Util.ExtensibleState qualified as XS
import XMonad.Util.Hacks (windowedFullscreenFixEventHook)
import XMonad.Util.Image (Placement (..))
import XMonad.Util.NamedActions (addDescrKeys, addName, subtitle, xMessage, (^++^))
import XMonad.Util.Run (runProcessWithInput, spawnPipe)
import XMonad.Util.SpawnOnce

-- writeText will write a string to a file using data.Text
import Data.Text qualified as T
import Data.Text.IO qualified as Tio

-- My own keybindings
myKeys config =
  (subtitle "Custom Keys" :) $
    mkNamedKeymap config $
      -- Legend on how to use modifiers
      --    Code | Key
      --    M    | super key
      --    C    | control
      --    S    | shift
      --    M1   | alt
      --    M2   | num lock
      --    M3   |
      --    M4   | super
      -- 🚀 Launch Programs

      -- Legend on how to use modifiers
      --    Code | Key
      --    M    | super key
      --    C    | control
      --    S    | shift
      --    M1   | alt
      --    M2   | num lock
      --    M3   |
      --    M4   | super
      -- 🚀 Launch Programs
      [ ("M-<Return>", addName "Spawn Terminal" $ spawn $ terminal config)
      , ("M-d", addName "Open program launcher" $ spawn "rofi -show combi -show-icons")
      , ("M-S-w", addName "Search open window" $ spawn "rofi -show window")
      , ("M-e", addName "Open emoji selector" $ spawn "rofimoji")
      , ("M-S-s", addName "Screenshot" $ spawn "flameshot gui")
      , ("M-S-C-s", addName "Simple screenshot" $ spawn "maim -su | xclip -selection clipboard -t image/png")
      , ("M-S-q", addName "Kill window" $ kill)
      , ("M-<Space>", addName "Layout: next" $ sendMessage NextLayout)
      , ("M-S-<Space>", addName "Layout: default" $ setLayout $ layoutHook config)
      , -- 🔄 Rotational Focus Movement
        ("M-<Tab>", addName "WindowStack: rotate next" $ windows S.focusDown >> myUpdateFocus)
      , ("M-S-<Tab>", addName "WindowStack: rotate previous" $ windows S.focusUp >> myUpdateFocus)
      , ("M-C-<Tab>", addName "WindowStack: swap next" $ windows S.swapDown >> myUpdateFocus)
      , ("M-C-S-<Tab>", addName "WindowStack: swap previous" $ windows S.swapUp >> myUpdateFocus)
      , -- 🔎 Easymotion
        ("M-g", addName "Easymotion: focus" $ selectWindow def >>= (`whenJust` windows . S.focusWindow) >> myUpdateFocus)
      , ("M-C-g", addName "Easymotion: kill" $ selectWindow def >>= (`whenJust` killWindow))
      , -- 🏃 Directional Focus Movement
        ("M-h", addName "Focus: left" $ windowGo L False >> myUpdateFocus)
      , ("M-j", addName "Focus: down" $ windowGo D False >> myUpdateFocus)
      , ("M-k", addName "Focus: up" $ windowGo U False >> myUpdateFocus)
      , ("M-l", addName "Focus: right" $ windowGo R False >> myUpdateFocus)
      , ("M-<Left>", addName "Focus: left" $ windowGo L False >> myUpdateFocus)
      , ("M-<Down>", addName "Focus: down" $ windowGo D False >> myUpdateFocus)
      , ("M-<Up>", addName "Focus: up" $ windowGo U False >> myUpdateFocus)
      , ("M-<Right>", addName "Focus: right" $ windowGo R False >> myUpdateFocus)
      , -- , ("M-m"                     , addName "Focus: master" $ windows S.focusMaster >> myUpdateFocus)
        -- 🔀 Directional Window Movement
        ("M-S-h", addName "Swap: left" $ windowSwap L False >> myUpdateFocus)
      , ("M-S-j", addName "Swap: down" $ windowSwap D False >> myUpdateFocus)
      , ("M-S-k", addName "Swap: up" $ windowSwap U False >> myUpdateFocus)
      , ("M-S-l", addName "Swap: right" $ windowSwap R False >> myUpdateFocus)
      , ("M-S-<Left>", addName "Swap: left" $ windowSwap L False >> myUpdateFocus)
      , ("M-S-<Down>", addName "Swap: down" $ windowSwap D False >> myUpdateFocus)
      , ("M-S-<Up>", addName "Swap: up" $ windowSwap U False >> myUpdateFocus)
      , ("M-S-<Right>", addName "Swap: right" $ windowSwap R False >> myUpdateFocus)
      , ("M-S-m", addName "Swap: master" $ windows S.swapMaster >> myUpdateFocus)
      , -- Window resizing
        ("M-C-h", addName "Expand: left" $ sendMessage $ ExpandTowards L)
      , ("M-C-j", addName "Expand: down" $ sendMessage $ ExpandTowards D)
      , ("M-C-k", addName "Expand: up" $ sendMessage $ ExpandTowards U)
      , ("M-C-l", addName "Expand: right" $ sendMessage $ ExpandTowards R)
      , ("M-C-<Left>", addName "Expand: left" $ sendMessage $ ExpandTowards L)
      , ("M-C-<Down>", addName "Expand: down" $ sendMessage $ ExpandTowards D)
      , ("M-C-<Up>", addName "Expand: up" $ sendMessage $ ExpandTowards U)
      , ("M-C-<Right>", addName "Expand: right" $ sendMessage $ ExpandTowards R)
      , -- Splitting and moving
        ("M-S-C-k", addName "Split: next" $ sendMessage $ SplitShift Next)
      , ("M-S-C-j", addName "Split: previous" $ sendMessage $ SplitShift Prev)
      , -- Rotations/Swappings
        ("M-r", addName "BSP: rotate" $ myUpdateFocus <> sendMessage Rotate)
      , ("M-S-r", addName "BSP: rotate left around parent" $ myUpdateFocus <> sendMessage RotateL)
      , ("M-C-r", addName "BSP: rotate right around parent" $ myUpdateFocus <> sendMessage RotateR)
      , ("M-s", addName "BSP: swap" $ myUpdateFocus <> sendMessage Swap)
      , ("M-u", addName "BSP: focus parent" $ myUpdateFocus <> sendMessage FocusParent)
      , ("M-x", addName "BSP: cut/select node" $ sendMessage SelectNode)
      , ("M-p", addName "BSP: paste/move node" $ sendMessage MoveNode)
      , ("M-a", addName "BSP: balance" $ sendMessage Balance)
      , ("M-S-a", addName "BSP: equalize" $ sendMessage Equalize)
      , -- Sublayouts
        ("M-M1-h", addName "Sublayout: push group - left" $ sendMessage $ pushGroup L)
      , ("M-M1-j", addName "Sublayout: push group - down" $ sendMessage $ pushGroup D)
      , ("M-M1-k", addName "Sublayout: push group - up" $ sendMessage $ pushGroup U)
      , ("M-M1-l", addName "Sublayout: push group - right" $ sendMessage $ pushGroup R)
      , ("M-M1-S-h", addName "Sublayout: push window - left" $ sendMessage $ pushWindow L)
      , ("M-M1-S-j", addName "Sublayout: push window - down" $ sendMessage $ pushWindow D)
      , ("M-M1-S-k", addName "Sublayout: push window - up" $ sendMessage $ pushWindow U)
      , ("M-M1-S-l", addName "Sublayout: push window - right" $ sendMessage $ pushWindow R)
      , ("M-M1-<Left>", addName "Sublayout: pull group - left" $ sendMessage $ pullGroup L)
      , ("M-M1-<Down>", addName "Sublayout: pull group - down" $ sendMessage $ pullGroup D)
      , ("M-M1-<Up>", addName "Sublayout: pull group - up" $ sendMessage $ pullGroup U)
      , ("M-M1-<Right>", addName "Sublayout: pull group - right" $ sendMessage $ pullGroup R)
      , ("M-M1-S-<Left>", addName "Sublayout: pull window - left" $ sendMessage $ pullWindow L)
      , ("M-M1-S-<Down>", addName "Sublayout: pull window - down" $ sendMessage $ pullWindow D)
      , ("M-M1-S-<Up>", addName "Sublayout: pull window - up" $ sendMessage $ pullWindow U)
      , ("M-M1-S-<Right>", addName "Sublayout: pull window - right" $ sendMessage $ pullWindow R)
      , ("M-M1-t", addName "LaptopMode: toggle tablet mode" $ toggleTabletMode)
      , ("M-t", addName "Sublayout: merge all" $ withFocused (sendMessage . MergeAll))
      , ("M-S-t", addName "Sublayout: unmerge" $ withFocused (sendMessage . UnMerge))
      , ("M-w", addName "Sublayout: unmerge" $ withFocused (sendMessage . UnMerge))
      ,
        ( "M-M1-."
        , addName "Sublayout: focus up" $ do
            spawn "notify-send \"focus up\""
            onGroup S.focusUp'
        )
      ,
        ( "M-M1-,"
        , addName "Sublayout: focus down" $ do
            spawn "notify-send \"focus down\""
            onGroup S.focusDown'
        )
      , -- (Un-)Hiding
        ("M-<Backspace>", addName "Window: minimize window" $ withFocused minimizeWindow)
      , ("M-S-<Backspace>", addName "Window: unminimize last" $ withLastMinimized maximizeWindowAndFocus)
      , ("M-S-C-<Backspace>", addName "Window: unminimize menu" $ selectMaximizeWindow)
      , ("M-m", addName "Window: maximize" $ withFocused (sendMessage . maximizeRestore))
      ,
        ( "M-c"
        , addName "Window: copy to all other workspaces" $ do
            spawn $
              "notify-send "
                ++ "'window copied to all workspaces' "
                ++ "'undo by pressing M-S-c\n"
                ++ "do not show on current workspace by pressing M-C-c'"
            windows copyToAll
        )
      ,
        ( "M-S-c"
        , addName "Window: delete all other copies" $ do
            killAllOtherCopies
            spawn "notify-send 'deleted all other copies'"
        )
      ,
        ( "M-C-c"
        , addName "Window: kill current copy of window" $ do
            kill1
            spawn "notify-send 'deleted current copy'"
        )
      , -- Other stuff
        ("M-b", addName "Statusbar: toggle" $ sendMessage ToggleStruts)
      , ("M-f", addName "Window: toggle float" $ withFocused toggleFloat)
      , ("M-,", addName "Master: increase" $ sendMessage (IncMasterN 1))
      , ("M-.", addName "Master: decrease" $ sendMessage (IncMasterN (-1)))
      , ("M-o", addName "Window: menu" $ windowMenu)
      , -- Quitting
        ("M-S-<Delete>", addName "Xmonad: exit" $ io exitSuccess)
      , ("M-<Delete>", addName "Xmonad: restart" $ restart "xmonad" True *> spawn "notify-send \"Xmonad: restarted\"")
      ,
        ( "M-C-<Delete>"
        , addName "Xmonad: recompile" $ do
            dirs <- liftIO getDirectories
            liftIO $ void $ recompile dirs True
        )
      , -- Debugging
        ("M-C-d", addName "Workspace: debug" $ debugStackFullString >>= liftIO . Tio.writeFile "/tmp/xmonad_debug" . T.pack)
      , -- Function Keys
        ("<XF86MonBrightnessUp>", addName "Brightness: Monitor: raise" $ raiseMonBrigthness)
      , ("<XF86MonBrightnessDown>", addName "Brightness: Monitor: lower" $ lowerMonBrigthness)
      , ("<XF86KbdBrightnessUp>", addName "Brightness: Keyboard: raise" $ raiseKbdBrigthness)
      , ("<XF86KbdBrightnessDown>", addName "Brightness: Keyboard: lower" $ lowerKbdBrigthness)
      , ("<XF86AudioLowerVolume>", addName "Volume: raise" $ raiseAudio)
      , ("<XF86AudioRaiseVolume>", addName "Volume: lower" $ lowerAudio)
      , -- , ("<XF86AudioMicMute>"      , addName "Microphone: toggle" $ spawn "pamixer --toggle-mute")
        ("<XF86AudioMute>", addName "Volume: toggle" $ spawn "pamixer --toggle-mute")
      , ("<XF86AudioNext>", addName "Media: next" $ spawn "playerctl next")
      , ("<XF86AudioPrev>", addName "Media: previous" $ spawn "playerctl previous")
      , ("<XF86AudioPlay>", addName "Media: pause" $ spawn "playerctl play-pause")
      , ("<XF86Launch1>", addName "Workspace: debug" $ debugStackFullString >>= liftIO . Tio.writeFile "/tmp/xmonad_debug" . T.pack)
      , ("<XF86Launch3>", addName "Select color" $ spawn "xcolor | perl -pe 'chomp if eof' | xclip -selection clipboard")
      , ("<XF86Launch4>", addName "Power profile: cycle" $ spawn "powerprofilesctl-cycle")
      , -- Workspace keys
        ("M-S-p", addName "Workspace: preview" $ spawn "xmonad-workspace-preview")
      ]
        ^++^ ( [("M-" ++ show n, withNthWorkspace S.greedyView (n - 1)) | n <- [0 .. 9]]
                ++ [("M-S-" ++ show n, withNthWorkspace S.shift (n - 1)) | n <- [0 .. 9]]
                ++ [ (modifier ++ nth_key, windows $ function nth_workspace)
                   | (nth_key, nth_workspace) <- zip (map show [1 .. 9]) (workspaces config)
                   , (modifier, function) <-
                      [ ("M-C-", viewOnScreen 0)
                      , ("M-M1-", viewOnScreen 1)
                      -- , ("M-", S.greedyView)
                      -- , ("M-S-", S.shift)
                      ]
                   ]
             )
 where
  -- Helper functions
  lowerMonBrigthness :: MonadIO m => m ()
  lowerMonBrigthness = spawn "brightnessctl set 5%-"
  raiseMonBrigthness :: MonadIO m => m ()
  raiseMonBrigthness = spawn "brightnessctl set 5%+"
  lowerKbdBrigthness :: MonadIO m => m ()
  lowerKbdBrigthness = spawn "brightnessctl --device=\"asus::kbd_backlight\" set 1-"
  raiseKbdBrigthness :: MonadIO m => m ()
  raiseKbdBrigthness = spawn "brightnessctl --device=\"asus::kbd_backlight\" set 1+"
  lowerAudio :: MonadIO m => m ()
  lowerAudio = spawn "pamixer --increase 5"
  raiseAudio :: MonadIO m => m ()
  raiseAudio = spawn "pamixer --decrease 5"
  myUpdateFocus = updatePointer (0.5, 0.5) (0.1, 0.1)

-- My additional keybindings
-- Additional state needed
newtype KeyboardToggleState = KeyboardToggleState Bool deriving (Typeable, Read, Show)
instance ExtensionClass KeyboardToggleState where
  initialValue = KeyboardToggleState False
myAdditionalKeys config =
  additionalKeys
    config
    [ ((0, xF86XK_TouchpadToggle), toggleTouchpad)
    , ((0, xF86XK_TouchpadOn), enableTouchpad)
    , ((0, xF86XK_PowerOff), spawn "notify-send 'Poweroff' 'button was pressed'")
    , -- Thinkpad X201T keys
      ((0, xF86XK_RotateWindows), spawn "screenrotation.sh cycle_left")
    , ((0, xF86XK_TaskPane), spawn "screenrotation.sh swap")
    , -- , ((0                 , xF86XK_ScreenSaver    ), spawn "xdotool key super+s")
      -- , ((0                 , xF86XK_Launch1        ), spawn "xdotool key super+r")
      -- Workspaces
      -- selectWindow def >>= (`whenJust` windows . S.focusWindow) >> myUpdateFocus
      ((myModMask, xK_numbersign), selectWorkspace amberXPConfig)
    , ((myModMask .|. shiftMask, xK_numbersign), renameWorkspace amberXPConfig)
    , ((myModMask .|. shiftMask, xK_plus), appendWorkspacePrompt amberXPConfig)
    , ((myModMask, xK_plus), addLastWorkspace)
    , ((myModMask, xK_minus), removeLastWorkspace)
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
  enableTouchpad =
    spawn "xinput --enable \"ELAN1201:00 04F3:3098 Touchpad\""
      *> spawn "xinput --enable \"AT Translated Set 2 keyboard\""
      *> spawn "notify-send 'touchpad enabled'"
  disableTouchpad :: MonadIO m => m ()
  disableTouchpad =
    spawn "xinput --disable \"ELAN1201:00 04F3:3098 Touchpad\""
      *> spawn "xinput --disable \"AT Translated Set 2 keyboard\""
      *> spawn "notify-send 'touchpad disabled'"

-- Needed for adding workspaces with automatic names
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
  removeWorkspaceByTag $ show $ workspaceLen

-- -- remove old thumbnails
-- home <- liftIO getHomeDirectory
-- let thumbDir = home ++ "/.cache/"
-- files <- liftIO $ listDirectory thumbDir
-- let oldFiles = [ f | f <- sort files, "xmonad_workspace_thumbnail" `isPrefixOf` f ]
-- liftIO $ removeFile $ thumbDir ++ last oldFiles
-- return ()

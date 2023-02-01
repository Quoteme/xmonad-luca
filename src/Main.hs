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
import Utilities
import Keybindings
-- }}}

-- Options
-- {{{
myTerminal                  = "st"
myFocusFollowsMouse         = True
myClickJustFocuses          = True -- clicking to focus passes click to window?
myBorderWidth               = 3
-- set the alt key as the mod key
myModMask                   = mod4Mask
myWorkspaces                = map show [1..3]
myNormalBorderColor         = "#0c0c0c"
myFocusedBorderColor        = "#888888"
myTheme :: Theme
myTheme = (defaultThemeWithImageButtons {
  activeColor         = "#161616",
  inactiveColor       = "#0c0c0c",
  urgentColor         = "#0c0c0c",
  activeBorderColor   = "#161616",
  inactiveBorderColor = "#0c0c0c",
  urgentBorderColor   = "#0c0c0c",
  activeBorderWidth   = 0,
  inactiveBorderWidth = 0,
  urgentBorderWidth   = 3,
  activeTextColor     = "#fae73b",
  inactiveTextColor   = "#d9d9d9",
  urgentTextColor     = "#fa693b",
  decoHeight          = 30,
  fontName            = "xft:scientifica:pixelsize=11:antialias=false"
})
-- }}}

-- My additional keybindings
-- {{{
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

-- Navigation2DConfig
-- {{{
myNavigation2DConfig = def { layoutNavigation = [
    ("myBSP", hybridOf sideNavigation lineNavigation ),
    ("tabletmodeBSP", hybridOf sideNavigation lineNavigation ),
    ("myTabletMode", hybridOf sideNavigation lineNavigation )
  ] }
-- }}}

-- Mouse bindings
-- {{{
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList
    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), \w -> focus w >> mouseMoveWindow w
                                       >> windows S.shiftMaster)
    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), \w -> focus w >> windows S.shiftMaster)
    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), \w -> focus w >> mouseResizeWindow w
                                       >> windows S.shiftMaster)
    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]
-- }}}

-- My Layouts
-- {{{
myLayout = avoidStruts
         $   myBSP
         ||| myTabletMode
         ||| myFullscreen
  where
    -- TODO: add tabs to this layout
    myBSP = renamed [Replace "myBSP"]
          $ hiddenWindows
          $ layoutHints
          $ smartBorders
          $ borderResize
          emptyBSP
    tabletmodeBSP = renamed [Replace "tabletmodeBSP"]
                  $ noBorders
                  $ windowSwitcherDecorationWithImageButtons shrinkText myTheme (draggingVisualizer myBSP)
    myTabletMode = renamed [Replace "myTabletMode"]
                $ minimize
                $ BW.boringWindows
                $ maximize
                $ extendedWindowSwitcherDecoration shrinkText (draggingVisualizer myBSP)
    myFullscreen = renamed [Replace "myFullscreen"]
                $ noBorders
                $ maximize
                $ Full


-- My own extended version of windowSwitcherDecoration
-- for example, draggina a window to the right edge of the screen should
-- move it to the next workspace
-- {{{

extendedWindowSwitcherDecoration :: (Eq a, Shrinker s) => s -> l a -> ModifiedLayout (Decoration ExtendedWindowSwitcherDecoration s) l a
extendedWindowSwitcherDecoration s = decoration s myOwnTheme EWSD

-- Custom theme
-- {{{

-- Icons / Menu buttons
-- {{{

-- support functions / values (for convenience)
-- {{{
convertToBool' :: [Int] -> [Bool]
convertToBool' = map (==1)

convertToBool :: [[Int]] -> [[Bool]]
convertToBool = map convertToBool'

buttonSize :: Int
buttonSize = length menuButton

buttonPadding :: Int
buttonPadding = 15

buttonMargin :: Int
buttonMargin = 5
-- }}}

-- {{{
menuButton :: [[Bool]]
menuButton = convertToBool
  [[1,1,1,1,0,0,0,0,1,1,1,1,0,0,0,0,1,1,1,1],
   [1,1,1,1,0,0,0,0,1,1,1,1,0,0,0,0,1,1,1,1],
   [1,1,1,1,0,0,0,0,1,1,1,1,0,0,0,0,1,1,1,1],
   [1,1,1,1,0,0,0,0,1,1,1,1,0,0,0,0,1,1,1,1],
   [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
   [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
   [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
   [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
   [1,1,1,1,0,0,0,0,1,1,1,1,0,0,0,0,1,1,1,1],
   [1,1,1,1,0,0,0,0,1,1,1,1,0,0,0,0,1,1,1,1],
   [1,1,1,1,0,0,0,0,1,1,1,1,0,0,0,0,1,1,1,1],
   [1,1,1,1,0,0,0,0,1,1,1,1,0,0,0,0,1,1,1,1],
   [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
   [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
   [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
   [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
   [1,1,1,1,0,0,0,0,1,1,1,1,0,0,0,0,1,1,1,1],
   [1,1,1,1,0,0,0,0,1,1,1,1,0,0,0,0,1,1,1,1],
   [1,1,1,1,0,0,0,0,1,1,1,1,0,0,0,0,1,1,1,1],
   [1,1,1,1,0,0,0,0,1,1,1,1,0,0,0,0,1,1,1,1]]
-- }}}

-- {{{
miniButton :: [[Bool]]
miniButton = convertToBool
  [[1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],
   [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],
   [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],
   [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],
   [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
   [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
   [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
   [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
   [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
   [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
   [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
   [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
   [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
   [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
   [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
   [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
   [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
   [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
   [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
   [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
   [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]]
-- }}}

-- {{{
maxiButton :: [[Bool]]
maxiButton = convertToBool
  [[1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],
   [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],
   [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],
   [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],
   [1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1],
   [1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1],
   [1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1],
   [1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1],
   [1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1],
   [1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1],
   [1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1],
   [1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1],
   [1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1],
   [1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1],
   [1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1],
   [1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1],
   [1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1],
   [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],
   [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],
   [0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],
   [0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]]
-- }}}

-- {{{
closeButton :: [[Bool]]
closeButton = convertToBool
  [[1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1],
   [1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1],
   [1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1],
   [0,1,1,1,1,1,0,0,0,0,0,0,0,0,0,1,1,1,1,1,0],
   [0,0,1,1,1,1,1,0,0,0,0,0,0,0,1,1,1,1,1,0,0],
   [0,0,0,1,1,1,1,1,0,0,0,0,0,1,1,1,1,1,0,0,0],
   [0,0,0,0,1,1,1,1,1,0,0,0,1,1,1,1,1,0,0,0,0],
   [0,0,0,0,0,1,1,1,1,1,0,1,1,1,1,1,0,0,0,0,0],
   [0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0],
   [0,0,0,0,0,0,0,1,1,1,1,1,1,1,0,0,0,0,0,0,0],
   [0,0,0,0,0,0,0,0,1,1,1,1,1,0,0,0,0,0,0,0,0],
   [0,0,0,0,0,0,0,1,1,1,1,1,1,1,0,0,0,0,0,0,0],
   [0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0],
   [0,0,0,0,0,1,1,1,1,1,0,1,1,1,1,1,0,0,0,0,0],
   [0,0,0,0,1,1,1,1,1,0,0,0,1,1,1,1,1,0,0,0,0],
   [0,0,0,1,1,1,1,1,0,0,0,0,0,1,1,1,1,1,0,0,0],
   [0,0,1,1,1,1,1,0,0,0,0,0,0,0,1,1,1,1,1,0,0],
   [0,1,1,1,1,1,0,0,0,0,0,0,0,0,0,1,1,1,1,1,0],
   [1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1],
   [1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1],
   [1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1]]
-- }}}

-- {{{
rotateButton :: [[Bool]]
rotateButton = convertToBool
  [[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
   [0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,0,1,1,1,0],
   [0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,1,1,0,1,0],
   [0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,1,0],
   [0,0,1,0,0,0,1,1,1,1,1,1,1,1,0,0,0,0,1,0],
   [0,1,0,0,0,1,0,0,0,0,0,0,1,1,1,0,0,0,1,0],
   [0,1,0,0,1,0,0,0,0,0,0,0,1,0,0,0,0,0,1,0],
   [0,1,0,0,1,0,0,0,0,0,0,0,1,1,1,1,1,1,1,0],
   [0,1,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
   [0,1,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
   [0,1,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
   [0,1,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
   [0,1,0,0,1,0,0,0,0,0,0,0,0,0,0,1,1,1,1,0],
   [0,1,0,0,1,0,0,0,0,0,0,0,0,0,1,0,0,0,1,0],
   [0,1,0,0,0,1,0,0,0,0,0,0,0,1,0,0,0,0,1,0],
   [0,0,1,0,0,0,1,1,1,1,1,1,1,0,0,0,0,1,0,0],
   [0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0],
   [0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0],
   [0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0],
   [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]]
-- }}}

-- {{{
swapButton :: [[Bool]]
swapButton = convertToBool
  [[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
   [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
   [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
   [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
   [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
   [0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0],
   [0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0],
   [0,0,1,1,1,0,0,0,0,0,0,0,0,0,0,1,1,1,0,0],
   [0,1,1,1,1,0,0,0,0,0,0,0,0,0,0,1,1,1,1,0],
   [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],
   [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],
   [0,1,1,1,1,0,0,0,0,0,0,0,0,0,0,1,1,1,1,0],
   [0,0,1,1,1,0,0,0,0,0,0,0,0,0,0,1,1,1,0,0],
   [0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0],
   [0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0],
   [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
   [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
   [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
   [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
   [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]]
-- }}}
-- }}}

-- Theme
-- {{{
myOwnTheme :: Theme
myOwnTheme = def {
  activeColor         = "#161616",
  inactiveColor       = "#0c0c0c",
  urgentColor         = "#0c0c0c",
  activeBorderColor   = "#161616",
  inactiveBorderColor = "#0c0c0c",
  urgentBorderColor   = "#0c0c0c",
  activeBorderWidth   = 0,
  inactiveBorderWidth = 0,
  urgentBorderWidth   = 3,
  activeTextColor     = "#888888",
  inactiveTextColor   = "#888888",
  urgentTextColor     = "#fa693b",
  decoHeight          = 30,
  fontName            = "xft:scientifica:pixelsize=11:antialias=false",
  windowTitleIcons    = [ (menuButton, CenterLeft buttonMargin)
                        , (rotateButton, CenterLeft (buttonSize + buttonPadding + buttonMargin))
                        , (swapButton, CenterLeft ((buttonSize + buttonPadding)*2 + buttonMargin))
                        -- , (miniButton, CenterRight ((buttonSize + buttonPadding)*2+buttonMargin))
                        -- , (maxiButton, CenterRight (buttonSize + buttonPadding + buttonMargin))
                        , (closeButton, CenterRight buttonMargin) ]
}
-- }}}

-- }}}

-- Custom layout
-- {{{
data ExtendedWindowSwitcherDecoration a = EWSD deriving (Show, Read)
instance Eq a => DecorationStyle ExtendedWindowSwitcherDecoration a where
  describeDeco _ = "ExtendedWindowSwitcherDecoration"
  -- {{{ 
  decorationCatchClicksHook EWSD mainw dFl dFr = do
    handleButtons dFl dFr
    where
      -- is the distance from right of the click correlated to the nth button from the right/right?
      -- left/right depend on the parameter dFs (distance from side)
      isNthButton :: Int -> Int -> Bool
      isNthButton dFs n = buttonMargin + n*(buttonSize+buttonPadding) < dFs
                        && dFs < buttonMargin + (n+1)*(buttonSize+buttonPadding)
      -- like isNthButton but to check if a right button was clicked
      isNthRightButton :: Int -> Bool
      isNthRightButton = isNthButton dFr
      -- like isNthButton but to check if a left button was clicked
      isNthLightButton :: Int -> Bool
      isNthLightButton = isNthButton dFl
      -- Call this function to handle button clicks and what happens on a button click
      -- if a button was clicked, return True, else False
      handleButtons :: Int -> Int -> X Bool
      handleButtons dFl dFr
        -- right side
        -- Close button
        | isNthRightButton 0 = do
          kill
          return True
        -- Maximize button
        -- | isNthRightButton 1 = do
        --   -- send a key to toggle fullscreen (not maximize) on the window
        --   -- this makes tabs and searchbars in webbrowsers disappear
        --   spawn "notify-send 'xmonad' 'maximize button clicked'"
        --   return True
        -- Minimize button
        -- | isNthRightButton 2 = do
        --   withFocused minimizeWindow
        --   return True
        -- left side
        -- Menu button
        | isNthLightButton 0 = do
          windowMenu
          return True
        -- Rotate button
        | isNthLightButton 1 = do
          sendMessage Rotate
          return True
        | isNthLightButton 2 = do
          sendMessage Swap
          return True
        -- no button was clicked
        | otherwise = return False
  --  }}}
  -- {{{
  decorationWhileDraggingHook _ ex ey (mainw, r) x y = do
    let rect = Rectangle (x - (fi ex - rect_x r))
                         (y - (fi ey - rect_y r))
                         (rect_width  r)
                         (rect_height r)
    -- when (x<10) $
    --   spawn $ format "notify-send 'xmonad internal' 'dragging at x: {0} y: {1}'" [show x, show y]
    sendMessage $ DraggingWindow mainw rect
  --  }}}
  -- {{{
  decorationAfterDraggingHook _ (mainw, r) decoWin = do
    focus mainw
    hasCrossed <- handleScreenCrossing mainw decoWin
    unless hasCrossed $ do
      sendMessage DraggingStopped
      performWindowSwitching mainw
    where
      performWindowSwitching :: Window -> X ()
      performWindowSwitching win =
          withDisplay $ \d -> do
             root <- asks theRoot
             (_, _, selWin, rx, ry, wx, wy, _) <- io $ queryPointer d root
             ws <- gets windowset
             let allWindows = S.index ws
             -- do a little double check to be sure
             when ((win `elem` allWindows) && (selWin `elem` allWindows)) $ do
                      let allWindowsSwitched = map (switchEntries win selWin) allWindows
                      -- let (ls, v) = break (win ==) allWindowsSwitched
                      let (ls,  t : rs) = break (win ==) allWindowsSwitched
                      let newStack = S.Stack t (reverse ls) rs
                      windows $ S.modify' $ const newStack
          where
              switchEntries a b x
                  | x == a    = b
                  | x == b    = a
                  | otherwise = x
  --  }}}
  -- {{{
  -- Only show decoration for currently focused window
  pureDecoration _ _ ht _ s _ (w, Rectangle x y wh ht') = if isInStack s w && w == S.focus s && ( length (S.up s) /= 0 || length (S.down s) /= 0 )
    then Just $ Rectangle x y wh ht
    else Nothing
  -- }}}
-- }}}
-- }}}
-- }}}

-- Manage hooks
-- {{{
myManageHook = composeAll [ appName =? "control_center" --> doRectFloat (S.RationalRect 0.65 0.05 0.325 0.45)
                          , className =? "Onboard" --> doFloat
                          , isDialog --> doCenterFloat
                          ]
-- }}}

-- Event hook
-- {{{
myEventHook = focusOnMouseMove
            <+> hintsEventHook
            <+> windowedFullscreenFixEventHook
            <+> dunstOnTop
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
                defaultServerCommands "layout-tablet"      = sendMessage $ JumpToLayout "myTabletMode"
                defaultServerCommands "layout-normal"      = sendMessage $ JumpToLayout "myBSP"
                defaultServerCommands "layout-get"         = sendMessage ToggleStruts
                defaultServerCommands "toggle-struts"      = sendMessage ToggleStruts
                defaultServerCommands "select-to-maximize" = selectMaximizeWindow
                defaultServerCommands "workspace-add"      = addLastWorkspace
                defaultServerCommands "workspace-remove"   = removeLastWorkspace
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
                dunstOnTop :: Event -> X All
                dunstOnTop (AnyEvent {ev_event_type = et}) = do
                  when (et == focusOut) $ do
                    spawn "xdotool windowraise `xdotool search --all --name Dunst`"
                  return $ All True
                dunstOnTop (FocusChangeEvent {}) = do
                  spawn "xdotool windowraise `xdotool search --all --name Dunst`"
                  return $ All True
                dunstOnTop ev = return $ All True
-- }}}

--The client events that xmonad is interested in
-- {{{ 
myClientMask = focusChangeMask .|. clientMask def
-- }}}

-- Screen / rander change hooks
-- {{{
myAfterRescreenHook :: X ()
myAfterRescreenHook = do
  spawn "xinput --map-to-output 'ELAN9008:00 04F3:2C82' eDP"
  spawn "xinput --map-to-output 'ELAN9008:00 04F3:2C82 Stylus Pen (0)' eDP"
  spawn "xinput --map-to-output 'ELAN9008:00 04F3:2C82 Stylus Eraser (0)' eDP"

myRandrChangeHook :: X ()
myRandrChangeHook = do
  spawn "notify-send 'Rescreen' 'screen changed'"
  spawn "xinput --map-to-output 'ELAN9008:00 04F3:2C82' eDP"
  spawn "xinput --map-to-output 'ELAN9008:00 04F3:2C82 Stylus Pen (0)' eDP"
  spawn "xinput --map-to-output 'ELAN9008:00 04F3:2C82 Stylus Eraser (0)' eDP"
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

-- My Log hook
-- {{{ 
-- {{{ Thumbnail config
removeOldThumbnails :: IO ()
removeOldThumbnails = do
  home <- getHomeDirectory
  let thumbDir = home ++ "/.cache/"
  files <- listDirectory thumbDir
  let oldFiles = [ f | f <- files, "xmonad_workspace_thumbnail" `isPrefixOf` f ]
  mapM_ (\f -> removeFile (thumbDir ++ f)) oldFiles

newtype ThumbnailLastSave = ThumbnailLastSave POSIXTime
  deriving (Show, Eq)
instance ExtensionClass ThumbnailLastSave where
  initialValue = ThumbnailLastSave 0

newtype LastSavedWorkspaceThumbnail = LastSavedWorkspaceThumbnail String
  deriving (Show, Eq)
instance ExtensionClass LastSavedWorkspaceThumbnail where
  initialValue = LastSavedWorkspaceThumbnail "no workspace"

shouldSaveNewThumbnail :: POSIXTime ->  X Bool
shouldSaveNewThumbnail delay = do
  -- Delay in seconds
  ThumbnailLastSave lastSaveTime <- XS.get
  LastSavedWorkspaceThumbnail lastSavedWorkspace <- XS.get
  currentWorkspace <- gets (S.tag . S.workspace . S.current . windowset)
  currentTime <- liftIO getPOSIXTime
  let shouldSave = currentTime - lastSaveTime > delay || currentWorkspace /= lastSavedWorkspace
  when shouldSave $ do
    XS.put $ ThumbnailLastSave currentTime
    XS.put $ LastSavedWorkspaceThumbnail currentWorkspace
  return shouldSave
-- }}}

myLogHook :: X ()
myLogHook = do
  thumbnailHook
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

-- Main
-- {{{
main = getDirectories >>= launch
        ( docks
        $ ewmh
        $ myAdditionalKeys
        $ addDescrKeys ((myModMask, xK_F1), xMessage) myKeys
        $ withNavigation2DConfig myNavigation2DConfig
        $ rescreenHook def{
          randrChangeHook = myRandrChangeHook,
          afterRescreenHook = myAfterRescreenHook
        }
        $ def
          {
            -- simple stuff
              terminal           = myTerminal,
              focusFollowsMouse  = myFocusFollowsMouse,
              clickJustFocuses   = myClickJustFocuses,
              borderWidth        = myBorderWidth,
              modMask            = myModMask,
              workspaces         = myWorkspaces,
              normalBorderColor  = myNormalBorderColor,
              focusedBorderColor = myFocusedBorderColor,
            -- mouse bindings
              mouseBindings      = myMouseBindings,
            -- hooks, layouts
              layoutHook         = myLayout,
              manageHook         = myManageHook,
              handleEventHook    = myEventHook,
              startupHook        = myStartupHook,
              clientMask         = myClientMask,
              logHook            = myLogHook
          })
-- }}}

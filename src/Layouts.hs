-- vim: fdm=marker tabstop=2 shiftwidth=2 expandtab
--
-- Define all the window layouts for the program here.
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Layouts where
import XMonad.Actions.Navigation2D
import qualified XMonad.Layout.BoringWindows as BW
import qualified XMonad.StackSet as S
import qualified Data.Map        as M
import qualified XMonad.Util.ExtensibleState as XS
import Graphics.X11 (Window, Rectangle (Rectangle, rect_x, rect_y, rect_width, rect_height), queryPointer)
import XMonad (X, kill, sendMessage, (|||), Full (Full), focus, withDisplay, asks, XConf (theRoot), io, gets, XState (windowset))
import XMonad.Layout.Decoration
import XMonad.Util.Image
import XMonad.Actions.WindowMenu
import XMonad.Layout.BinarySpacePartition
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Hidden
import XMonad.Layout.LayoutHints
import XMonad.Layout.NoBorders
import XMonad.Layout.BorderResize
import XMonad.Layout.Renamed
import XMonad.Layout.Minimize
import XMonad.Layout.Maximize
import XMonad.Layout.DraggingVisualizer
import XMonad.Layout.WindowSwitcherDecoration
import Options
import Control.Monad (unless, when)
import XMonad.Layout.DecorationAddons (handleScreenCrossing)
import XMonad.Operations (windows)
import Theme
import Icons
import LaptopMode
import XMonad.Layout.Tabbed (simpleTabbed)
import XMonad.Layout.SubLayouts (subLayout, subTabbed)

-- {{{ Navigation2DConfig
myNavigation2DConfig = def { layoutNavigation = [
    ("myBSP", hybridOf sideNavigation lineNavigation ),
    ("myTabletMode", hybridOf sideNavigation lineNavigation )
  ] }
-- }}}

-- {{{ My Layouts
myLayout = avoidStruts
         myTabletMode
         ||| myFullscreen
  where
    -- TODO: add tabs to this layout
    myBSP = renamed [Replace "myBSP"]
          $ hiddenWindows
          $ layoutHints
          $ smartBorders
          $ borderResizeNear 12
          $ subLayout [] simpleTabbed
          emptyBSP
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
  pureDecoration _ _ ht _ s _ (w, Rectangle x y wh ht') =
    if isInStack s w
      && w == S.focus s
      && ( length (S.up s) /= 0
        || length (S.down s) /= 0 )
    then Just $ Rectangle x y wh ht
    else Nothing

  decorate ds w h r s wrs wr = do
    laptopmode <- getLaptopMode
    if laptopmode == TabletMode then
      return $ pureDecoration ds w h r s wrs wr
    else
      return Nothing
  -- }}}
-- }}}
-- }}}
-- }}}

-- vim: fdm=marker tabstop=2 shiftwidth=2 expandtab
--
-- Small utility functions for small actions

module Utilities where

import Control.Monad
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe
import Graphics.X11.Types (Window)
import XMonad (X, appName, title, windows)
import XMonad.Actions.GridSelect
import XMonad.Actions.Minimize (maximizeWindow, withMinimized)
import XMonad.Core (runQuery)
import XMonad.StackSet qualified as S

-- Allow user to select window to reopen
selectMaximizeWindow :: X ()
selectMaximizeWindow = do
  -- withMinimized (mapM_ maximizeWindow)
  withMinimized
    ( \minimizedWindows -> do
        -- Get the window title of the minimized windows
        minimizedWindowTitles <- mapM getWinTitle minimizedWindows
        selectedWin <- gridselect def (zip minimizedWindowTitles minimizedWindows)
        when (isJust selectedWin) $
          maximizeWindow (fromJust selectedWin)
    )
  return ()
  where
    getWinTitle :: Window -> X String
    getWinTitle w = do
      winTitle <- runQuery title w
      winAppName <- runQuery appName w
      return $ winTitle ++ " : " ++ winAppName

toggleFloat w =
  windows
    ( \s ->
        if M.member w (S.floating s)
          then S.sink w s
          else S.float w (S.RationalRect (1 / 3) (1 / 4) (1 / 2) (1 / 2)) s
    )

-- vim: fdm=marker tabstop=2 shiftwidth=2 expandtab
--
-- Small utility functions for small actions

module Utilities where
import XMonad (X, title, appName)
import Graphics.X11.Types (Window)
import XMonad.Actions.Minimize (withMinimized, maximizeWindow)
import XMonad.Actions.GridSelect
import Control.Monad
import Data.Maybe
import XMonad.Core (runQuery)

-- Allow user to select window to reopen
-- {{{
selectMaximizeWindow :: X ()
selectMaximizeWindow = do
  -- withMinimized (mapM_ maximizeWindow)
  withMinimized (\minimizedWindows -> do
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

-- }}}

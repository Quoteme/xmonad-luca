module State where

import Control.Concurrent (MVar, modifyMVar_, newMVar)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (sort, sortOn)
import GHC.IO (unsafePerformIO)
import XMonad (ExtensionClass, MonadIO (liftIO), Window, WorkspaceId, X, XState (windowset), gets, initialValue, runQuery, title)
import XMonad.Actions.Minimize (withMinimized)
import XMonad.StackSet qualified as S
import XMonad.Util.ExtensibleState qualified as XS

type MinimizedWindows = [(Window, String)]

{- | WorkspaceInfo is a tuple of the:
workspace name,
whether it is visible and
whether it is focused
-}
type WorkspaceInfo = (WorkspaceId, Bool, Bool)

data AppState = AppState
  { layout :: String
  , layouts :: [String]
  , workspaces :: [WorkspaceInfo]
  , minimizedWindows :: MinimizedWindows
  }

instance ExtensionClass (MVar AppState) where
  initialValue = unsafePerformIO $ newMVar def

def :: AppState
def =
  AppState
    { layout = "myBSP"
    , layouts = ["myBSP", "fullscreen"]
    , workspaces = []
    , minimizedWindows = []
    }

nextLayout :: AppState -> AppState
nextLayout state = state{layout = nextLayout'}
 where
  currentLayout = layout state
  nextLayout' = ((!! 1) . dropWhile (/= currentLayout) . cycle) $ layouts state

_getWorkspaces :: X [WorkspaceInfo]
_getWorkspaces = do
  ws <- gets windowset
  (S.current ws & S.workspace & S.tag, True, True)
    : [(w & S.workspace & S.tag, True, False) | w <- S.visible ws]
    ++ [(w & S.tag, False, False) | w <- S.hidden ws]
    & sortOn (\(w, _, _) -> w)
    & pure

updateWorkspaces :: X ()
updateWorkspaces = do
  appstate <- XS.get :: X (MVar State.AppState)
  wi <- _getWorkspaces
  liftIO $ modifyMVar_ appstate $ \s -> pure s{State.workspaces = wi}

updateMinimizedWindows :: X ()
updateMinimizedWindows = do
  appstate <- XS.get :: X (MVar State.AppState)
  minimized' <- withMinimized pure
  minimized <-
    mapM
      ( \w -> do
          windowName <- runQuery title w
          return (w, windowName)
      )
      minimized' ::
      X MinimizedWindows
  liftIO $ modifyMVar_ appstate $ \s -> return s{State.minimizedWindows = minimized}

initialize :: X (MVar AppState)
initialize = do
  -- get a list of strings (workspace names)
  wi <- _getWorkspaces
  liftIO $
    newMVar
      def
        { workspaces = wi
        }

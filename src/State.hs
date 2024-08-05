module State where

import Control.Concurrent.STM (TVar, atomically, modifyTVar, newTVar, newTVarIO)
import Data.Functor ((<&>))
import GHC.IO (unsafePerformIO)
import XMonad (ExtensionClass, MonadIO (liftIO), WorkspaceId, X, XState (windowset), gets, initialValue)
import XMonad.StackSet qualified as S
import XMonad.Util.ExtensibleState qualified as XS

data AppState = AppState
  { layout :: String
  , layouts :: [String]
  , workspace :: String
  , workspaces :: [WorkspaceId]
  }

instance ExtensionClass (TVar AppState) where
  initialValue = unsafePerformIO $ newTVarIO def

def :: AppState
def =
  AppState
    { layout = "myBSP"
    , layouts = ["myBSP", "fullscreen"]
    , workspace = "0"
    , workspaces = []
    }

nextLayout :: AppState -> AppState
nextLayout state = state{layout = nextLayout'}
 where
  currentLayout = layout state
  nextLayout' = ((!! 1) . dropWhile (/= currentLayout) . cycle) $ layouts state

updateWorkspaces :: X ()
updateWorkspaces = do
  appstate <- XS.get :: X (TVar State.AppState)
  ws <- gets windowset <&> map S.tag . S.workspaces
  liftIO $ atomically $ modifyTVar appstate $ \s -> s{State.workspaces = ws}

initialize :: X (TVar AppState)
initialize = do
  -- get a list of strings (workspace names)
  ws <- gets windowset <&> map S.tag . S.workspaces
  liftIO $
    newTVarIO
      def
        { workspaces = ws
        }

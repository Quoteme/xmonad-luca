module State where

import Control.Concurrent.STM (TVar, atomically, newTVar, newTVarIO)
import GHC.IO (unsafePerformIO)
import XMonad (ExtensionClass, initialValue)

data AppState = AppState
  { layout :: String
  , layouts :: [String]
  , workspace :: String
  , workspaces :: [String]
  }

instance ExtensionClass (TVar AppState) where
  initialValue = unsafePerformIO initialize

def :: AppState
def =
  AppState
    { layout = "myBSP"
    , layouts = ["myBSP", "fullscreen"]
    , workspace = "0"
    , workspaces = ["0", "1", "2"]
    }

addWorkspace :: AppState -> String -> AppState
addWorkspace state newWorkspace = state{workspaces = newWorkspace : workspaces state}

removeWorkspace :: AppState -> String -> AppState
removeWorkspace state workspaceToRemove = state{workspaces = filter (/= workspaceToRemove) $ workspaces state}

initialize :: IO (TVar AppState)
initialize =
  newTVarIO def

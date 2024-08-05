module DBusServer where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class
import DBus
import DBus.Client
import DBus.Introspection (Signal (Signal, signalArgs, signalName))
import State qualified

start :: TVar State.AppState -> IO ()
start appState = do
  putStrLn "Starting DBus server"
  client <- connectSession
  requestResult <-
    requestName
      client
      (busName_ "org.xmonad.bus")
      [ nameAllowReplacement
      , nameReplaceExisting
      , nameDoNotQueue
      ]
  when (requestResult /= NamePrimaryOwner) $ do
    error "Service already in use"
  export
    client
    (objectPath_ "/general")
    defaultInterface
      { interfaceName = interfaceName_ "org.xmonad.bus"
      , interfaceMethods =
          [ autoMethod (memberName_ "Layout") $ do
              state <- readTVarIO appState
              return (State.layout state)
          , autoMethod (memberName_ "Layouts") $ do
              state <- readTVarIO appState
              return (State.layouts state)
          , autoMethod (memberName_ "Workspace") $ do
              state <- readTVarIO appState
              return (State.workspace state)
          , autoMethod (memberName_ "Workspaces") $ do
              state <- readTVarIO appState
              return (State.workspaces state)
          ]
      , interfaceSignals =
          [ Signal
              { signalName = memberName_ "LayoutChanged"
              , signalArgs = []
              }
          , Signal
              { signalName = memberName_ "WorkspacesChanged"
              , signalArgs = []
              }
          , Signal
              { signalName = memberName_ "WorkspaceChanged"
              , signalArgs = []
              }
          ]
      }
  -- wait forever for calls
  forever (threadDelay 1000000)

signalLayoutChanged :: Client -> IO ()
signalLayoutChanged client = do
  emit client $
    ( signal
        (objectPath_ "/general")
        (interfaceName_ "org.xmonad.bus")
        (memberName_ "LayoutChanged")
    )
      { signalBody = [toVariant "hallo"]
      }

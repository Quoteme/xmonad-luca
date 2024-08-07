module DBusServer where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class
import DBus
import DBus.Client
import DBus.Introspection (Signal (Signal, signalArgs, signalName))
import Data.Function ((&))
import State qualified
import XMonad (Window, X, XState (windowset), appName, gets, liftIO, runQuery, title, whenJust)
import XMonad.Actions.Minimize (withMinimized)
import XMonad.StackSet (peek)
import XMonad.Util.ExtensibleState qualified as XS

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
          , autoMethod (memberName_ "Workspaces") $ do
              state <- readTVarIO appState
              return (State.workspaces state)
          , autoMethod (memberName_ "MinimizedWindows") $ do
              state <- readTVarIO appState
              return (State.minimizedWindows state)
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
              { signalName = memberName_ "WindowChanged"
              , signalArgs = []
              }
          , Signal
              { signalName = memberName_ "MinimizedWindowsChanged"
              , signalArgs = []
              }
          ]
      }
  -- wait forever for calls
  forever (threadDelay 1000000)

{- | Emit a signal over DBUS that some value of the app state has changed.
for an example, see [signalLayoutChanged].
-}
_signalAppStateChanged :: (IsVariant a) => String -> (State.AppState -> a) -> X ()
_signalAppStateChanged memberName' stateAccessor = do
  appState <- XS.get :: X (TVar State.AppState)
  liftIO $ do
    state <- readTVarIO appState
    client <- connectSession
    emit client $
      ( signal
          (objectPath_ "/general")
          (interfaceName_ "org.xmonad.bus")
          (memberName_ memberName')
      )
        { signalBody = [toVariant (stateAccessor state)]
        }

signalLayoutChanged :: X ()
signalLayoutChanged = _signalAppStateChanged "LayoutChanged" State.layout

signalWorkspacesChanged :: X ()
signalWorkspacesChanged = _signalAppStateChanged "WorkspacesChanged" State.workspaces

signalMinimizedWindowsChanged :: X ()
signalMinimizedWindowsChanged = _signalAppStateChanged "MinimizedWindowsChanged" (_serializeMinimizedWindows . State.minimizedWindows)
 where
  _serializeMinimizedWindows :: State.MinimizedWindows -> Variant
  _serializeMinimizedWindows x = toVariant [toVariant (toVariant w, toVariant n) | (w, n) <- x]

-- | Emit a signal over DBUS that the focused window has changed.
signalWindowChanged :: X ()
signalWindowChanged = do
  fw <- gets $ peek . windowset
  whenJust fw $ \w -> do
    windowName <- runQuery title w
    liftIO $ do
      client <- connectSession
      emit client $
        ( signal
            (objectPath_ "/general")
            (interfaceName_ "org.xmonad.bus")
            (memberName_ "WindowChanged")
        )
          { signalBody = [toVariant windowName]
          }

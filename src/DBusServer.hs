module DBusServer where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class
import DBus
import DBus.Client
import State qualified

sayHello :: String -> IO String
sayHello name = return ("Hello " ++ name ++ "!")

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
      { interfaceName = (interfaceName_ "org.xmonad.bus")
      , interfaceMethods =
          [ autoMethod (memberName_ "Layout") $ do
              state <- readTVarIO appState
              return (State.layout state)
          , autoMethod (memberName_ "Hello") sayHello
          ]
      }

  -- wait forever for calls
  forever (threadDelay 1000000)

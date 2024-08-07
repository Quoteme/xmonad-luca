-- vim: fdm=marker tabstop=2 shiftwidth=2 expandtab
--
-- Add custom settings for changing from/to laptop mode here
-- changing to laptop mode will show window decorations for example

module LaptopMode where

import Control.Monad (forever, when, void)
import Control.Monad.Primitive
import Data.List (isInfixOf)
import GHC.IO (unsafePerformIO)
import GHC.IO.Handle (hFlush, hGetLine)
import System.Process (CreateProcess (std_out), StdStream (CreatePipe), createProcess, readProcess, shell, runCommand)
import XMonad
import XMonad.Util.ExtensibleState qualified as XS
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.SpawnOnce (spawnOnce)
import Text.Format (format)

-- Save the current mode of the laptop for layouts to know
data LaptopMode = TabletMode | LaptopMode | CinemaMode
  deriving (Show, Eq)

instance ExtensionClass LaptopMode where
  initialValue = LaptopMode

getLaptopMode :: X LaptopMode
getLaptopMode = XS.get

setLaptopMode :: LaptopMode -> X ()
setLaptopMode = XS.put

toggleTabletMode :: X ()
toggleTabletMode = do
  mode <- getLaptopMode
  case mode of
    TabletMode -> setLaptopMode LaptopMode
    LaptopMode -> setLaptopMode TabletMode
    CinemaMode -> setLaptopMode CinemaMode

processCommand :: String -> X ()
processCommand line = do
  -- Process the line or perform any desired actions
  -- putStrLn $ "Received event: " ++ line
  when ("switch tablet-mode state 1" `isInfixOf` line) $ do
      spawnOnce "notify-send 'XMonad' 'Tablet mode activated'"
      io $ void $ runCommand "xmonadctl layout-tablet"
  when ("switch tablet-mode state 0" `isInfixOf` line) $ do
      spawnOnce "notify-send 'XMonad' 'Tablet mode deactivated'"
      io $ void $ runCommand "xmonadctl layout-normal"

tabletModeHook :: X ()
tabletModeHook = do
  -- use libinput to find the device id of `Asus WMI hotkeys`
  deviceID <- io $ readProcess "sh" ["-c", "libinput list-devices | grep 'Asus WMI hotkeys' -A 2 | grep -o '/dev/input/event[0-9]*'"] ""
  spawnOnce $ format "notify-send 'XMonad' 'Tablet mode detection started for device {}'" [deviceID]
  -- use `libinput debug-events --device deviceID` to listen for events
  -- specifically, call the above command and whenever a new line is printed, run the function `processCommand` with the line as an argument
  (_, Just stdoutHandle, _, processHandle) <- io $ createProcess (shell $ "libinput debug-events --device " ++ deviceID) { std_out = CreatePipe }
  -- Continuously read and process lines from the command output
  let loop = do
        line <- io $ hGetLine stdoutHandle
        processCommand line
        loop 
  loop

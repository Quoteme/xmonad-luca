-- vim: fdm=marker tabstop=2 shiftwidth=2 expandtab
--
-- Add custom settings for changing from/to laptop mode here
-- changing to laptop mode will show window decorations for example

module LaptopMode where

import Control.Monad (forever, when)
import Control.Monad.Primitive
import Data.List (isInfixOf)
import GHC.IO (unsafePerformIO)
import GHC.IO.Handle (hFlush, hGetLine)
import System.Process (CreateProcess (std_out), StdStream (CreatePipe), createProcess, readProcess, shell)
import XMonad
import XMonad.Util.ExtensibleState qualified as XS
import XMonad.Util.Run (spawnPipe)

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

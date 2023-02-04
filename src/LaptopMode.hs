-- vim: fdm=marker tabstop=2 shiftwidth=2 expandtab
--
-- Add custom settings for changing from/to laptop mode here
-- changing to laptop mode will show window decorations for example

module LaptopMode where

import XMonad
import qualified XMonad.Util.ExtensibleState as XS
import GHC.IO (unsafePerformIO)
import Control.Monad.Primitive

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

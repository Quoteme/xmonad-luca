-- vim: fdm=marker tabstop=2 shiftwidth=2 expandtab
--
-- This file adds functionality to xmonad which allows it to automatically
-- create thumbnails of workspaces. Using [xmonad-workspace-preview], we
-- can then show a menu to the user which allows them to switch to a
-- workspace by clicking on the thumbnail.

module Thumbnail where

import qualified XMonad.Layout.BoringWindows as BW
import qualified XMonad.StackSet as S
import qualified Data.Map        as M
import qualified XMonad.Util.ExtensibleState as XS
import XMonad
import Data.Time.Clock.POSIX
import System.Directory (getHomeDirectory, listDirectory, removeFile)
import GHC.OldList (isPrefixOf)
import Control.Monad


removeOldThumbnails :: IO ()
removeOldThumbnails = do
  home <- getHomeDirectory
  let thumbDir = home ++ "/.cache/"
  files <- listDirectory thumbDir
  let oldFiles = [f | f <- files, "xmonad_workspace_thumbnail" `isPrefixOf` f]
  mapM_ (\f -> removeFile (thumbDir ++ f)) oldFiles

newtype ThumbnailLastSave = ThumbnailLastSave POSIXTime
  deriving (Show, Eq)

instance ExtensionClass ThumbnailLastSave where
  initialValue = ThumbnailLastSave 0

newtype LastSavedWorkspaceThumbnail = LastSavedWorkspaceThumbnail String
  deriving (Show, Eq)

instance ExtensionClass LastSavedWorkspaceThumbnail where
  initialValue = LastSavedWorkspaceThumbnail "no workspace"

shouldSaveNewThumbnail :: POSIXTime -> X Bool
shouldSaveNewThumbnail delay = do
  -- Delay in seconds
  ThumbnailLastSave lastSaveTime <- XS.get
  LastSavedWorkspaceThumbnail lastSavedWorkspace <- XS.get
  currentWorkspace <- gets (S.tag . S.workspace . S.current . windowset)
  currentTime <- liftIO getPOSIXTime
  let shouldSave = currentTime - lastSaveTime > delay || currentWorkspace /= lastSavedWorkspace
  when shouldSave $ do
    XS.put $ ThumbnailLastSave currentTime
    XS.put $ LastSavedWorkspaceThumbnail currentWorkspace
  return shouldSave

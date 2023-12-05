module Layouts.TreeLayout
  ( TreeLayout (..),
    emptyTreeLayout,
  )
where

import Data.Bifunctor
import Data.Maybe
import Layouts.Helpers.Direction
import Layouts.Helpers.Involution
import Layouts.Helpers.Tree
import Text.Format
import XMonad
import XMonad.Layout.BinarySpacePartition (Rotate (Rotate), Swap (..))
import XMonad.StackSet

-- | Container to store all the extra-data for a node in our [TreeLayout].
data WindowNode = WindowNode
  { percentage :: Rational,
    window :: Window
  }
  deriving (Show, Read)

-- | We define two [WindowNode]s to be equal if their [window]s are equal.
-- |
-- | In practice, this means we would consider two [WindowNode Window]s to be
-- | equal if they store the same window.
instance Eq WindowNode where
  (==) :: WindowNode -> WindowNode -> Bool
  (==) (WindowNode _ v1) (WindowNode _ v2) = v1 == v2

data BranchNode = BranchNode
  { rotation :: Direction
  }
  deriving (Show, Read, Eq)

-- | A datatype to represent a tree layout of windows for XMonad.
data TreeLayout a = TreeLayout
  { tree :: Tree BranchNode WindowNode,
    defaultBranch :: BranchNode,
    currentPath :: Path,
    lastFocused :: Maybe Window
  }
  deriving (Show, Read, Eq)

instance LayoutClass TreeLayout Window where
  -- \| The name we give to our layout.
  description _ = "TreeLayout"

  -- \| This function will place the windows on the screen (some given [rect]).
  doLayout (TreeLayout tree defaultBranch currentPath lastFocused) rect stack = do
    -- get the current desktop dimensions
    let Rectangle sx sy sw sh = rect
    -- get the currently focused window
    focused <- gets (peek . windowset) :: X (Maybe Window)
    -- if the focused window has changed from the last time, we want to recalculate the currentPath
    -- \| To be able to search for our focused window, we first need to convert our
    -- \| [Tree BranchNode WindowNode] to a [Tree BranchNode Window].
    -- \| This is, because we search a [Window] and not a [WindowNode].
    -- \| We can do that by using the [bimap] (or here the [second = bimap id]) function.
    let windowTree = second window tree
    let newpath = if isJust focused then fromMaybe [] $ find (fromJust focused) windowTree else []
    -- let newpath' = if focused /= lastFocused then currentPath else newpath'
    let newpath' = if focused /= lastFocused then currentPath else newpath
    -- get all the windows in the stack
    let windows = XMonad.StackSet.integrate stack
    -- wrap each window in a [WindowNode]
    let windowNodes = map (WindowNode 1) windows
    -- add all new windows to the tree
    let tree' = updateLeafs (fromMaybe defaultBranch) [] tree windowNodes
    -- define the new tree
    let tree'' = fromMaybe tree tree'
    -- calculate the new positions of the windows
    let rects = layoutRects tree'' rect
    -- write the tree to a file for debugging
    -- return $! (rects, Just (TreeLayout tree'' defaultBranch newpath' focused))
    return (rects, Just (TreeLayout tree'' defaultBranch currentPath focused))

  -- \| This function is used to modify the layout using the keyboard.
  -- \| More generally, we actually modify the layout using "messages".
  -- \| Most of the time, a message is send by pressing a key combination though, like so:
  -- \|
  -- \| > ((modMask, xK_r), sendMessage $ Rotate)
  handleMessage (TreeLayout tree defaultBranch currentPath lastFocused) someMessage
    | Just Rotate <- fromMessage someMessage = do
        -- \| 1. Rotate the branch at the given path
        -- \| We define a helper function [rotatetree]
        -- \| to rotate the branches counter-clockwise
        let tree' = apply [] rotatetree tree
        -- \| 2. Return the new layout
        return $ Just $ TreeLayout (fromMaybe tree tree') defaultBranch currentPath lastFocused
    | Just Swap <- fromMessage someMessage = do
        -- \| 1. Swap the branch at the given path
        xmessage $ format "path: {0}" [show currentPath]
        -- let tree' = apply currentPath swap tree
        -- \| 2. Return the new layout
        return $ Just $ TreeLayout tree defaultBranch currentPath lastFocused
    | otherwise = return Nothing
    where
      -- \| helper functions
      -- \|
      -- \| Rotate a tree counter clockwise
      rotatetree :: Tree BranchNode WindowNode -> Tree BranchNode WindowNode
      rotatetree t = first (\w -> BranchNode (rotateCCW $ rotation w)) t

-- | This is the starting point of our layout.
-- | You want to include this in your [layoutHook].
emptyTreeLayout :: TreeLayout Window
emptyTreeLayout =
  TreeLayout
    { tree = emptyTree,
      defaultBranch = BranchNode RIGHT,
      currentPath = [],
      lastFocused = Nothing
    }
  where
    emptyTree = Branch (BranchNode RIGHT) []

-- | Calculate the positions of all the windows using the given [TreeLayout].
-- | The algorithm used to calculate the positions is pretty simple:
-- |
-- | 1. We are always given a [Rectangle] to work with.
-- |    This rectangle represents the dimensions the area we want to fill with
-- |    our windows.
-- | 2. We are also given a [Tree] of [WindowNode]s.
-- |    This tree represents the windows we want to place in the given [Rectangle].
-- |
-- | If we are given a [Leaf] we simply place the window in the given [Rectangle]
-- | and fill the entire area with it.
-- |
-- | If we are given a [Branch], we remember that it just consists of a list of
-- | sub[Tree]s and some [BranchNode] data. We use the [BranchNode] data to
-- | determine the direction of the split and then we split the [Rectangle] in
-- | parts in that direction. We then recursively call [layoutRects] on each
-- | sub[Tree] and each part of the [Rectangle] and combine the results.
layoutRects :: Tree BranchNode WindowNode -> Rectangle -> [(Window, Rectangle)]
layoutRects (Leaf wn) (Rectangle sx sy sw sh) = [(window wn, Rectangle sx sy sw sh)]
layoutRects (Branch (BranchNode RIGHT) bs) r@(Rectangle sx sy sw sh) = result
  where
    -- \| We need to calculate some numbers before we can construct the
    -- \| rectangle/window pairing
    -- \|
    -- \| 1. Number of subtrees in the branch
    n = length bs
    -- \| 2. The width given for each subtree
    width :: Rational
    width = fromIntegral sw / fromIntegral n
    -- \| 3. An x-offset for each subtree
    delta :: Int -> Position
    delta i = fromIntegral i * floor width
    -- \| 4. Define a helper function that gives us the i-th rectangle
    ithRect :: Rectangle -> Int -> Rectangle
    ithRect (Rectangle x' y' w' h') i = Rectangle (x' + delta i) y' (fromIntegral $ floor width) h'
    -- \| 5. Apply [layoutRects] to each subtree
    subRects = [layoutRects subtree (ithRect r i) | (i, subtree) <- zip [0 ..] bs]
    -- \| 7. Lastly, we only need to flatten the list of lists of lists ... of rectangles
    -- \| into a list of rectangles
    result :: [(Window, Rectangle)]
    result = concat subRects
-- FIXME: This currently causes XMonad to crash
layoutRects b@(Branch (BranchNode LEFT) _) r = layoutRects (Branch (BranchNode RIGHT) (trunk $ involution b)) r
layoutRects (Branch (BranchNode UP) bs) r@(Rectangle sx sy sw sh) = result
  where
    -- \| Just like in the [RIGHT] case, we need to do the same thing here,
    -- \| but now we need to split the [Rectangle] vertically instead of
    -- \| horizontally.
    n = length bs
    height :: Rational
    height = fromIntegral sh / fromIntegral n
    delta :: Int -> Position
    delta i = fromIntegral i * floor height
    ithRect :: Rectangle -> Int -> Rectangle
    ithRect (Rectangle x' y' w' h') i = Rectangle x' (y' + delta i) w' (fromIntegral $ floor height)
    subRects = [layoutRects subtree (ithRect r i) | (i, subtree) <- zip [0 ..] bs]
    result :: [(Window, Rectangle)]
    result = concat subRects
layoutRects b@(Branch (BranchNode DOWN) _) r = layoutRects (Branch (BranchNode UP) (trunk $ involution b)) r

-- FIXME: Implement FRONT and BACK for stacked windows
module Layouts.TreeLayout
  ( TreeLayout (..),
    emptyTreeLayout,
  )
where

import Data.Maybe
import Layouts.Helpers.Tree
import XMonad
import XMonad.StackSet

-- | A datatype to represent the direction of a split in a [Node] of our [TreeLayout
-- |
-- | Example:
-- |
-- | Right:
-- | [window1] [window2] [window3] [window4]
-- |
-- | Left:
-- | [window4] [window3] [window2] [window1]
-- |
-- | Up:
-- | [window4]
-- | [window3]
-- | [window2]
-- | [window1]
-- |
-- | Down:
-- | [window1]
-- | [window2]
-- | [window3]
-- | [window4]
data Axis = RIGHT | LEFT | UP | DOWN
  deriving (Show, Read, Eq)

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
  { rotation :: Axis
  }
  deriving (Show, Read, Eq)

-- | A datatype to represent a tree layout of windows for XMonad.
data TreeLayout a = TreeLayout
  { tree :: Tree BranchNode WindowNode,
    defaultBranch :: BranchNode
  }
  deriving (Show, Read, Eq)

instance LayoutClass TreeLayout Window where
  -- \| The name we give to our layout.
  description _ = "TreeLayout"

  -- \| This function will place the windows on the screen (some given [rect]).
  doLayout (TreeLayout tree defaultBranch) rect stack = do
    -- get the current desktop dimensions
    let Rectangle sx sy sw sh = rect
    -- get the currently focused window
    let focused = XMonad.StackSet.focus stack
    -- get all the windows in the stack
    let windows = XMonad.StackSet.integrate stack
    -- wrap each window in a [WindowNode]
    let windowNodes = map (WindowNode 1) windows
    -- add all new windows to the tree
    let tree' = updateLeafs (fromMaybe defaultBranch) [] tree windowNodes
    -- write the tree to a file
    liftIO $ writeFile "/tmp/.xmonad-tree.txt" (show windowNodes ++ "\n" ++ show tree ++ "\n" ++ show tree')
    -- define the new tree
    let tree'' = fromMaybe tree tree'
    -- calculate the new positions of the windows
    let rects = layoutRects tree'' rect
    return (l, Just (TreeLayout tree'' defaultBranch))
    where
      l = [(w, rect) | w <- [XMonad.StackSet.focus stack] ++ up stack ++ down stack]

-- | This is the starting point of our layout.
-- | You want to include this in your [layoutHook].
emptyTreeLayout :: TreeLayout Window
emptyTreeLayout =
  TreeLayout
    { tree = emptyTree
    }
  where
    emptyTree = Branch (BranchNode RIGHT) []

-- | Calculate the positions of all the windows using the given [TreeLayout].
layoutRects :: Tree BranchNode WindowNode -> Rectangle -> [(Window, Rectangle)]
layoutRects (Leaf wn) (Rectangle sx sy sw sh) = [(window wn, Rectangle sx sy sw sh)]
layoutRects (Branch a bs) (Rectangle sx sy sw sh) = undefined
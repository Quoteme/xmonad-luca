module Layouts.TreeLayout
  ( TreeLayout (..),
    emptyTreeLayout,
  )
where

import Data.Graph
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
  deriving (Show, Read)

-- | Container to store all the extra-data for a node in our [TreeLayout].
data WindowNode a = WindowNode
  { width :: Int,
    height :: Int,
    rotation :: Axis,
    value :: a
  }
  deriving (Show, Read)

-- | A datatype to represent a tree layout of windows for XMonad.
data TreeLayout a = TreeLayout
  { tree :: Tree (WindowNode a)
  }
  deriving (Show, Read)

instance LayoutClass TreeLayout Window where
  -- \| The name we give to our layout.
  description _ = "TreeLayout"

  -- \| This function will place the windows on the screen (some given [rect]).
  pureLayout (TreeLayout tree) rect stack = [(w, rect) | w <- [XMonad.StackSet.focus stack] ++ up stack ++ down stack]
    where
      a = 1

-- | This is the starting point of our layout.
-- | You want to include this in your [layoutHook].
emptyTreeLayout :: TreeLayout Window
emptyTreeLayout =
  TreeLayout
    { tree = emptyTree
    }
  where
    emptyTree = Node (WindowNode 0 0 RIGHT 0) []
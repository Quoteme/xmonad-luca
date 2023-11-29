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
  deriving (Show, Read, Eq)

-- | Container to store all the extra-data for a node in our [TreeLayout].
data WindowNode a = WindowNode
  { width :: Int,
    height :: Int,
    rotation :: Axis,
    value :: a
  }
  deriving (Show, Read)

-- | We define two [WindowNode]s to be equal if their [value]s are equal.
-- |
-- | In practice, this means we would consider two [WindowNode Window]s to be
-- | equal if they store the same window.
-- |
-- | We consider two [WindowNode (Tree Window)]s to be equal if they store the same
instance Eq a => Eq (WindowNode a) where
  (==) :: WindowNode a -> WindowNode a -> Bool
  (==) (WindowNode _ _ _ v1) (WindowNode _ _ _ v2) = v1 == v2

instance Functor WindowNode where
  fmap :: (a -> b) -> WindowNode a -> WindowNode b
  fmap f (WindowNode w h r v) = WindowNode w h r (f v)

instance Foldable WindowNode where
  foldr :: (a -> b -> b) -> b -> WindowNode a -> b
  foldr f z (WindowNode _ _ _ v) = f v z

-- | A datatype to represent a tree layout of windows for XMonad.
newtype TreeLayout a = TreeLayout
  { tree :: Tree (WindowNode a)
  }
  deriving (Show, Read, Eq)

-- | Functor instance for our [TreeLayout].
instance Functor TreeLayout where
  fmap :: (a -> b) -> TreeLayout a -> TreeLayout b
  fmap f (TreeLayout tree) = TreeLayout $ fmap (fmap f) tree

instance Foldable TreeLayout where
  foldr :: (a -> b -> b) -> b -> TreeLayout a -> b
  foldr f z (TreeLayout tree) = foldr (flip (foldr f)) z tree

instance LayoutClass TreeLayout Window where
  -- \| The name we give to our layout.
  description _ = "TreeLayout"

  -- \| This function will place the windows on the screen (some given [rect]).
  doLayout (TreeLayout tree) rect stack = do
    return (l, Just (TreeLayout tree))
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
    emptyTree = Node (WindowNode 0 0 RIGHT 0) []

-- | Check if an element is in our TreeLayout.
check :: Eq a => a -> TreeLayout a -> Bool
check x (TreeLayout tree) = WindowNode x `elem` tree